#!/usr/bin/env python3
"""Builds AIHC with AIHC in the interpreter: every package of
bootstrap/plan.tsv, as many at once as their dependencies allow, and last
the aihc library with its executable.

usage: bootstrap.py [--jobs N] [--out DIR] [--runner COMMAND]

A line of the plan: name, version, kind, where, the SHA-256 of the
package's archive, the SHA-256 of its .cabal file, and the packages it
needs. The kind is core (one of AIHC's own core libraries), hackage (where
is the revision of the .cabal file) or local (where is a directory of this
repository). The plan is AIHC's own, with the hackage and pretty-ui flags
off: aihc plan bin/aihc --executable aihc.

The interpreted AIHC does not talk to Hackage. So the sources are fetched
here, checked against the plan, and put in one directory, the workspace,
which AIHC is told to take its packages from.

A .hsc file goes through hsc2hs. That is hsc2hs from Hackage, run in the
interpreter as well (scripts/hsc2hs.sh): its sources are first put through
the C preprocessor, which the interpreter does not have. AIHC_HSC2HS can
name another hsc2hs instead. No other source
preprocessor is needed, and none is used, so that no program that GHC
built takes part.

Everything goes to DIR (build/bootstrap): the workspace, the store of built
packages, the logs, results.tsv with when each package started and how long
it took, and the compiler, DIR/aihc. scripts/bootstrap-tui.py shows the
progress of a run from these files. A run that is started again goes on after the
packages that are done. The runner (sh scripts/run-aihc.sh) is the command
that AIHC's arguments are given to."""
import hashlib
import json
import os
import shlex
import shutil
import subprocess
import sys
import tarfile
import time
import urllib.request

HACKAGE = 'https://hackage.haskell.org/package/'
CONSTRAINTS = ['--constraint', 'aihc -hackage', '--constraint', 'aihc -pretty-ui']
# hsc2hs from Hackage: the release, and the SHA-256 of its archive.
HSC2HS = ('hsc2hs-0.68.10', '6f4e34d788fe2ca7091ee0a10307ee8a7c060a1ba890f2bffad16a7d4d5cef76')
# What the C preprocessor is told about the platform when it reads hsc2hs.
# Its conditions only ask for a recent base and process, and for Windows.
HSC2HS_CPP = ['-E', '-P', '-w', '-undef', '-traditional', '-x', 'c',
              '-DMIN_VERSION_base(a,b,c)=1', '-DMIN_VERSION_process(a,b,c)=1',
              '-D__GLASGOW_HASKELL__=912',
              '-D%s_HOST_OS' % ('darwin' if sys.platform == 'darwin' else 'linux')]
# These three are built with the first package that needs them.
WITH_THE_FIRST = ('aihc-rts', 'aihc-prim', 'aihc-base')


def option(name, default):
    if name in sys.argv:
        return sys.argv[sys.argv.index(name) + 1]
    return default


here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
out = os.path.abspath(option('--out', here + '/build/bootstrap'))
jobs = int(option('--jobs', os.cpu_count() or 1))
runner = shlex.split(option('--runner', 'sh scripts/run-aihc.sh'))
workspace = out + '/workspace'
store = out + '/store'

rows = [line.rstrip('\n').split('\t') for line in open(here + '/bootstrap/plan.tsv')]
info = {row[0]: row for row in rows}
needs = {row[0]: [d for d in row[6].split(',') if d] for row in rows}


def fetch(url, sha256):
    request = urllib.request.Request(url, headers={'User-Agent': 'aihc-bootstrap'})
    data = urllib.request.urlopen(request).read()
    if hashlib.sha256(data).hexdigest() != sha256:
        sys.exit('%s is not what the plan says it is' % url)
    return data


def prepare_workspace():
    os.makedirs(workspace, exist_ok=True)
    os.makedirs(out + '/downloads', exist_ok=True)
    for name, version, kind, where, archive_hash, cabal_hash, _ in rows:
        directory = workspace + '/' + name
        if kind == 'local' and not os.path.islink(directory):
            os.symlink(here + '/' + where, directory)
        if kind != 'hackage' or os.path.isdir(directory):
            continue
        release = name + '-' + version
        print('fetching ' + release, flush=True)
        archive = out + '/downloads/' + release + '.tar.gz'
        with open(archive, 'wb') as file:
            file.write(fetch(HACKAGE + release + '/' + release + '.tar.gz', archive_hash))
        cabal = fetch(HACKAGE + release + '/revision/' + where + '.cabal', cabal_hash)
        unpacked = out + '/downloads/' + release
        shutil.rmtree(unpacked, ignore_errors=True)
        with tarfile.open(archive) as tar:
            for member in tar.getmembers():
                if not (member.name + '/').startswith(release + '/') or '..' in member.name.split('/'):
                    sys.exit('%s has a file outside %s' % (archive, release))
            tar.extractall(out + '/downloads')
        with open(unpacked + '/' + name + '.cabal', 'wb') as file:
            file.write(cabal)
        os.rename(unpacked, directory)


def prepare_hsc2hs():
    release, sha256 = HSC2HS
    directory = out + '/tools/' + release
    if os.path.isdir(directory + '/pp'):
        return directory
    print('fetching ' + release, flush=True)
    data = fetch(HACKAGE + release + '/' + release + '.tar.gz', sha256)
    shutil.rmtree(out + '/tools', ignore_errors=True)
    os.makedirs(out + '/tools')
    with open(out + '/tools/' + release + '.tar.gz', 'wb') as file:
        file.write(data)
    with tarfile.open(out + '/tools/' + release + '.tar.gz') as tar:
        for member in tar.getmembers():
            if not (member.name + '/').startswith(release + '/') or '..' in member.name.split('/'):
                sys.exit('%s has a file outside %s' % (release, release))
        tar.extractall(out + '/tools')
    for root, _, files in os.walk(directory + '/src'):
        for file in files:
            if file.endswith('.hs'):
                source = os.path.join(root, file)
                target = directory + '/pp' + source[len(directory + '/src'):]
                os.makedirs(os.path.dirname(target), exist_ok=True)
                subprocess.run(['cc'] + HSC2HS_CPP + [source, '-o', target], check=True)
    return directory


def command(name):
    _, _, kind, where, _, _, _ = info[name]
    if name == 'aihc-stage1':
        # The executable is built from its main module: the aihc library
        # is then built with it, and all it needs is taken from the store.
        # (Installing the library wants what its tests need as well.)
        arguments = ['build', here + '/vendor/aihc/bin/aihc/exe/Main.hs', '-p', 'aihc',
                     '-o', out + '/aihc', '--print-timings'] + CONSTRAINTS
    elif kind == 'core':
        arguments = ['install', here + '/vendor/aihc/core-libs/' + name]
    elif kind == 'hackage':
        arguments = ['install', workspace + '/' + name, '--immutable']
    else:
        arguments = ['install', here + '/' + where, '--immutable'] + CONSTRAINTS
    return ['/usr/bin/time', '-p'] + runner + arguments + [
        '--workspace', workspace, '--store', store,
        '--build-root', out + '/target-' + name, '-O', '0']


def build():
    info['aihc-stage1'] = info.pop('aihc')
    needs['aihc-stage1'] = needs.pop('aihc')
    state = {name: 'done' for name in WITH_THE_FIRST}
    todo = [name for name in info if name not in state]
    for name in todo:
        # Nothing starts beside the package that builds the first three.
        if name != 'aihc-internal' and 'aihc-internal' not in needs[name]:
            needs[name].append('aihc-internal')
    if os.path.exists(out + '/results.tsv'):
        for line in open(out + '/results.tsv'):
            fields = line.rstrip('\n').split('\t')
            if len(fields) > 1 and fields[1] == 'done':
                state[fields[0]] = 'done'
    os.makedirs(out + '/logs', exist_ok=True)
    results = open(out + '/results.tsv', 'a')
    # Which process runs the build, so that scripts/bootstrap-tui.py can
    # tell a running build from one that stopped.
    with open(out + '/run.json', 'w') as file:
        json.dump({'pid': os.getpid(), 'jobs': jobs, 'started': time.time()}, file)
    missing = here + '/scripts/missing-tool.sh'
    environment = dict(os.environ, AIHC_COMPILER_IDENTITY='bootstrap',
                       AIHC_ALEX=missing, AIHC_HAPPY=missing, AIHC_C2HS=missing)
    # An hsc2hs that AIHC_HSC2HS names is used instead of the interpreted one.
    if not environment.get('AIHC_HSC2HS'):
        environment['AIHC_HSC2HS'] = here + '/scripts/hsc2hs.sh'
        environment['HSC2HS_SOURCES'] = prepare_hsc2hs()
    running = {}
    start = time.time()

    def record(name, fields):
        results.write('\t'.join([name, state[name]] + fields) + '\n')
        results.flush()
        print('%6.0f s  %-7s %-28s %s' % (time.time() - start, state[name], name,
                                         ' '.join(fields)), flush=True)

    def finish(name, process, began, log):
        log.close()
        cpu = '?'
        for line in open(out + '/logs/' + name + '.log', errors='replace'):
            fields = line.split()
            if len(fields) == 2 and fields[0] == 'user':
                cpu = fields[1]
        state[name] = 'done' if process.returncode == 0 else 'failed'
        record(name, ['%.0f s' % (time.time() - began), cpu + ' s cpu'])

    while True:
        for name in list(running):
            process, began, log = running[name]
            if process.poll() is not None:
                del running[name]
                finish(name, process, began, log)
        for name in todo:
            if name in state or name in running or len(running) >= jobs:
                continue
            if any(state.get(d) in ('failed', 'skipped') for d in needs[name]):
                state[name] = 'skipped'
                record(name, [])
            elif all(state.get(d) == 'done' for d in needs[name]):
                log = open(out + '/logs/' + name + '.log', 'w')
                # An AIHC that knows the variable writes its progress events
                # there, one JSON object a line (scripts/bootstrap-tui.py).
                events = out + '/logs/' + name + '.events.jsonl'
                if os.path.exists(events):
                    os.remove(events)
                began = time.time()
                results.write('%s\tstarted\t%.0f\n' % (name, began))
                results.flush()
                running[name] = (subprocess.Popen(command(name), cwd=here,
                                                  env=dict(environment, AIHC_PROGRESS_EVENTS=events),
                                                  stdout=log, stderr=subprocess.STDOUT),
                                 began, log)
        if not running and all(name in state for name in todo):
            break
        time.sleep(1)
    count = {s: sum(1 for name in todo if state[name] == s)
             for s in ('done', 'failed', 'skipped')}
    print('%.0f s: %d done, %d failed, %d skipped' % (
        time.time() - start, count['done'], count['failed'], count['skipped']))
    if count['failed'] or count['skipped']:
        sys.exit('the logs are in ' + out + '/logs')
    print('the compiler is ' + out + '/aihc')


prepare_workspace()
build()
