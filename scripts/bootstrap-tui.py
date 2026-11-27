#!/usr/bin/env python3
"""Shows how far a run of scripts/bootstrap.py has come, and how long it
has left.

usage: bootstrap-tui.py [--out DIR] [--once] [--interval SECONDS]

It only reads what the build writes to DIR (build/bootstrap), so it can be
started, quit and started again while the build runs:

- results.tsv: when each package started, and how it ended;
- run.json: the process and the number of jobs of the build;
- logs/NAME.log: what AIHC says, from which the current step is taken;
- logs/NAME.events.jsonl: the progress events of an AIHC that writes them
  (see below), from which the modules done are taken.

How long a package takes is known from bootstrap/timings.tsv, a run of the
same plan: the interpreter does the same work at the same speed each time.
The time left is the longer of two estimates: the longest chain of packages
that wait for each other, and the work left shared among the jobs.

The events are one JSON object a line, as AIHC_PROGRESS_EVENTS asks for:
    {"time": 1764150000.5, "event": "build", "item": "aihc-0.3.0", "modules": 141}
    {"time": 1764150100.0, "event": "modules", "item": "aihc-0.3.0", "modules": 3}
"build" gives an item's module count, "modules" how many more of them are
compiled. Other events ("prepare", "compile", "link", "done", ...) name the
step the item is at.

--once prints the state once, as text, instead of drawing it until q."""
import curses
import json
import os
import sys
import time

here = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
# These three are built with the first package that needs them.
WITH_THE_FIRST = ('aihc-rts', 'aihc-prim', 'aihc-base')
# For a package that bootstrap/timings.tsv does not know.
UNKNOWN_SECONDS = 60.0


def option(name, default):
    if name in sys.argv:
        return sys.argv[sys.argv.index(name) + 1]
    return default


def read_plan():
    """The packages that the build runs, each with the packages it waits
    for, as scripts/bootstrap.py runs them."""
    needs = {}
    for line in open(here + '/bootstrap/plan.tsv'):
        row = line.rstrip('\n').split('\t')
        name = 'aihc-stage1' if row[0] == 'aihc' else row[0]
        if name in WITH_THE_FIRST:
            continue
        deps = ['aihc-stage1' if d == 'aihc' else d for d in row[6].split(',') if d]
        needs[name] = [d for d in deps if d not in WITH_THE_FIRST]
    for name in needs:
        if name != 'aihc-internal' and 'aihc-internal' not in needs[name]:
            needs[name].append('aihc-internal')
    return needs


def read_timings():
    timings = {}
    path = here + '/bootstrap/timings.tsv'
    if os.path.exists(path):
        for line in open(path):
            if line.startswith('#') or not line.strip():
                continue
            name, wall, _ = line.rstrip('\n').split('\t')
            timings[name] = float(wall)
    return timings


def read_results(out):
    """The last thing results.tsv says about each package: ('started',
    time) or (state, seconds it took)."""
    results = {}
    path = out + '/results.tsv'
    if os.path.exists(path):
        for line in open(path):
            fields = line.rstrip('\n').split('\t')
            if len(fields) < 2:
                continue
            if fields[1] == 'started':
                results[fields[0]] = ('started', float(fields[2]))
            else:
                took = float(fields[2].split()[0]) if len(fields) > 2 and fields[2] else 0.0
                results[fields[0]] = (fields[1], took)
    return results


def read_run(out):
    """The build's run.json, and whether its process is still there."""
    try:
        with open(out + '/run.json') as file:
            run = json.load(file)
    except (OSError, ValueError):
        return None, False
    try:
        os.kill(run['pid'], 0)
        alive = True
    except (OSError, KeyError):
        alive = False
    return run, alive


def current_step(out, name):
    """What the package is doing, and its modules done and in all, from its
    events if AIHC writes them, else from the last step in its log."""
    step, done, total = '', 0, 0
    events = out + '/logs/' + name + '.events.jsonl'
    if os.path.exists(events):
        counts = {}
        for line in open(events, errors='replace'):
            try:
                event = json.loads(line)
            except ValueError:
                continue  # a line still being written
            kind, item = event.get('event', ''), event.get('item', '')
            if kind == 'build':
                counts.setdefault(item, [0, 0])[1] = event.get('modules', 0)
            elif kind == 'modules':
                counts.setdefault(item, [0, 0])[0] += event.get('modules', 0)
            if kind not in ('modules', 'log', 'workers'):
                step = (kind + ' ' + item).strip()
        done = sum(c[0] for c in counts.values())
        total = sum(c[1] for c in counts.values())
    log = out + '/logs/' + name + '.log'
    if not step and os.path.exists(log):
        for line in open(log, errors='replace'):
            words = line.split(None, 1)
            if words and words[0] in ('store', 'build', 'built', 'link', 'install'):
                step = ' '.join(line.split())
    return step, done, total


def last_line(path):
    lines = [line.rstrip() for line in open(path, errors='replace') if line.strip()] if os.path.exists(path) else []
    lines = [line for line in lines if not line.split(None, 1)[0] in ('real', 'user', 'sys')]
    return lines[-1] if lines else ''


def duration(seconds):
    seconds = int(max(0, seconds))
    hours, rest = divmod(seconds, 3600)
    return '%d:%02d:%02d' % (hours, rest // 60, rest % 60) if hours else '%d:%02d' % (rest // 60, rest % 60)


def bar(fraction, width):
    filled = int(round(max(0.0, min(1.0, fraction)) * width))
    return '[' + '#' * filled + '.' * (width - filled) + ']'


def snapshot(out, needs, timings, now):
    """The state of the build, as lines of text."""
    results = read_results(out)
    run, alive = read_run(out)
    jobs = run['jobs'] if run else 1
    expected = {name: timings.get(name, UNKNOWN_SECONDS) for name in needs}

    state, elapsed, steps, left = {}, {}, {}, {}
    for name in needs:
        kind, value = results.get(name, ('waiting', 0.0))
        if kind == 'started':
            kind = 'running' if alive else 'stopped'
            elapsed[name] = now - value
            steps[name] = current_step(out, name)
        state[name] = kind
        if kind in ('done', 'skipped'):
            left[name] = 0.0
        elif kind == 'running':
            _, done, modules = steps[name]
            if modules:
                left[name] = expected[name] * (1 - done / modules)
            else:
                left[name] = expected[name] - elapsed[name]
            # A package that takes longer than it did is nearly done.
            left[name] = max(left[name], 0.05 * expected[name])
        else:
            left[name] = expected[name]

    # When each package can end, if every package starts once those it
    # waits for have ended: the longest chain still ahead.
    ends = {}

    def end(name):
        if name not in ends:
            if state[name] in ('done', 'skipped'):
                ends[name] = 0.0
            else:
                before = max([end(d) for d in needs[name] if d in needs] or [0.0])
                ends[name] = (0.0 if state[name] == 'running' else before) + left[name]
        return ends[name]

    chain = max([end(name) for name in needs] or [0.0])
    work = sum(left.values()) / max(1, jobs)
    remaining = 0.0 if all(state[n] in ('done', 'skipped') for n in needs) else max(chain, work)

    total = sum(expected.values())
    done_seconds = sum(expected[n] - left[n] for n in needs)
    counts = {}
    for name in needs:
        counts[state[name]] = counts.get(state[name], 0) + 1

    if counts.get('done', 0) == len(needs):
        status = 'finished: the compiler is ' + out + '/aihc'
    elif alive:
        status = 'running for %s, about %s left' % (duration(now - run['started']), duration(remaining))
    elif not results:
        status = 'no build has started in ' + out
    else:
        status = 'not running (started again, it goes on after the packages that are done)'
    lines = ['AIHC bootstrap   ' + status, '',
             '%s %3.0f%%   %d of %d packages done' % (bar(done_seconds / total if total else 1, 40),
                                                     100 * done_seconds / total if total else 100,
                                                     counts.get('done', 0), len(needs))]
    if counts.get('failed') or counts.get('skipped'):
        lines.append('%d failed, %d skipped for want of them' % (counts.get('failed', 0), counts.get('skipped', 0)))

    active = sorted((n for n in needs if state[n] in ('running', 'stopped')), key=lambda n: -elapsed[n])
    if active:
        lines += ['', 'Building' if alive else 'Stopped while building']
        for name in active:
            step, done, modules = steps[name]
            if modules:
                progress = '%s %d/%d modules' % (bar(done / modules, 20), done, modules)
            else:
                progress = '%s %s of ~%s' % (bar(elapsed[name] / expected[name], 20),
                                            duration(elapsed[name]), duration(expected[name]))
            lines.append('  %-26s %s  %s' % (name, progress, step))

    ready = [n for n in needs if state[n] == 'waiting' and all(state.get(d) == 'done' for d in needs[n] if d in needs)]
    waiting = [n for n in needs if state[n] == 'waiting']
    if waiting:
        # The packages on the longest chain ahead, in the order they build.
        path, name = [], max(waiting, key=end)
        while name is not None:
            path.append(name)
            before = [d for d in needs[name] if d in needs and state[d] not in ('done', 'skipped')]
            name = max(before, key=end) if before else None
        lines += ['', 'Longest chain ahead: ' + ' <- '.join(path)]
        if ready:
            lines.append('Ready to start: ' + ', '.join(sorted(ready)))

    failed = [n for n in needs if state[n] == 'failed']
    for name in failed:
        lines += ['', 'FAILED %s, see %s' % (name, out + '/logs/' + name + '.log'),
                  '  ' + last_line(out + '/logs/' + name + '.log')]

    finished = sorted((n for n in needs if state[n] == 'done'), key=lambda n: -expected[n])
    if finished:
        lines += ['', 'Done, longest first: ' + ', '.join('%s %s' % (n, duration(results[n][1])) for n in finished[:6])]
    return lines


def draw(screen, out, needs, timings, interval):
    curses.curs_set(0)
    screen.timeout(int(interval * 1000))
    while True:
        lines = snapshot(out, needs, timings, time.time()) + ['', 'q quits']
        screen.erase()
        height, width = screen.getmaxyx()
        for row, line in enumerate(lines[:height - 1]):
            screen.addnstr(row, 0, line, width - 1, curses.A_BOLD if row == 0 else 0)
        screen.refresh()
        if screen.getch() in (ord('q'), ord('Q')):
            return


def main():
    out = os.path.abspath(option('--out', here + '/build/bootstrap'))
    needs, timings = read_plan(), read_timings()
    if '--once' in sys.argv or not sys.stdout.isatty():
        print('\n'.join(snapshot(out, needs, timings, time.time())))
    else:
        try:
            curses.wrapper(draw, out, needs, timings, float(option('--interval', '1')))
        except KeyboardInterrupt:
            pass


main()
