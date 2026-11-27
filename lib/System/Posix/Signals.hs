module System.Posix.Signals where

-- AIHC asks to turn SIGTERM into an exception. The interpreter installs no
-- handler: a SIGTERM stops it at once, which only matters to a run that
-- was being stopped anyway.

type Signal = Int

data SignalSet = SignalSet

data Handler = Default | Ignore | Catch (IO ()) | CatchOnce (IO ())

sigTERM :: Signal
sigTERM = 15

sigINT :: Signal
sigINT = 2

installHandler :: Signal -> Handler -> Maybe SignalSet -> IO Handler
installHandler _ _ _ = pure Default
