-- | Daemon-side lifecycle entry points.
--
-- The bespoke 'Data.Binary' request/response loop that used to live
-- here is gone — the daemon now speaks Cap'n Proto exclusively via
-- 'Corvus.Rpc.Server.runCapnpServer'. What remains is the
-- start-of-day and end-of-day glue: cleaning up stale persistent
-- state at boot and gracefully stopping VMs / networks at shutdown.
module Corvus.Server
  ( handleStartup
  , handleGracefulShutdown
  )
where

import Control.Monad (void)
import Corvus.Action (runAction)
import Corvus.Handlers.Lifecycle (GracefulShutdown (..), Startup (..))
import Corvus.Types

--------------------------------------------------------------------------------
-- Startup Handler
--------------------------------------------------------------------------------

-- | Run startup tasks: clean stale state, start namespace, autostart networks and VMs.
-- Delegates to the Startup action which records itself as a task with subtasks.
handleStartup :: ServerState -> Int -> IO ()
handleStartup state retentionDays = void $ runAction state (Startup retentionDays)

--------------------------------------------------------------------------------
-- Graceful Shutdown Handler
--------------------------------------------------------------------------------

-- | Gracefully shut down all running VMs and networks.
-- Delegates to the GracefulShutdown action which records itself as a task.
handleGracefulShutdown :: ServerState -> IO ()
handleGracefulShutdown state = void $ runAction state GracefulShutdown
