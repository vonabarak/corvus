-- | Request handling module.
-- Re-exports handlers from submodules and provides the main dispatch function.
module Corvus.Handlers
  ( -- * Request handling
    handleRequest,

    -- * Re-exports from submodules
    module Corvus.Handlers.Core,
    module Corvus.Handlers.Vm,
  )
where

import Corvus.Handlers.Core
import Corvus.Handlers.Vm
import Corvus.Protocol
import Corvus.Types

--------------------------------------------------------------------------------
-- Request Dispatch
--------------------------------------------------------------------------------

-- | Handle a request and produce a response
handleRequest :: ServerState -> Request -> IO Response
handleRequest state req = case req of
  -- Core handlers
  ReqPing -> handlePing
  ReqStatus -> handleStatus state
  ReqShutdown -> handleShutdown state
  -- VM handlers
  ReqListVms -> handleVmList state
  ReqShowVm vmId -> handleVmShow state vmId
  ReqVmStart vmId -> handleVmStart state vmId
  ReqVmStop vmId -> handleVmStop state vmId
  ReqVmPause vmId -> handleVmPause state vmId
  ReqVmReset vmId -> handleVmReset state vmId
