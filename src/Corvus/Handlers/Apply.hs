-- | Compatibility façade for apply actions and entry points.
module Corvus.Handlers.Apply
  ( ApplyAction (..)
  , ApplyDiskCreate (..)
  , ApplyVmCreate (..)
  , handleApplyValidate
  , executeApply
  ) where

import Corvus.Handlers.Apply.Disk (ApplyDiskCreate (..))
import Corvus.Handlers.Apply.Execute (ApplyAction (..), executeApply)
import Corvus.Handlers.Apply.Validation (handleApplyValidate)
import Corvus.Handlers.Apply.Vm (ApplyVmCreate (..))
