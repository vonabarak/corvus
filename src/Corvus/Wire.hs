-- | Cap'n Proto wire layer: converts between Corvus's existing
-- Haskell domain types ('Corvus.Model' entities and the
-- 'Corvus.Protocol.*' info / detail records) and the parsed
-- representations of the generated 'Capnp.Gen.*' schema modules.
--
-- This module is a single import surface that re-exports the
-- per-subsystem converters; see "Corvus.Wire.Enums",
-- "Corvus.Wire.Common", etc. for the granular APIs.
module Corvus.Wire
  ( module Corvus.Wire.Errors
  , module Corvus.Wire.Time
  , module Corvus.Wire.Enums
  , module Corvus.Wire.Common
  , module Corvus.Wire.CloudInit
  , module Corvus.Wire.Disk
  , module Corvus.Wire.Network
  , module Corvus.Wire.Node
  , module Corvus.Wire.SshKey
  , module Corvus.Wire.SharedDir
  , module Corvus.Wire.Vm
  , module Corvus.Wire.Task
  , module Corvus.Wire.Apply
  , module Corvus.Wire.Template
  )
where

import Corvus.Wire.Apply
import Corvus.Wire.CloudInit
import Corvus.Wire.Common
import Corvus.Wire.Disk
import Corvus.Wire.Enums
import Corvus.Wire.Errors
import Corvus.Wire.Network
import Corvus.Wire.Node
import Corvus.Wire.SharedDir
import Corvus.Wire.SshKey
import Corvus.Wire.Task
import Corvus.Wire.Template
import Corvus.Wire.Time
import Corvus.Wire.Vm
