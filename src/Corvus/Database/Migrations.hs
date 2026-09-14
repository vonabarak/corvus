-- | Retained upgrade history. Removing a module and its entry deliberately
-- retires the corresponding starting versions; never renumber migrations.
module Corvus.Database.Migrations (migrations) where

import Corvus.Database.Migration (Migration)
import qualified Corvus.Database.Migrations.V003 as V003

migrations :: [Migration]
migrations = [V003.migration]
