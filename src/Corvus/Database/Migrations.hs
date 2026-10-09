-- | Retained upgrade history. Removing a module and its entry deliberately
-- retires the corresponding starting versions; never renumber migrations.
module Corvus.Database.Migrations (migrations) where

import Corvus.Database.Migration (Migration)
import qualified Corvus.Database.Migrations.V003 as V003
import qualified Corvus.Database.Migrations.V004 as V004
import qualified Corvus.Database.Migrations.V005 as V005
import qualified Corvus.Database.Migrations.V006 as V006
import qualified Corvus.Database.Migrations.V007 as V007
import qualified Corvus.Database.Migrations.V008 as V008
import qualified Corvus.Database.Migrations.V009 as V009
import qualified Corvus.Database.Migrations.V010 as V010
import qualified Corvus.Database.Migrations.V011 as V011
import qualified Corvus.Database.Migrations.V012 as V012

migrations :: [Migration]
migrations = [V003.migration, V004.migration, V005.migration, V006.migration, V007.migration, V008.migration, V009.migration, V010.migration, V011.migration, V012.migration]
