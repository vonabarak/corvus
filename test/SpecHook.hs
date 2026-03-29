-- | Global test hook applied by hspec-discover.
-- Enables parallel execution for all specs. Integration tests are safe for
-- parallelism because each 'it' block creates its own daemon and uses
-- UUID-based resource names. Unit tests that share mutable database state
-- must opt out with 'sequential'.
module SpecHook (hook) where

import Test.Hspec

hook :: Spec -> Spec
hook = parallel
