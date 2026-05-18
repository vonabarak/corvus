-- | DB row → Cap'n Proto spec translators for `corvus-nodeagent`.
--
-- Empty in Phase 1: nothing crosses the wire yet beyond
-- bootstrap. Disk / cloud-init translators land in Phase 2; VM
-- translators in Phase 3. Mirror of "Corvus.NetAgentClient.Spec"
-- by intent.
module Corvus.NodeAgentClient.Spec () where
