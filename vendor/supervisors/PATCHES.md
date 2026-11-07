# Local changes to supervisors 0.2.1.0

Source: https://hackage.haskell.org/package/supervisors-0.2.1.0

Corvus owns this local package alongside capnp and lifetimes. Shutdown uses a
private asynchronous exception, catches it around the complete worker lifetime,
and joins admitted children. Admission is atomic with a start gate; submissions
after shutdown cannot start a body. Unexpected exceptions are still reported,
with an optional contextual reporter used by Corvus services. Tests cover normal
shutdown, parent failure, unexpected cancellation, queued workers, and admission
races. No process-wide exception handler is replaced.
