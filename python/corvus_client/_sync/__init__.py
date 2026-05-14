"""Synchronous Corvus client.

Wraps the async core (`corvus_client._async`) by scheduling coroutines
on a background asyncio + kj_loop thread (`corvus_client._runloop`).
Each sync method is a one-liner: `return self._rl.run(self._a.foo(...))`.
"""
