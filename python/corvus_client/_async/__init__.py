"""Async core for the Corvus client.

Every RPC call lives here as a coroutine. The sync wrapper in
`corvus_client._sync` schedules these coroutines onto a background
asyncio loop so the public API is symmetric in both faces.
"""
