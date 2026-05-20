"""Node-registration shortcut: hand off to ``crv node add``.

The Corvus daemon stores its node list in Postgres and a fresh
deployment needs at least one ``Node`` row before the scheduler
will pick anything. Running ``crv node add`` is one command short
of automatic — corvus-admin already knows the node's name (the
cert CN), the host (the SAN IP), and the conventional ports —
so this just shells out so the admin doesn't have to remember.
"""

from __future__ import annotations

import shutil
import subprocess
import time
from dataclasses import dataclass


class RegisterError(RuntimeError):
    """Raised when the underlying ``crv`` invocation fails or the
    healthcheck never arrives. Carries the failed argv + stderr."""

    def __init__(self, message: str, *, stderr: str | None = None) -> None:
        super().__init__(message)
        self.stderr = stderr


# Default ports baked into Corvus' nodeagent / netd defaults. The
# Haskell side surfaces these from `Corvus.Node.Server.defaultNodeAgentPort`
# / `Corvus.Netd.Server.defaultNetdPort` — kept in sync here so
# `corvus-admin register foo --host 10.0.0.21` works with zero
# extra flags.
DEFAULT_NODE_AGENT_PORT = 9878
DEFAULT_NET_AGENT_PORT = 9877


@dataclass
class RegisterResult:
    name: str
    host: str
    node_agent_port: int
    net_agent_port: int
    healthy: bool
    # The raw output of `crv node show` after register, kept for
    # callers that want to inspect the agent-pushed stats.
    show_stdout: str


def register_node(
    *,
    name: str,
    host: str,
    node_agent_port: int = DEFAULT_NODE_AGENT_PORT,
    net_agent_port: int = DEFAULT_NET_AGENT_PORT,
    base_path: str | None = None,
    description: str | None = None,
    crv_path: str | None = None,
    healthcheck_timeout_sec: float = 15.0,
) -> RegisterResult:
    """Run ``crv node add`` and poll ``crv node show`` until the
    healthcheck arrives. Returns details once the node is live.

    ``crv_path`` defaults to whichever ``crv`` is first on
    ``$PATH``; pass an absolute path for tests or unusual layouts.
    """

    crv = crv_path or shutil.which("crv")
    if crv is None:
        raise RegisterError(
            "crv not found on $PATH; install it (`make install`) or pass crv_path"
        )

    add_argv = [
        crv,
        "node",
        "add",
        name,
        "--host",
        host,
        "--node-agent-port",
        str(node_agent_port),
        "--net-agent-port",
        str(net_agent_port),
    ]
    if base_path is not None:
        add_argv += ["--base-path", base_path]
    if description is not None:
        add_argv += ["--description", description]

    proc = subprocess.run(add_argv, text=True, capture_output=True)
    if proc.returncode != 0:
        raise RegisterError(
            f"`crv node add` failed (rc={proc.returncode}): {proc.stderr.strip()}",
            stderr=proc.stderr,
        )

    show_stdout, healthy = _poll_show(
        crv, name, timeout_sec=healthcheck_timeout_sec
    )
    return RegisterResult(
        name=name,
        host=host,
        node_agent_port=node_agent_port,
        net_agent_port=net_agent_port,
        healthy=healthy,
        show_stdout=show_stdout,
    )


def _poll_show(
    crv: str,
    name: str,
    *,
    timeout_sec: float,
    poll_interval_sec: float = 0.5,
) -> tuple[str, bool]:
    """Run ``crv node show`` repeatedly until either the
    healthcheck field looks non-empty or the timeout elapses.
    Returns the last stdout + a healthy/not flag."""

    deadline = time.monotonic() + timeout_sec
    last_stdout = ""
    while time.monotonic() < deadline:
        proc = subprocess.run(
            [crv, "node", "show", name],
            text=True,
            capture_output=True,
        )
        last_stdout = proc.stdout
        # The current CLI prints "Health check: <iso-datetime>"
        # once an agent push arrives. Anything else (or "-") means
        # the supervisor hasn't dialed yet. Use a forgiving
        # substring match so cosmetic changes to the CLI don't
        # break this poll silently.
        text = proc.stdout.lower()
        if "health" in text and "agent unavailable" not in text:
            # The Haskell-side renderer prints the timestamp in
            # ISO format; any non-empty value other than "-" or
            # "never" counts.
            for line in proc.stdout.splitlines():
                lower = line.lower()
                if "healthcheck" in lower or "health check" in lower:
                    val = line.split(":", 1)[-1].strip()
                    if val and val not in ("-", "never", "unknown"):
                        return last_stdout, True
        time.sleep(poll_interval_sec)
    return last_stdout, False
