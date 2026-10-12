# List Output Columns

Text list commands accept `-O COLS` or `--columns COLS`. Select columns in the
order you want them displayed; selectors are case insensitive and surrounding
whitespace is ignored. Column headers use the same canonical names as selectors.
Without the flag, each command retains its default column set and order.

```bash
crv vm list -O ID,NAME,STATUS
crv -O NAME,NODE,AUTOSTART vm list -o text
crv disk list --columns=all
```

The special value `all` displays default columns followed by all additional
columns. It must be used alone. Duplicate columns are allowed. Empty selections,
empty comma segments, and unknown names are errors. The flag only works with
text output (the default), and only for list commands. Combining it with
`-o json` or `-o yaml` fails before connecting to the daemon. Structured output
without the flag continues to contain the complete response.

The standard Bash, Zsh, and Fish completion scripts complete column names for
both flags, including each segment in a comma-separated selection. Suggestions
use the current command's catalog, omit columns already selected, and require
no daemon connection. Before a command is known, completion offers the union of
available names. `all` is suggested only as a standalone value.

## Available columns

The default columns are listed in display order. `all` adds the additional
columns in the order shown below.

| Command | Default columns | Additional columns |
| --- | --- | --- |
| `vm list` | `ID`, `NAME`, `NODE`, `STATUS`, `CPUS`, `RAM`, `HEALTH`, `GUEST_AGENT`, `CLOUD_INIT`, `HEADLESS` | `TPM`, `AUTOSTART`, `HEALTHCHECK`, `REBOOT_QUIRK`, `CPU_MODEL`, `GRAPHICS_ADAPTER`, `VSOCK`, `BALLOON`, `RNG`, `STARTUP_TIMEOUT_SEC` |
| `disk list` | `ID`, `NAME`, `TAGS`, `FORMAT`, `SIZE`, `EPHEMERAL`, `ATTACHED_TO` | `CREATED`, `BACKING_IMAGE`, `PLACEMENTS` |
| `network list` | `ID`, `NAME`, `SUBNET`, `DHCP`, `NAT`, `STATUS`, `PEERS`, `AUTOSTART` | `DNSMASQ_PID`, `CREATED`, `VNI`, `PEER_NODE_IDS`, `DNS_SERVERS`, `DOMAIN`, `HOST_DNS` |
| `node list` | `ID`, `NAME`, `HOST`, `STATE`, `NETD`, `CPUS`, `RAM_FREE`, `DISK_FREE`, `LOAD1` | `NODE_AGENT_PORT`, `NET_AGENT_PORT`, `CREATED`, `RAM_TOTAL`, `DISK_TOTAL`, `LAST_NODE_AGENT_PUSH`, `LAST_NET_AGENT_PUSH`, `NETD_DISABLED`, `NETD_CONNECTED` |
| `template list` | `ID`, `NAME`, `CPUS`, `RAM`, `TPM` | `DESCRIPTION`, `HEADLESS`, `GUEST_AGENT`, `AUTOSTART`, `REBOOT_QUIRK`, `GRAPHICS_ADAPTER`, `VSOCK`, `BALLOON`, `RNG`, `STARTUP_TIMEOUT_SEC` |
| `task list` | `ID`, `SUBSYSTEM`, `COMMAND`, `ENTITY`, `CLIENT`, `RESULT`, `STARTED`, `DURATION`, `MESSAGE` | `PARENT_ID`, `FINISHED` |
| `ssh-key list` | `ID`, `NAME`, `PUBLIC_KEY`, `ATTACHED_VMS` | `CREATED` |
| `ssh-key list-vm VM` | `ID`, `NAME`, `PUBLIC_KEY` | `ATTACHED_VMS`, `CREATED` |
| `net-if list VM` | `ID`, `MODEL`, `TYPE`, `DEVICE`, `MAC`, `GUEST_IPS` | `NETWORK`, `IP_ADDRESS` |
| `shared-dir list VM` | `ID`, `PATH`, `TAG`, `CACHE`, `READ_ONLY`, `PID` | — |
| `audio-device list VM` | `ID`, `MODEL`, `BACKEND`, `OPTIONS` | — |
| `disk snapshot list DISK` | `ID`, `NAME`, `CREATED`, `SIZE`, `LIVE`, `QUIESCED`, `HAS_VMSTATE` | — |
| `vm snapshot list VM` | `NAME`, `CREATED`, `CARRIER`, `DISKS`, `SIZE` | `VM` |

## Values and headers

Boolean values in all text list and detail output display `yes`/`no`.
Descriptive headers replace `CI`, `AS`, `EPH`, `Q`, and `V` with `CLOUD_INIT`, `AUTOSTART`, `EPHEMERAL`,
`QUIESCED`, and `HAS_VMSTATE`; the old abbreviations are not selectors.

Absent optional values and empty lists display `-`. Timestamps are UTC in
`YYYY-MM-DD HH:MM:SS` format. Sizes use
human-readable byte units, and `STARTUP_TIMEOUT_SEC` is an integer in seconds.
`BACKING_IMAGE`, `NETWORK`, and `VM` display `name (id)` references. `PLACEMENTS`
displays `node-name (id): path`, separated by semicolons. `DNS_SERVERS` and
`PEER_NODE_IDS` contain comma-separated values; `PEERS` is a count.

`HEALTHCHECK` is the last guest-agent heartbeat timestamp; `HEALTH` is the
current health label. `IP_ADDRESS` is the configured interface address, while
`GUEST_IPS` contains addresses observed through the guest agent. Template columns
are limited to the template list response; drives and other detail fields
remain available through `template show`.
