#!/bin/sh
set -eu

pkg info -e qemu-guest-agent
test -c /dev/vtcon/org.qemu.guest_agent.0
service qemu-guest-agent status
test "$(sysrc -n qemu_guest_agent_enable)" = YES
test "$(sysrc -n nuageinit_enable)" = YES
test "$(sysrc -n firstboot_pkg_upgrade_enable)" = NO
test "$(kenv console)" = comconsole
test "$(kenv comconsole_speed)" = 115200
sysctl -n kern.console | grep -q 'ttyu0'
pgrep -f 'getty.*ttyu0' >/dev/null
