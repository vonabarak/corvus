subarch: amd64
target: stage4
rel_type: corvus
version_stamp: headless
profile: default/linux/amd64/23.0/no-multilib/systemd
snapshot_treeish: @REPO_COMMIT@
source_subpath: seed/@SEED@
portage_confdir: /usr/local/lib/corvus-build/catalyst/portage
keep_repos: gentoo
binrepo_path: amd64/binpackages/23.0/x86-64
pkgcache_path: /mnt/corvus-cache/catalyst/packages/amd64-systemd

stage4/use: bash-completion
stage4/packages:
    net-misc/openssh
    app-emulation/qemu-guest-agent
    mail-mta/msmtp
    app-admin/sudo
    sys-fs/growpart
    sys-fs/dosfstools
    sys-boot/efibootmgr
    net-fs/nfs-utils
    sys-block/thin-provisioning-tools
    app-editors/vim
    app-shells/bash-completion

boot/kernel: gentoo
boot/kernel/gentoo/sources: sys-kernel/gentoo-sources
boot/kernel/gentoo/config: /usr/local/lib/corvus-build/catalyst/kernel.config
stage4/gk_mainargs: --install --no-bootloader --kernel-filename=vmlinuz-catalyst --save-config --kernel-config-filename=config-catalyst kernel
stage4/root_overlay: /usr/local/lib/corvus-build/catalyst/root-overlay
stage4/fsscript: /usr/local/lib/corvus-build/catalyst/finish.sh
stage4/unmerge: sys-kernel/genkernel sys-kernel/gentoo-sources
stage4/rm: /usr/src/linux /boot/initramfs* /etc/portage/profile
