#!/bin/sh
wipefs -a "$DEV"
sgdisk --zap-all "$DEV"
sgdisk -n 1:0:+1M  -t 1:ef02 -c 1:bios "$DEV"
sgdisk -n 2:0:0    -t 2:8300 -c 2:root "$DEV"
partprobe "$DEV" || sleep 2

mkfs.ext4 -F -L corvus-root "${DEV}2"
