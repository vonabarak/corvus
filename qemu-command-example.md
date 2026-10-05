# Example Corvus QEMU command

This is a real QEMU argument list assembled by the Corvus nodeagent in
[`src/Corvus/Node/Command.hs`](src/Corvus/Node/Command.hs). Read the code blocks
from top to bottom as one command. Each line is the next option or option/value
pair; the blocks are **not** a shell script. Names, IDs, ports, and paths are
values from this particular VM.

## VM identity and resources

```text
qemu-system-x86_64
-name w11,process=corvus-vm-1
-machine type=q35,accel=kvm
-cpu host
-enable-kvm
-m 8192
-object memory-backend-memfd,id=mem,size=8192M,share=on
-numa node,memdev=mem
-smp 8
```

- `qemu-system-x86_64` runs the x86-64 system emulator.
- `-name` names the guest `w11` and labels the host process `corvus-vm-1`.
- `-machine` selects the Q35 PCIe machine model with KVM acceleration.
- `-cpu host` exposes the host CPU model and features to the guest.
- `-enable-kvm` explicitly enables KVM.
- `-m 8192` allocates 8192 MiB (8 GiB) of guest RAM.
- `-object memory-backend-memfd` creates shareable RAM of the same size,
  identified as `mem`, so the external `virtiofsd` process can access it.
- `-numa node,memdev=mem` uses that memory backend for the guest NUMA node.
- `-smp 8` presents eight virtual CPUs.

## PCIe buses and storage controller

```text
-device pcie-root-port,id=hotplug-rp,chassis=0,slot=0
-device pcie-pci-bridge,id=hotplug,bus=hotplug-rp
-device pcie-root-port,id=virtio-rp-1,chassis=2,slot=2
-device pcie-root-port,id=scsi-rp,chassis=1,slot=1
-device virtio-scsi-pci,id=scsi0,bus=scsi-rp
```

- `hotplug-rp` is a PCIe root port. The `hotplug` PCI bridge attaches to it
  and supplies a bus for later device hotplug.
- `virtio-rp-1` is a dedicated PCIe root port for boot-time virtio disk 1.
- `scsi-rp` is another root port. The always-present `scsi0` virtio SCSI
  controller attaches there and can also accept disks added later.
- The `chassis` and `slot` values place each root port in the guest's PCIe
  topology; the `id` and `bus` values link devices to their ports.

## Guest communication and TPM

```text
-device vhost-vsock-pci,guest-cid=1000,id=vsock0
-device virtio-balloon-pci,id=balloon0
-object rng-random,id=rng0,filename=/dev/urandom
-device virtio-rng-pci,rng=rng0,id=virtio-rng0
-device virtio-serial,id=virtio-serial0
-chardev socket,id=qga0,path=/run/user/1000/corvus/vms/1/qga.sock,server=on,wait=off
-device virtserialport,chardev=qga0,name=org.qemu.guest_agent.0
-chardev socket,id=chrtpm,path=/run/user/1000/corvus/vms/1/swtpm.sock
-tpmdev emulator,id=tpm0,chardev=chrtpm
-device tpm-crb,tpmdev=tpm0
```

- `vhost-vsock-pci` supplies host/guest vsock communication; this guest's
  context ID (CID) is `1000`.
- `virtio-balloon-pci` lets the host adjust the guest's usable memory.
- `rng-random` and `virtio-rng-pci` provide host entropy to the guest.
  Vsock, balloon, and RNG are each enabled by default and can be disabled per VM.
- `virtio-serial` supplies guest serial ports for the guest agent and SPICE
  agent.
- The `qga0` socket listens for QEMU guest agent traffic without delaying VM
  startup; the following `virtserialport` exposes it under the guest agent's
  standard port name.
- The `chrtpm` socket connects QEMU to a separate `swtpm` emulator.
  `-tpmdev` names that backend `tpm0`, and `tpm-crb` presents it to the
  guest through the TPM CRB interface.

## Display and USB redirection

```text
-spice addr=0.0.0.0,port=5900,disable-ticketing=off
-chardev spicevmc,id=vdagent,name=vdagent
-device virtio-vga
-device virtserialport,chardev=vdagent,name=com.redhat.spice.0
-device nec-usb-xhci,id=xhci
-chardev spicevmc,id=usbredirchardev1,name=usbredir
-device usb-redir,chardev=usbredirchardev1,id=usbredirdev1
-chardev spicevmc,id=usbredirchardev2,name=usbredir
-device usb-redir,chardev=usbredirchardev2,id=usbredirdev2
-chardev spicevmc,id=usbredirchardev3,name=usbredir
-device usb-redir,chardev=usbredirchardev3,id=usbredirdev3
```

- `-spice` offers the VM display on all host interfaces at port `5900`;
  `disable-ticketing=off` leaves SPICE ticketing enabled.
- The `vdagent` SPICE channel connects to the guest's
  `com.redhat.spice.0` virtio serial port. `virtio-vga` is the guest's
  graphics adapter.
- `nec-usb-xhci` supplies an emulated USB 3 controller.
- Each numbered `spicevmc` channel pairs with the `usb-redir` device bearing
  the same number. The three pairs allow three USB redirection channels.

## Audio and control sockets

```text
-audiodev spice,id=audio2
-device ich9-intel-hda,id=hda2
-device hda-micro,bus=hda2.0,audiodev=audio2
-monitor unix:/run/user/1000/corvus/vms/1/monitor.sock,server,nowait
-qmp unix:/run/user/1000/corvus/vms/1/qmp.sock,server,nowait
```

- `-audiodev spice` routes guest audio through the SPICE backend `audio2`.
- `ich9-intel-hda` is the guest audio controller; `hda-micro` adds a speaker
  and microphone codec on that controller using `audio2`.
- `-monitor` exposes the human monitor (HMP) on a Unix socket.
- `-qmp` exposes the machine-readable QMP control socket used by the
  nodeagent. Both sockets listen without waiting for a client.

## Disk and UEFI firmware

```text
-blockdev driver=qcow2,node-name=drive-1,read-only=off,cache.direct=off,cache.no-flush=off,discard=unmap,file.driver=file,file.filename=/home/bobr/VMs/w11/w11-disk.qcow2,file.read-only=off
-device virtio-blk-pci,id=device-1,drive=drive-1,bus=virtio-rp-1,write-cache=on,discard=on
-drive file=/usr/share/edk2/OvmfX64/OVMF_CODE_4M.secboot.qcow2,format=qcow2,if=pflash,readonly=on
-drive file=/home/bobr/VMs/w11/w11-ovmf-vars-secboot.qcow2,format=qcow2,if=pflash
```

- `-blockdev` opens the writable qcow2 file as block node `drive-1`.
  `file.driver=file` and `file.filename` identify the host file;
  `read-only=off` and `file.read-only=off` make both layers writable.
  `cache.direct=off` allows host file caching, `cache.no-flush=off` honors
  guest flushes, and `discard=unmap` passes discard requests to the file.
- `virtio-blk-pci` presents `drive-1` as a guest disk on `virtio-rp-1`.
  It advertises a write cache and accepts guest discard requests.
- The first `-drive` maps the Secure Boot OVMF firmware code as read-only
  pflash. The second maps this VM's writable OVMF variable store as pflash.

## Network and shared directory

```text
-netdev tap,id=net0,ifname=corvus-tap-1,script=no,downscript=no
-device virtio-net-pci,netdev=net0,mac=52:54:00:2f:42:cf
-chardev socket,id=virtiofs0,path=/run/user/1000/corvus/vms/1/virtiofsd-dropbox.sock
-device vhost-user-fs-pci,chardev=virtiofs0,tag=dropbox
```

- `-netdev tap` connects `net0` to the existing host TAP interface
  `corvus-tap-1`. QEMU does not run network setup or teardown scripts.
- `virtio-net-pci` presents a guest NIC backed by `net0` with the shown MAC
  address.
- The `virtiofs0` socket connects QEMU to the `virtiofsd` process for the
  shared directory. `vhost-user-fs-pci` exposes it as a virtio-fs device
  under the guest mount tag `dropbox`.
