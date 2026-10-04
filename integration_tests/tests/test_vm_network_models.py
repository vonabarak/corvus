"""Verify that changing NIC models changes the PCI device in the guest."""

from corvus_test_harness import SingleNodeCase, VmSsh


class TestNetworkModels(SingleNodeCase):
    def test_edit_model_changes_guest_pci_device(self) -> None:
        with VmSsh(self) as vm:
            assert len(vm.cap.show().net_ifs) == 1
            nic = vm.cap.show().net_ifs[0]
            assert nic.model == "virtio-net-pci"
            pci = vm.run("lspci -n").stdout
            assert "1af4:1000" in pci or "1af4:1041" in pci, pci

            assert vm.shell is not None
            vm.shell.close()
            vm.shell = None
            vm.cap.stop(wait=True)
            vm.cap.edit_net_if(nic.id, "e1000")
            assert vm.cap.show().net_ifs[0].model == "e1000"
            vm.cap.start(wait=True)
            with self.vm_shell(vm.cap) as shell:
                shell.wait_ready(timeout_sec=90)
                pci = shell.run("lspci -n").stdout
                assert "8086:100e" in pci, pci
