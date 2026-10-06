DISKS := corvus-test-vm
TEMPLATES :=
PIPELINE := corvus-test-vm.yml

include ../image.mk

dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../multi-os ensure
