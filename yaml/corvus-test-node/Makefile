DISKS := corvus-test-node
TEMPLATES := corvus-test-node
PIPELINE := corvus-test-node.yml

include ../image.mk

dependencies:
	+$(MAKE) -C ../../integration_tests/keys build
	+$(MAKE) -C ../gentoo-test ensure-headless
