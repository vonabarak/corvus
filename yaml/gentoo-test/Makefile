.PHONY: clean build

all: build

clean:
	crv vm delete --delete-disks gentoo-corvus-test
	crv template delete gentoo-headless
	crv disk delete gentoo-base-headless
	crv template delete gentoo-cloud


build:
	crv build gentoo-test.yml --wait
