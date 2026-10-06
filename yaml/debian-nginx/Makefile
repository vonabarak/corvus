DISKS := debian-12-nginx
TEMPLATES :=
PIPELINE := debian-nginx.yml

include ../image.mk

dependencies:
	+$(MAKE) -C ../multi-os ensure
