export bindir ?= $(bin)
export includedir ?= $(iraf)include/

all: libvotable

install: all
	$(MAKE) -C libvotable install

clean:
	$(MAKE) -C libvotable clean

libvotable:
	$(MAKE) -C libvotable libVOTable.a

.PHONY: libvotable clean all install
