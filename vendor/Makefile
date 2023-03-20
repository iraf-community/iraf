export bindir ?= $(bin)
export includedir ?= $(iraf)include/

all: cfitsio libvotable

install: all
	$(MAKE) -C cfitsio install
	rm -rf $(bindir)pkgconfig/
	rmdir $(bindir)pkgconfig || true
	$(MAKE) -C libvotable install

clean:
	$(MAKE) -C cfitsio distclean || true
	$(MAKE) -C libvotable clean

cfitsio:
	cd cfitsio && \
	./configure --includedir=$(includedir) \
	            --bindir=$(bindir) --libdir=$(bindir)
	$(MAKE) -C cfitsio libcfitsio.a

libvotable:
	$(MAKE) -C libvotable libVOTable.a

.PHONY: cfitsio libvotable clean all install
