export bindir ?= $(bin)
export includedir ?= $(iraf)include/

all: cfitsio

install: all
	mkdir -p $(includedir) $(bindir)
	$(MAKE) -C cfitsio install
	rm -rf $(bindir)pkgconfig/
	rmdir $(bindir)pkgconfig || true

clean:
	$(MAKE) -C cfitsio distclean || true

cfitsio:
	cd cfitsio && \
	./configure --includedir=$(includedir) \
	            --bindir=$(bindir) --libdir=$(bindir) \
	            --disable-curl \
	            FC=none
	$(MAKE) -C cfitsio libcfitsio.a

.PHONY: cfitsio clean all install
