#
#  Makefile for the IRAF source tree.
#
# ---------------------------------------------------------------------------

# IRAF specific variables
export iraf = $(shell pwd)/

# If IRAFARCH is not set, automatically determine it.
# If it set, but empty, build IRAF without arch specific
# directory names.
export IRAFARCH	?= $(shell unix/hlib/irafarch.sh -current)
ifeq ($(IRAFARCH),)
unexport IRAFARCH
else
export arch = .$(IRAFARCH)
endif

# Default IRAF directory structure
export hostid = unix
export host = $(iraf)$(hostid)/
export hlib = $(host)hlib/
export hbin = $(host)bin$(arch)/
export bin = $(iraf)bin$(arch)/
export noao = $(iraf)noao/
export tmp = /tmp/

# Compiler and other build tools. While the C compiler can be chosen,
# the F77 compiler is fixed to the F2C one provided by IRAF.
export CC ?= cc
export F77 = $(hlib)f77.sh
export FC = $(F77)
export F2C = $(hbin)f2c.e
export MKPKG = $(hbin)mkpkg.e
export XC = $(hbin)xc.e
export RANLIB = ranlib

# General compiler flags. Compiler flags specific to the build of the
# host tools and software are in unix/Makefile.
export CFLAGS ?= -g -O2
export XC_CFLAGS = $(CPPFLAGS) $(CFLAGS)

.PHONY: all sysgen clean test arch noao host novos core vendor bindirs bin_links config inplace starttime

all:: sysgen

# Do a full sysgen, which consists on the host binaries, the core
# system, and the NOAO package.
sysgen: starttime host core noao
	@echo "============== IRAF build was successful! ==============="
	@echo
	@echo "Start: $(shell date -r .build_started)"
	@echo "End:   $(shell date)"
	@echo
	@echo "You may now run \"make test\" for a quick test of the build"
	@rm -f .build_started

# Just create a file on sysgen start so that its creation time can be used
starttime:
	touch .build_started

# Bootstrap first stage: build xc, mkpkg etc. without the Virtual
# Operating System (VOS)
novos: arch bindirs
	$(MAKE) -C $(host) NOVOS=yes bindir=$(hbin) install
	$(MAKE) -C $(host) clean

# Build the libraries containing the Virtual Operating System, and
# re-build the build tools (xc, mkpkg, ...) with VOS
host: novos
	$(MKPKG) syslibs
	$(MAKE) -C $(host) bindir=$(hbin) boot/install
	$(MAKE) -C $(host) clean

# Build vendor libs (cfitsio and libvotable)
vendor: host
	$(MAKE) -C $(iraf)vendor \
	    includedir=$(iraf)include/ bindir=$(bin) install
	$(MAKE) -C $(iraf)vendor clean

# Build the core system.
core: host vendor
	$(MKPKG)

# Build the NOAO package.
noao: host vendor core
	cd $(noao) && $(MKPKG) -p noao

# Run the test suite.
test:
	./test/run_tests

# Remove all binaries built. This keeps .x files that were generated
# by generic, xyacc and similar.
clean:
	$(MAKE) -C unix clean
	$(MAKE) -C vendor clean
	find ./local ./math ./pkg ./sys ./noao/[adfimnorst]* \
	     -type f -name \*.\[aeo\] -exec rm -f {} \;
	rm -f $(bin)/* noao/bin$(arch)/* $(hbin)* .build_started \
	      include/drvrsmem.h include/fitsio.h include/fitsio2.h \
	      include/longnam.h include/votParse.h include/votParse_spp.h

# Prepare the source tree for the architecture specifics
arch:
	if [ "$(shell $(hlib)irafarch.sh -nbits)" = 64 ] ; then \
	    ln -sf iraf64.h $(hlib)iraf.h && \
	    ln -sf mach64.h $(hlib)mach.h ; \
	else \
	    ln -sf iraf32.h $(hlib)iraf.h && \
	    ln -sf mach32.h $(hlib)mach.h ; \
	fi
	if [ "$(shell $(hlib)irafarch.sh -endian)" = big ] ; then \
	    ln -sf swapbe.h $(hlib)swap.h ; \
	else \
	    ln -sf swaple.h $(hlib)swap.h ; \
	fi

# Create bindirs for current architecture
bindirs:
	if [ bin$(arch) != bin ] ; then \
	    mkdir -p bin$(arch) noao/bin$(arch) unix/bin$(arch) && \
	    rm -rf bin unix/bin noao/bin && \
	    ln -s bin$(arch) bin && \
	    ln -s bin$(arch) noao/bin && \
	    ln -s bin$(arch) unix/bin ; \
	else \
	    if [ -L bin ] ; then rm -f bin unix/bin noao/bin ; fi && \
	    mkdir -p bin noao/bin unix/bin ; \
	fi

# Patch the "iraf" environment variable into shell scripts
# The "grep -q" calls are to make sure that the file was edited
config:
	sed -E -i.orig \
	    s+'^([[:space:]]*d_iraf=).*+\1"$(iraf)"'+ \
	    $(DESTDIR)$(hlib)ecl.sh \
	    $(DESTDIR)$(hlib)setup.sh \
            $(DESTDIR)$(hlib)mkiraf.sh
	grep '"$(iraf)"' $(DESTDIR)$(hlib)ecl.sh
	grep '"$(iraf)"' $(DESTDIR)$(hlib)setup.sh
	grep '"$(iraf)"' $(DESTDIR)$(hlib)mkiraf.sh
	sed -E -i.orig \
	    s+'^([[:space:]]*set[[:space:]]*d_iraf[[:space:]]*=[[:space:]]*).*+\1"$(iraf)"'+ \
	    $(DESTDIR)$(hlib)setup.csh
	grep '"$(iraf)"' $(DESTDIR)$(hlib)setup.csh

bindir = $(HOME)/.iraf/bin

# Create symbolic links for user callable scripts and executables
binary_links:
	mkdir -p $(DESTDIR)$(bindir)
	ln -sf $(hlib)ecl.sh $(DESTDIR)$(bindir)/cl
	ln -sf $(hlib)ecl.sh $(DESTDIR)$(bindir)/ecl
	ln -sf $(hlib)mkiraf.sh $(DESTDIR)$(bindir)/mkiraf
	for hprog in mkpkg rmbin rmfiles rtar sgidispatch wtar xc xyacc ; do \
	    ln -sf $(hbin)$${hprog}.e $(DESTDIR)$(bindir)/$${hprog} ; \
	done

# Do a default in-place (user) installation
inplace: config binary_links
	$(hlib)mkiraf.sh --default --init
	ln -sf $(hlib)libc/iraf.h $(hlib)/setup.*sh $(HOME)/.iraf/
	echo $(iraf) > $(HOME)/.iraf/irafroot
	if [ "$(IRAFARCH)" ] ; then \
	  echo "$(IRAFARCH)" > $(HOME)/.iraf/arch ; \
	else \
	  rm -f $(HOME)/.iraf/arch ; \
	fi

# Strip unneeded files from the installation.
#
# *Warning*: Calling this directly will remove all source files from
#            your source directory.
strip: noao/bin/x_quad.e
	cd $(DESTDIR)$(iraf) && $(hostid)/bin/rmfiles.e -f $(hostid)/hlib/strip.iraf
	cd $(DESTDIR)$(iraf)/noao && ../$(hostid)/bin/rmfiles.e -f lib/strip.noao

prefix ?= /usr/local

install: noao/bin/x_quad.e
	mkdir -p $(DESTDIR)$(prefix)/lib/iraf $(DESTDIR)$(prefix)/bin $(DESTDIR)$(prefix)/share/man/man1 $(DESTDIR)/etc/iraf
	cp -a -f bin* dev extern include lib local math mkpkg noao pkg sys test unix \
	         $(DESTDIR)$(prefix)/lib/iraf
	$(MAKE) config binary_links strip iraf=$(prefix)/lib/iraf/ bindir=$(prefix)/bin
	cp -f $(hlib)mkiraf.man $(DESTDIR)$(prefix)/share/man/man1/mkiraf.1
	cp -f $(hlib)ecl.man $(DESTDIR)$(prefix)/share/man/man1/ecl.1
	ln -sf ecl.1 $(DESTDIR)$(prefix)/share/man/man1/cl.1
	cp -f $(host)boot/mkpkg/mkpkg.man $(DESTDIR)$(prefix)/share/man/man1/mkpkg.1
	cp -f $(host)boot/spp/xc.man $(DESTDIR)$(prefix)/share/man/man1/xc.1
	cp -f $(host)boot/xyacc/xyacc.man $(DESTDIR)$(prefix)/share/man/man1/xyacc.1
	cp -f $(host)boot/generic/generic.man $(DESTDIR)$(prefix)/share/man/man1/generic.1
	cp -f $(host)gdev/sgidev/sgidispatch.man $(DESTDIR)$(prefix)/share/man/man1/sgidispatch.1
	echo $(prefix)/lib/iraf/ > $(DESTDIR)/etc/iraf/irafroot
