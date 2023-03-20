#
#  Makefile for the IRAF source tree.
#
# ---------------------------------------------------------------------------

# IRAF specific variables
export iraf = $(shell pwd)/
export IRAFARCH	?= $(shell unix/hlib/irafarch.sh -current)

# Default IRAF directory structure
export hostid = unix
export host = $(iraf)$(hostid)/
export hlib = $(host)hlib/
export hbin = $(host)bin.$(IRAFARCH)/
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
export CFLAGS ?= -g -Wall -O2
CFLAGS += $(CARCH)
export LDFLAGS += $(CARCH)
export XC_CFLAGS = $(CPPFLAGS) $(CFLAGS) -I$(iraf)include

.PHONY: all sysgen clean test arch

all:: sysgen

# Do a full sysgen.
sysgen: arch
	# Bootstrap first stage: build xc, mkpkg etc. without the
	# Virtual Operating System (VOS)
	$(MAKE) -C $(host) NOVOS=yes bindir=$(hbin) install clean

	# Build the libraries containing the Virtual Operating System
	(cd $(iraf)sys && $(MKPKG))

	# Re-build the build tools (xc, mkpkg, ...) with VOS
	$(MAKE) -C $(host) bindir=$(hbin) boot/install clean

	# Build vendor libs (cfitsio and libvotable)
	$(MAKE) -C $(iraf)vendor \
	    includedir=$(iraf)include bindir=$(iraf)bin.$(IRAFARCH) \
	    install clean

	# Build the full core system
	$(MKPKG)

	# Build the NOAO package
	(cd $(iraf)noao && noao=$(iraf)noao/ $(MKPKG) -p noao)

test:
	env -u XC_CFLAGS ./test/run_tests

clean:
	$(MAKE) -C unix clean
	$(MAKE) -C vendor clean
	find ./local ./math ./pkg ./sys ./noao/[adfimnorst]* \
	     -type f -name \*.\[aeo\] -exec rm -f {} \;
	rm -f bin.$(IRAFARCH)/* noao/bin.$(IRAFARCH)/* $(hbin)* \
	      include/drvrsmem.h include/fitsio.h include/fitsio2.h \
	      include/longnam.h include/votParse.h include/votParse_spp.h

arch:
	mkdir -p bin.$(IRAFARCH) noao/bin.$(IRAFARCH) unix/bin.$(IRAFARCH)
	rm -f bin unix/bin noao/bin
	ln -s bin.$(IRAFARCH) bin
	(cd noao && ln -s bin.$(IRAFARCH) bin)
	(cd unix && ln -s bin.$(IRAFARCH) bin)

	if [ "$(shell $(hlib)irafarch.sh -nbits)" = 64 ] ; then \
	    ( cd unix/hlib && \
	      ln -sf iraf64.h iraf.h && \
	      ln -sf mach64.h mach.h ) ; \
	else \
	    ( cd unix/hlib && \
	      ln -sf iraf32.h iraf.h && \
	      ln -sf mach32.h mach.h ) ; \
	fi
	if [ "$(shell $(hlib)irafarch.sh -endian)" = big ] ; then \
	    ( cd unix/hlib && ln -sf swapbe.h swap.h ) ; \
	else \
	    ( cd unix/hlib && ln -sf swaple.h swap.h ) ; \
	fi
