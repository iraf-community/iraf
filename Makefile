#
#  Makefile for the IRAF source tree.
#
# ---------------------------------------------------------------------------

# IRAF specific variables
export iraf	= $(shell pwd)/
export IRAFARCH	?= $(shell unix/hlib/irafarch.sh -current)

# Default IRAD directory structure
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
export RANLIB = ranlib

# General compiler flags. Compiler flags specific to the build of the
# host tools and software are in unix/Makefile.
export CFLAGS ?= -g -Wall -O2
CFLAGS += $(CARCH)
export LDFLAGS = $(CARCH)
export XC_CFLAGS = $(CPPFLAGS) $(CFLAGS) -I$(iraf)include

all:: sysgen

# Do a full sysgen.
sysgen: bin
	# Bootstrap first stage: build xc, mkpkg etc. without the
	# Virtual Operating System (VOS)
	$(MAKE) -C $(host) NOVOS=yes hbin clean

	# Build the libraries containing the Virtual Operating System
	(cd $(iraf)sys && $(MKPKG))

	# Re-build the build tools (xc, mkpkg, ...) with VOS
	$(MAKE) -C $(host) boot/hbin clean

	# Build vendor libs (cfitsio and libvotable)
	$(MAKE) -C $(iraf)vendor all

	# Build the full core system
	$(MKPKG)

	# Build the NOAO package
	(cd $(iraf)noao && noao=$(iraf)noao/ $(MKPKG) -p noao)


# Clean the IRAF tree of all binaries.
src pristine::
	util/mksrc

# Clean the IRAF tree of binaries for the currently configured arch.
clean::
	util/mkclean

# Make only the NOAO package.
noao::
	cd noao ; $(MKPKG) -p noao


bin: $(IRAFARCH)

macosx macintel macos64 linux linux64 freebsd freebsd64 hurd generic::
	util/mkarch $@
