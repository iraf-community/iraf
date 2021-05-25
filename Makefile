#
#  Makefile for the IRAF source tree.
#
# ---------------------------------------------------------------------------

# Compiler Flags.

RELEASE		= v2.16
export CFLAGS	= -g -Wall -O2 $(CARCH)
export LDFLAGS	= $(CARCH)
export  iraf     = $(shell pwd)/

all:: sysgen

# Do a full sysgen.
sysgen::
	@echo "Building the IRAF $(RELEASE) software tree"
	@echo "" ; date ; echo ""
	(util/mksysgen)
	@echo "" ; date ; echo ""

# Clean the IRAF tree of all binaries.
src pristine::
	(util/mksrc)

# Clean the IRAF tree of binaries for the currently configured arch.
clean::
	(util/mkclean)

# Make only the NOAO package.
noao::
	(cd noao ; mkpkg -p noao)


# ----------------------------------------------------------------------
# architectures
# ----------------------------------------------------------------------
showarch::
	(mkpkg arch)

macosx macintel macos64 linux linux64 freebsd freebsd64 hurd generic::
	(util/mkarch $@)
