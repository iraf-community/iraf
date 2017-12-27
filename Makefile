#
#  Makefile for the IRAF source tree.
#
# ---------------------------------------------------------------------------

# Compiler Flags.

RELEASE		= v2.16
CFLAGS 		=
CDEBUGFLAGS 	= -O2 -Wall
BOOTSTRAPCFLAGS = 
        
CC 		= gcc 
AS 		= gcc -c -x assembler
AR 		= ar clq
CP 		= cp -p

export  iraf     = $(shell pwd)/

all:: update

# Do a full sysgen.
sysgen::
	@echo "Building the IRAF $(RELEASE) software tree"
	@echo "" ; date ; echo ""
	(util/mksysgen)
	@echo "" ; date ; echo ""

# Update (compile) recent changes.
update::
	@echo "Updating the IRAF $(RELEASE) software tree"
	@echo "" ; date ; echo ""
	(util/mkup)
	@echo "" ; date ; echo ""

# Update (compile) with debug libraries.
updatex::
	@echo "Updating the IRAF $(RELEASE) software tree"
	@echo "" ; date ; echo ""
	(util/mkupx)
	@echo "" ; date ; echo ""

# Update with cumulative patch of entire system
latest::
	@echo "Updating IRAF $(RELEASE) to latest release."
	@echo "" ; date ; echo ""
	(util/self_update)
	(util/iraf_update -all 2>&1 | egrep -v "unexpected end of file")
	@echo "" ; date ; echo ""

# Check if system is the latest distributed version
check_latest::
	(util/iraf_update -list)

# Update with cumulative patch of core system
latest_src::
	@echo "Updating IRAF $(RELEASE) to latest source release."
	@echo "" ; date ; echo ""
	(util/iraf_update -src)
	@echo "" ; date ; echo ""

# Update with cumulative patch of core system
latest_core::
	@echo "Updating IRAF $(RELEASE) to core release."
	@echo "" ; date ; echo ""
	(util/iraf_update -core)
	@echo "" ; date ; echo ""


# Update recent changes from the repository.
self_update::
	(util/self_update)




# Clean the IRAF tree of all binaries.
src::
	(util/mksrc)
pristine::
	(util/mksrc)

# Clean the IRAF tree of binaries for the currently configured arch.
clean::
	(util/mkclean)

# Make only the NOAO package.
noao::
	(cd noao ; mkpkg -p noao)

# Summarize the spool files.
summary::
	(mkpkg summary)
	(chdir noao   ; mkpkg -p noao   summary)




# ----------------------------------------------------------------------
# architectures
# ----------------------------------------------------------------------
showarch::
	(mkpkg arch)
generic::
	(util/mkarch generic)

macosx::
	(util/mkarch macosx)
macintel::
	(util/mkarch macintel)
redhat::
	(util/mkarch redhat)
linux::
	(util/mkarch linux)
linux64::
	(util/mkarch linux64)
linuxarm::
	(util/mkarch linuxarm)
linuxarm64::
	(util/mkarch linuxarm64)
linuxmips::
	(util/mkarch linuxmips)
freebsd::
	(util/mkarch freebsd)
cygwin::
	(util/mkarch cygwin)
sunos::
	(util/mkarch sunos)
sparc::
	(util/mkarch sparc)
ssun::
	(util/mkarch ssun)



# ----------------------------------------------------------------------
# common rules for all Makefiles - do not edit

.c.i:
	$(RM) $@
	$(CC) -E $(CFLAGS) $(_NOOP_) $*.c > $@

.SUFFIXES: .s

.c.s:
	$(RM) $@
	$(CC) -S $(CFLAGS) $(_NOOP_) $*.c

emptyrule::

cleandir::
	(util/mksrc)

distclean:: cleandir

# ----------------------------------------------------------------------
# dependencies generated by makedepend

