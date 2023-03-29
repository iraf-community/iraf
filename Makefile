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
export CFLAGS ?= -g -Wall -O2
CFLAGS += $(CARCH)
export LDFLAGS += $(CARCH)
export XC_CFLAGS = $(CPPFLAGS) $(CFLAGS) -I$(iraf)include

.PHONY: all sysgen clean test arch noao host novos core vendor bindirs bin_links config inplace

all:: sysgen

# Do a full sysgen, which consists on the host binaries, the core
# system, and the NOAO package.
sysgen: host core noao

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
# The XC_CFLAGS are reset here since they produce warnings that
# confuse the test scripts.
test:
	env -u XC_CFLAGS ./test/run_tests

# Remove all binaries built. This keeps .x files that were generated
# by generic, xyacc and similar.
clean:
	$(MAKE) -C unix clean
	$(MAKE) -C vendor clean
	find ./local ./math ./pkg ./sys ./noao/[adfimnorst]* \
	     -type f -name \*.\[aeo\] -exec rm -f {} \;
	rm -f $(bin)/* noao/bin$(arch)/* $(hbin)* \
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
	sed -E s+'^([[:space:]]*d_iraf=).*+\1"$(iraf)"'+ \
	       -i $(hlib)ecl.sh $(hlib)cl.sh
	grep '"$(iraf)"' $(hlib)ecl.sh
	grep '"$(iraf)"' $(hlib)cl.sh
	sed -E s+'^([[:space:]]*export)?([[:space:]]*iraf=).*+\1\2"$(iraf)"'+ \
	       -i $(hlib)setup.sh $(hlib)mkiraf.sh
	grep '"$(iraf)"' $(hlib)setup.sh
	grep '"$(iraf)"' $(hlib)mkiraf.sh
	sed -E s+'^([[:space:]]*setenv[[:space:]]*iraf[[:space:]]*).*+\1"$(iraf)"'+ \
	       -i $(hlib)setup.csh
	grep '"$(iraf)"' $(hlib)setup.csh

bindir = $(HOME)/.iraf/bin/

# Create symbolic links for user callable scripts and executables
binary_links:
	mkdir -p $(bindir)
	for script in mkiraf cl ecl ; do \
	    ln -sf $(hlib)$${script}.sh $(bindir)$${script} ; \
	done
	for hprog in mkpkg rmbin rmfiles rtar sgidispatch wtar xc xyacc ; do \
	    ln -sf $(hbin)$${hprog}.e $(bindir)$${hprog} ; \
	done

# Do a default in-place (user) installation
inplace: config binary_links
	$(hlib)mkiraf.sh --default --init
	ln -sf $(hlib)libc/iraf.h $(hlib)/setup.*sh $(HOME)/.iraf/
	if [ "$(IRAFARCH)" ] ; then \
	  echo "$(IRAFARCH)" > $(HOME)/.iraf/arch ; \
	else \
	  rm -f $(HOME)/.iraf/arch ; \
	fi
