export CFLAGS := $(HSI_CF)
export FC := $(F77)
export FFLAGS := $(HSI_FF)
export FLIBS := $(HSI_F77LIBS)
export LDFLAGS := $(HSI_LF)
export LIBS = $(HSI_LIBS)

hbin: setarch
	$(MAKE) -C os $@
	$(MAKE) -C f2c $@
	$(MAKE) -C boot $@
	$(MAKE) -C gdev $@

clean:
	$(MAKE) -C os $@
	$(MAKE) -C f2c $@
	$(MAKE) -C gdev $@
	$(MAKE) -C boot $@

# Set the HSI architecture.
setarch:
	ln -sf bin.$(MACH) bin


.PHONY: hbin hlib clean setarch
