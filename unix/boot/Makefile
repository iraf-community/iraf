export CFLAGS := $(HSI_CF)
export FFLAGS := $(HSI_FF)
export LDFLAGS := $(HSI_LF)
export LIBS := $(HSI_LIBS)
export FLIBS := $(HSI_F77LIBS)
export FC := $(F77)

hlib: bootlib/hbin
	$(MAKE) -C generic $@
	$(MAKE) -C mkpkg $@
	$(MAKE) -C rmbin $@
	$(MAKE) -C rmfiles $@
	$(MAKE) -C rtar $@
	$(MAKE) -C wtar $@
	$(MAKE) -C xyacc $@
	$(MAKE) -C spp $@

bootlib/hbin:
	$(MAKE) -C bootlib hbin


clean:
	$(MAKE) -C bootlib $@
	$(MAKE) -C generic $@
	$(MAKE) -C mkpkg $@
	$(MAKE) -C rmbin $@
	$(MAKE) -C rmfiles $@
	$(MAKE) -C rtar $@
	$(MAKE) -C wtar $@
	$(MAKE) -C xyacc $@
	$(MAKE) -C spp $@

.PHONY: hlib clean bootlib/hbin
