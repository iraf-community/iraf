export HSI_CF = $(XC_CFLAGS)
export HSI_XF = -x -Inolibc -/Wall -/O2
export HSI_FF = -g -DBLD_KERNEL -O2
export HSI_LF = $(LDFLAGS)
export HSI_F77LIBS =
export HSI_LFLAGS =

ifdef NOVOS
export HSI_LIBS := ${hlib}libboot.a ${hlib}libos.a -lm
export HSI_CF += -DNOVOS
else
export HSI_LIBS := $(hlib)libboot.a $(iraf)lib/libsys.a $(iraf)lib/libvops.a $(hlib)libos.a $(hbin)libf2c.a -lm
endif

export CFLAGS := $(HSI_CF)
export ASFLAGS := $(CFLAGS) # for zsvjmp.S
export FFLAGS := $(HSI_FF)
export FLIBS := $(HSI_F77LIBS)
export LDFLAGS := $(HSI_LF)
export LIBS = $(HSI_LIBS)

boot/hbin:
	$(MAKE) -C boot hbin

hbin:
	$(MAKE) -C os $@
	$(MAKE) -C f2c $@
	$(MAKE) -C boot $@
	$(MAKE) -C gdev $@

clean:
	$(MAKE) -C os $@
	$(MAKE) -C f2c $@
	$(MAKE) -C gdev $@
	$(MAKE) -C boot $@

.PHONY: hbin boot/hbin clean
