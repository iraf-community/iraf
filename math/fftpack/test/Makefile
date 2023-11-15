
include ../make.inc

SRC=tcfft1.f trfft1.f tcosq1.f tcost1.f tsinq1.f tsint1.f \
    tcfft2.f trfft2.f

EXES=$(subst .f,.exe, $(SRC))

all : $(EXES)

$(EXES) : $(SRC)

%.exe : %.f
	rm -f $@
	$(F90) $< -o $@ -L../lib -l fftpack
	./$@


clean :
	rm -f $(EXES)
