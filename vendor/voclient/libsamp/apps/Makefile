#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the libsamp application tasks.
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	= samp
VERSION    	= 1.0
PLATFORM       := $(shell uname -s)
PLMACH	       := $(shell uname -m)
HERE           := $(shell /bin/pwd)
BINDIR          := ../../bin/
LIBDIR          := ../../lib/
INCDIR          := ../../include/


# secondary dependencies
LIBBASE     = lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep
.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


ifeq ($(PLATFORM), "Darwin")
    ifeq  ($(PLMACH), "x86_64")
        CARCH   = -m64
    else
        CARCH   = -m32
    endif
else
    CLIBS       = -lm -lc -lpthread
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

CFLAGS          = -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./



# includes, flags and libraries
CC 	    = gcc
CINCS  	    = -I$(HERE) -I../ -I../../include -L../ -L../../lib/ -L../
CFLAGS 	    = -g -Wall -D$(PLATFORM) $(CINCS)

#F77 	    = g77
F77 	    = gfortran
FFLAGS 	    = -g -Wall



# list of source and include files

C_SRCS 	    = samp.c
C_OBJS 	    =
C_INCS 	    =  

F77_SRCS    = 
F77_OBJS    = 
F77_INCS    =  

SPP_SRCS    = 
SPP_OBJS    = 
SPP_INCS    = 


LIBS 	    = -lsamp -lcurl $(CLIBS)

SPP_TASKS   = 
F77_TASKS   = 
C_TASKS	    = samp 
	      
TARGETS	    = $(F77_TASKS) $(SPP_TASKS) $(C_TASKS)


# Targets

all: $(TARGETS)

c_progs:    $(C_TASKS)
spp_progs:  $(SPP_TASKS)
f77_progs:  $(F77_TASKS)

clean:
	/bin/rm -rf .make.state .nse_depinfo *.[aeo] *.dSYM
	/bin/rm -rf $(TARGETS)

everything:
	make clean
	make all
	make install

help: HELP

install: all 



###############################################################################
# Unit test programs to be built.
###############################################################################

demo:	$(TARGETS)


###########################
#  C Test programs
###########################

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)

samp:	samp.c ../libsamp.a
	$(CC) $(CFLAGS) -o samp samp.c $(LIBS)



###########################
#  SPP Test programs
###########################



###########################
#  Fortran Test programs.
###########################




###############################################################################
# Leave this stuff alone.
###############################################################################

$(STATICLIB): $(C_SRCS:%.c=Static/%.o)
	/usr/bin/ar rv $@ $?
Static/%.o: %.c $(C_INCS)
	/usr/bin/gcc $(CINCS) $(CFLAGS) -c $< -o $@
Static:
	/bin/mkdir $@
	chmod 777 $@

$(SHAREDLIB): $(C_SRCS:%.c=Shared/%.o)
	/usr/bin/ld -shared -o $@ $? -lc -ldl
Shared/%.o: %.c $(C_INCS)
	/usr/bin/gcc $(CINCS) $(CFLAGS) -fpic -shared -c $< -o $@
Shared:
	/bin/mkdir $@
	chmod 777 $@

