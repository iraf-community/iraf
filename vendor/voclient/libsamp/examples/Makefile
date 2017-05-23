#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the libsamp example tasks.
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	= samp
VERSION    	= 1.0
PLATFORM       := $(shell uname -s)
PLMACH         := $(shell ../../getarch)
HERE           := $(shell /bin/pwd)


# secondary dependencies

LIBBASE     = lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep

.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC 	    = gcc
CINCS  	    = -I$(HERE) -I../ -I../../include -L../ -L../../lib/ -L../
CFLAGS 	    = -g -Wall -D$(PLATFORM) $(CINCS)

ifeq ("$(PLATFORM)", "Darwin")
    ifeq  ("$(PLMACH)", "macintel")
        CARCH   = -m64 -mmacosx-version-min=10.5
    else
        CARCH   = -arch i386 -arch ppc -m32 -mmacosx-version-min=10.4
    endif
else
    CLIBS       = -lm -lc -lpthread -lcurl
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

#CARCH   	= -arch i386 -arch ppc -m32 -mmacosx-version-min=10.4
CLIBS           = -lm -lc -lcurl -lpthread
CFLAGS          = -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./




#F77 	    = g77
F77 	    = gfortran
FFLAGS 	    = -g -Wall



# list of source and include files

C_SRCS 	    = snoop.c send.c
C_OBJS 	    =
C_INCS 	    =  

F77_SRCS    = 
F77_OBJS    = 
F77_INCS    =  

SPP_SRCS    = 
SPP_OBJS    = 
SPP_INCS    = 


LIBS 	    = -lsamp -lcurl

SPP_TASKS   = 
F77_TASKS   = 
C_TASKS	    = snoop send
	      
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

snoop:	snoop.c ../libsamp.a
	$(CC) $(CFLAGS) -o snoop snoop.c $(LIBS)

send:	send.c ../libsamp.a
	$(CC) $(CFLAGS) -o send send.c $(LIBS)



###########################
#  SPP Test programs
###########################

#foo_spp:  foo_spp.x
#	xc -/I../ -/L../ foo_spp.x -lsamp



###########################
#  Fortran Test programs.
###########################

#foo_f77:  foo_f77.f
#	$(F77) $(FFLAGS) -o foo_f77 foo_f77.f ../libsamp.a -lc




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

