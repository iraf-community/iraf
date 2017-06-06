#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the VOClient Tasking Interface
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	= voTask
VERSION    	= 1.0
PLATFORM        := $(shell uname -s)
PLMACH          := $(shell uname -m)
HERE            := $(shell /bin/pwd)
BINDIR          := ../bin/
LIBDIR          := ../lib/
INCDIR          := ../include/
PYVER       	:= $(shell csh -c "python --version |& cut -c8-10")

# secondary dependencies
LIBBASE     = lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep
.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC              = gcc
CINCS           = -I$(INCDIR)  -I.

ifeq ($(PLATFORM),Darwin)
    ifeq  ($(PLMACH),x86_64)
        CARCH   = -m64
    else
        CARCH   = -m32
    endif
    PYFLAGS 	= -dynamiclib -I/usr/include/python$(PYVER)/ -lpython$(PYVER)
else
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
    PYFLAGS 	= -DLINUX
endif

CLIBS           = -lm -lc -lcurl -lpthread -lm
CFLAGS          = -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./



# includes, flags and libraries
CC 	    = gcc
CINCS  	    = -I$(HERE) -I../ -I../include -L../ -L../lib/ -L../

#F77 	    = g77
F77 	    = gfortran
FFLAGS 	    = -g -Wall

# list of source and include files
C_SRCS 	    = voParam.c voPkg.c voTask.c voTaskMethod.c voTaskUtil.c
C_OBJS 	    = voParam.o voPkg.o voTask.o voTaskMethod.o voTaskUtil.o
C_INCS 	    = voTask.h voTaskP.h

C_TASKS	    = 
TARGETS	    = $(C_TASKS)

SRCS	    = $(C_SRCS)
OBJS	    = $(C_OBJS)
HOST_LIBS   = -lcurl -lcfitsio -lpthread -lm
LIBS        = -L../../lib $(HOST_LIBS) -lvotable -lsamp -lVOClient -lcfitsio


all:
	make lib

World:  lib

install: all
	cp lib$(NAME).a $(LIBDIR)
	mv $(TARGETS) $(BINDIR)

objs:   $(OBJS)


# Targets

#all: $(TARGETS)

progs:    $(C_TASKS)

votools: pkgMain.o 	
	$(CC) $(CFLAGS) -o votools.vop pkgMain.c ../libVOApps.a $(LIBS)
	/bin/rm -rf *.dSYM

pylib:	
	$(CC) $(PYFLAGS) -o voPackage.so voPackage.c

clean:
	/bin/rm -rf .make.state .nse_depinfo *.[aeo] *.dSYM
	/bin/rm -rf $(TARGETS)

everything:
	make clean
	make all
	make install

help: HELP

install: all 



####################################
#  LIBVOTASK dependency libraries.
####################################

lib: objs
	ar rv lib$(NAME).a $(OBJS)
	$(RM) $(OBJS)



###############################################################################
# Unit test programs to be built.
###############################################################################

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)


###########################
#  C Test programs
###########################


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
	/usr/bin/gcc $(CINCS) $(CFLAGS) -g -c $< -o $@
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
