#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the VOClient Package Applications
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	= VOApps
VERSION    	= 1.0
PLATFORM        := $(shell uname -s)
PLMACH          := $(shell uname -m)
HERE            := $(shell /bin/pwd)
BINDIR          := ../bin/
LIBDIR          := ../lib/
INCDIR          := ../include/


# secondary dependencies
LIBBASE     = lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep
.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC 	    = gcc
CINCS  	    = -I$(HERE) -I../ -I$(INCDIR) -I../../../include 
LFLAGS	    = -L../ -L../lib/ -L../../../bin/

#F77 	    = g77
F77 	    = gfortran
FFLAGS 	    = -g -Wall


LIBCFITSIO	= ../lib/libcfitsio.a

ifeq ($(PLATFORM),Darwin)
    ifeq ($(PLMACH),x86_64)
        CARCH   = -m64
    else
        CARCH   = -m32
    endif
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),macintel)
            CARCH = -m64
        else
            CARCH = -m32
        endif
    endif
else
    CLIBS       =  -lcurl -lm -lc -lpthread -lrt
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

#CLIBS           = -lcurl -lm -lpthread -lc
CFLAGS          = -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./ 



# list of source and include files

C_SRCS 	    = votcnv.c votget.c votinfo.c vosesame.c vodata.c voregistry.c \
	      votpos.c votcat.c votsplit.c votstat.c votjoin.c voatlas.c \
	      votsort.c vosamp.c voiminfo.c \
	      voimage.c vocatalog.c vospectra.c votopic.c \
	      voApps_spp.c
C_OBJS 	    = votcnv.o votget.o votinfo.o vosesame.o vodata.o voregistry.o \
	      votpos.o votcat.o votsplit.o votstat.o votjoin.o voatlas.o \
	      votsort.o vosamp.o voiminfo.o \
	      voimage.o vocatalog.o vospectra.o votopic.o \
	      voApps_spp.o
C_INCS 	    = voApps.h voAppsP.h



F77_SRCS    =
F77_OBJS    =
F77_INCS    =  

SPP_SRCS    =
SPP_OBJS    =
SPP_INCS    = votParse_spp.h

SPP_TASKS   =
F77_TASKS   =
C_TASKS	    = voregistry \
	      vosesame \
	      vodata voatlas voimage vocatalog vospectra votopic \
	      votcnv votget votpos votinfo votstat votsort \
	      vosamp \
	      voiminfo \

	      # Not yet imlemented (and/or working)
	      #votcat votsplit votjoin

	      
TARGETS	    = $(F77_TASKS) $(SPP_TASKS) $(C_TASKS)

SRCS	    = $(C_SRCS) $(F77_SRCS) $(SPP_SRCS) 
OBJS	    = $(C_OBJS) $(F77_OBJS) $(SPP_OBJS) 
HOST_LIBS   = -lexpat -lcurl $(LFLAGS) -lcfitsio $(CLIBS)
LIBS        = lib$(NAME).a -lVOTable -lVOClient -lsamp $(HOST_LIBS)



all: c_progs
	mkdir -p $(LIBDIR) $(BINDIR)
	cp lib$(NAME).a $(LIBDIR)
	cp $(TARGETS) $(BINDIR)


World:  lib

install: all
	cp lib$(NAME).a $(LIBDIR)
	mv $(TARGETS) $(BINDIR)

objs:   $(OBJS)



# Targets

#all: $(TARGETS)

c_progs:    $(C_TASKS)
spp_progs:  $(SPP_TASKS)
f77_progs:  $(F77_TASKS)

distclean:
	make clean
	/bin/rm -rf *.fits *.xml

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
#  LIBVOAPPS dependency libraries.
####################################

lib: objs suplib
	ar rv lib$(NAME).a $(OBJS)
	$(RM) $(OBJS)

suplib:
	(cd lib ; make all)


###############################################################################
# Unit test programs to be built.
###############################################################################

demo:	$(TARGETS)

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)


###########################
#  C Test programs
###########################

#  Note:  VOSESSION has its own main()
vosession:  vosession.o lib
	$(CC) $(CFLAGS) -o vosession vosession.c $(LIBS)
	/bin/rm -rf vosession.dSYM

session_cmd:  session_cmd.o lib
	$(CC) $(CFLAGS) -o session_cmd session_cmd.c $(LIBS)
	/bin/rm -rf session_cmd.dSYM


vodata: voApps.c vodata.o lib
	$(CC) $(CFLAGS) -o vodata voApps.c $(LIBS)
	/bin/rm -rf vodata.dSYM

voregistry: voApps.c voregistry.o lib
	$(CC) $(CFLAGS) -o voregistry voApps.c $(LIBS)
	/bin/rm -rf voregistry.dSYM

vosamp:  voApps.c vosamp.o lib
	$(CC) $(CFLAGS) -o vosamp voApps.c $(LIBS)
	/bin/rm -rf vosamp.dSYM

voatlas:  voApps.c voatlas.o lib
	$(CC) $(CFLAGS) -o voatlas voApps.c $(LIBS)
	/bin/rm -rf voatlas.dSYM

voiminfo:  voApps.c voiminfo.o lib
	$(CC) $(CFLAGS) -o voiminfo voApps.c $(LIBS)
	/bin/rm -rf voiminfo.dSYM

vosloanspec:  voApps.c vosloanspec.o lib
	$(CC) $(CFLAGS) -o vosloanspec voApps.c $(LIBS)
	/bin/rm -rf vosloanspec.dSYM

votcnv:  voApps.c votcnv.o lib
	$(CC) $(CFLAGS) -o votcnv voApps.c $(LIBS)
	/bin/rm -rf votcnv.dSYM

votcat:  voApps.c votcat.o lib
	$(CC) $(CFLAGS) -o votcat voApps.c $(LIBS)
	/bin/rm -rf votcat.dSYM

votget:  voApps.c votget.o lib
	$(CC) $(CFLAGS) -o votget voApps.c $(LIBS)
	/bin/rm -rf votget.dSYM

votpos:  voApps.c votpos.o lib
	$(CC) $(CFLAGS) -o votpos voApps.c $(LIBS)
	/bin/rm -rf votpos.dSYM

votinfo:  voApps.c votinfo.o lib
	$(CC) $(CFLAGS) -o votinfo voApps.c $(LIBS)
	/bin/rm -rf votinfo.dSYM

votjoin:  voApps.c votjoin.o lib
	$(CC) $(CFLAGS) -o votjoin voApps.c $(LIBS)
	/bin/rm -rf votjoin.dSYM

votsort:  voApps.c votsort.o lib
	$(CC) $(CFLAGS) -o votsort voApps.c $(LIBS)
	/bin/rm -rf votsort.dSYM

votsplit:  voApps.c votsplit.o lib
	$(CC) $(CFLAGS) -o votsplit voApps.c $(LIBS)
	/bin/rm -rf votsplit.dSYM

votstat:  voApps.c votstat.o lib
	$(CC) $(CFLAGS) -o votstat voApps.c $(LIBS)
	/bin/rm -rf votstat.dSYM

vosesame: voApps.c vosesame.o lib
	$(CC) $(CFLAGS) -o vosesame voApps.c $(LIBS)
	/bin/rm -rf vosesame.dSYM

voimage: voApps.c voimage.o lib
	$(CC) $(CFLAGS) -o voimage voApps.c $(LIBS)
	/bin/rm -rf voimage.dSYM

vocatalog: voApps.c vocatalog.o lib
	$(CC) $(CFLAGS) -o vocatalog voApps.c $(LIBS)
	/bin/rm -rf vocatalog.dSYM

vospectra: voApps.c vospectra.o lib
	$(CC) $(CFLAGS) -o vospectra voApps.c $(LIBS)
	/bin/rm -rf vospectra.dSYM

votopic: voApps.c votopic.o lib
	$(CC) $(CFLAGS) -o votopic voApps.c $(LIBS)
	/bin/rm -rf votopic.dSYM



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
