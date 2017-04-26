#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the libVOTable example tasks.
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	= VOTable
VERSION    	= 1.0
PLATFORM       := $(shell uname -s)
#PLATFORM  	= LINUX
#PLATFORM  	= MACOSX
#PLATFORM  	= CYGWIN
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
CINCS  	    = -I$(HERE) -I../ -I../include -L../ -L../../lib/ -L../
CFLAGS 	    = -g -Wall -D$(PLATFORM) $(CINCS)

#F77 	    = g77
F77 	    = gfortran -m64
FFLAGS 	    = -g -Wall



# list of source and include files

C_SRCS 	    = votconcat.c votcompress.c votdump.c votget.c \
		votinfo.c votcopy.c
C_OBJS 	    = votconcat.o votcompress.o votdump.o votget.o \
		votinfo.o votcopy.o
C_INCS 	    =  

F77_SRCS    = votpos_f77.f votdump_f77.f
F77_OBJS    = votpos_f77.o votdump_f77.o
F77_INCS    =  

SPP_SRCS    = votget_spp.x votinfo_spp.x
SPP_OBJS    = votget_spp.o votinfo_spp.o
SPP_INCS    = votParse_spp.h


LIBS 	    = -lVOTable -lcfitsio -lcurl -lm

SPP_TASKS   = votget_spp votinfo_spp
F77_TASKS   = votpos_f77 votdump_f77
C_TASKS	    = votcompress votcopy votdump votget votinfo votconcat votpos
	      
TARGETS	    = $(C_TASKS) # $(F77_TASKS) $(SPP_TASKS)


# Targets

all: c_progs

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

zz:  zz.c
	$(CC) $(CFLAGS) -I/usr/local/include -o zz zz.c $(LIBS) -lcfitsio

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)

votcompress:  votcompress.c
	$(CC) $(CFLAGS) -o votcompress votcompress.c $(LIBS)

votconcat:  votconcat.c
	$(CC) $(CFLAGS) -o votconcat votconcat.c $(LIBS)

votcopy:  votcopy.c
	$(CC) $(CFLAGS) -o votcopy votcopy.c $(LIBS)

votdump:  votdump.c
	$(CC) $(CFLAGS) -o votdump votdump.c $(LIBS)

votget:  votget.c
	$(CC) $(CFLAGS) -o votget votget.c $(LIBS) -lcurl

votinfo:  votinfo.c
	$(CC) $(CFLAGS) -o votinfo votinfo.c $(LIBS)

votpos:  votpos.c
	$(CC) $(CFLAGS) -o votpos votpos.c $(LIBS)



###########################
#  SPP Test programs
###########################

votget_spp:  votget_spp.x
	xc -/I../ -/L../ votget_spp.x -lVOTable

votinfo_spp:  votinfo_spp.x
	xc -/I../ -/L../ votinfo_spp.x -lVOTable



###########################
#  Fortran Test programs.
###########################

votpos_f77:  votpos_f77.f
	$(F77) $(FFLAGS) -o votpos_f77 votpos_f77.f ../libVOTable.a -lc

votdump_f77:  votdump_f77.f
	$(F77) $(FFLAGS) -o votdump_f77 votdump_f77.f -L../ -lVOTable -lm -lc




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
