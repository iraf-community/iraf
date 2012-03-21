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
CC              = gcc
CINCS           = -I$(INCDIR)  -I.

ifeq ($(PLATFORM), "Darwin")
    ifeq  ($(PLMACH), "x86_64")
        CARCH   = -m64 -mmacosx-version-min=10.5
    else
        CARCH   = -arch i386 -arch ppc -m32 -mmacosx-version-min=10.4
    endif
else
    CLIBS       = -lm -lc -lcurl -lpthread
    CARCH       =
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

C_SRCS 	    = votcopy.c votget.c votinfo.c vosesame.c vodata.c vodirectory.c \
	      voObj.c voSvc.c voAclist.c voDALUtil.c voUtil.c \
              voSCS.c voSIAP.c voSSAP.c voUtil.c voRanges.c \
              voKML.c voXML.c voHTML.c voApps_spp.c # voInv.c voParse.c
C_OBJS 	    = votcopy.o votget.o votinfo.o vosesame.o vodata.o vodirectory.o \
	      voObj.o voSvc.o voAclist.o voDALUtil.o voUtil.o \
              voSCS.o voSIAP.o voSSAP.o voUtil.o voRanges.o \
              voKML.o voXML.o voHTML.o voApps_spp.o # voInv.o voParse.o
C_INCS 	    = voApps.h voAppsP.h



F77_SRCS    =
F77_OBJS    =
F77_INCS    =  

SPP_SRCS    =
SPP_OBJS    =
SPP_INCS    = votParse_spp.h

SPP_TASKS   =
F77_TASKS   =
C_TASKS	    = vodata vodirectory votcopy votget votinfo vosesame
	      
TARGETS	    = $(F77_TASKS) $(SPP_TASKS) $(C_TASKS)

SRCS	    = $(C_SRCS) $(F77_SRCS) $(SPP_SRCS) 
OBJS	    = $(C_OBJS) $(F77_OBJS) $(SPP_OBJS) 
HOST_LIBS   = -lcurl -lcfitsio -lpthread -lm
LIBS        = lib$(NAME).a -lVOTable -lVOClient -lsamp $(HOST_LIBS)



all:    .BASE lib

World:  .BASE lib

install: all
	cp lib$(NAME).a $(LIBDIR)
	mv $(TARGETS) $(BINDIR)

objs:   $(OBJS)



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



####################################
#  LIBVOAPPS dependency libraries.
####################################

.BASE:
	touch .BASE

lib: objs
	ar rv lib$(NAME).a $(OBJS)



###############################################################################
# Unit test programs to be built.
###############################################################################

demo:	$(TARGETS)

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)


###########################
#  C Test programs
###########################

vodata: $(DATA_OBJS) $(INCS)
	$(CC) $(CFLAGS) -o vodata voApps.c $(LIBS) -lsamp

vodirectory: $(REG_OBJS) $(INCS)
	$(CC) $(CFLAGS) -o vodirectory voApps.c $(LIBS)

votcopy:  voApps.c votcopy.c
	$(CC) $(CFLAGS) -o votcopy voApps.c $(LIBS)

votget:  voApps.c votget.c
	$(CC) $(CFLAGS) -o votget voApps.c $(LIBS) -lcurl

votinfo:  voApps.c votinfo.c
	$(CC) $(CFLAGS) -o votinfo voApps.c $(LIBS)

vosesame:  voApps.c vosesame.c
	$(CC) $(CFLAGS) -o vosesame voApps.c $(LIBS)




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
