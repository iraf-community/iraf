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
BINDIR          := ../../bin/
LIBDIR          := ../../lib/
INCDIR          := ../../include/


# secondary dependencies
LIBBASE     = ../lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep
.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC 	    = gcc
CINCS  	    = -I$(HERE) -I../ -I../../include -I../../../../include -L../../lib/


ifeq ($(PLATFORM),Darwin)
    ifeq  ($(PLMACH),x86_64)
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
    CLIBS       = -lm -lc -lcurl -lpthread
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

CLIBS           = -lm -lc -lcurl -lpthread -lm
CFLAGS          = -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./





# list of source and include files

SRCS 	    = voObj.c voSvc.c voAclist.c voDALUtil.c voFITS.c voUtil.c \
              voSCS.c voSIAP.c voSSAP.c voUtil.c voRanges.c voLog.c \
              voKML.c voXML.c voHTML.c voTask.c voParams.c vosUtil.c
OBJS 	    = voObj.o voSvc.o voAclist.o voDALUtil.o voFITS.o voUtil.o \
              voSCS.o voSIAP.o voSSAP.o voUtil.o voRanges.o voLog.o \
              voKML.o voXML.o voHTML.o voTask.o voParams.o vosUtil.o
INCS 	    = ../voApps.h ../voAppsP.h


all:    lib

install: all

objs:   $(OBJS)


# Targets

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

lib: objs
	ar rv ../lib$(NAME).a $(OBJS)
	$(RM) $(OBJS)



###############################################################################
# Unit test programs to be built.
###############################################################################

demo:	$(TARGETS)

zztest:  zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LIBS)



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
