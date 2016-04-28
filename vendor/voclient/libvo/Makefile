#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the VO Uber-library
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	 = VO
VERSION    	 = 1.0
PLATFORM  	:= $(shell uname -s)
PLMACH  	:= $(shell uname -m)
PLBITS  	:= $(shell ../getarch -nbits)  # UR
HERE      	:= $(shell /bin/pwd)
BINDIR    	:= ../bin/
LIBDIR    	:= ../lib/
INCDIR    	:= ../include/
URCFLAGS	:= $(CFLAGS)


# secondary dependencies

LIBBASE		= lib$(NAME)
STATICLIB 	= $(LIBDIR)/$(LIBBASE).a
SHAREDLIB 	= $(LIBDIR)/$(LIBBASE).so.$(VERSION)


# includes, flags and libraries
CC              = gcc
CINCS           = -I$(INCDIR)  -I.

CLIBS		= -lm -lc -lcurl -lpthread
ifeq  ($(PLATFORM), "Darwin")
    ifeq  ($(PLBITS), "64")  # Base on IRAFARCH for UR, rather than OS bits
        CARCH	= -mmacosx-version-min=10.5
    else
        CARCH	= -arch i386 -mmacosx-version-min=10.4
    endif
else
    CARCH	= 
endif

CFLAGS 		= $(URCFLAGS) -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./



# list of source and include files
SRCS 		= 
OBJS 		= 
INCS 		= 
LIBS		=
APPS		=


all:	lib

World:	lib

install: all
	mv lib$(NAME).a $(LIBDIR)

objs:	$(OBJS) $(INCS)

clean:
	/bin/rm -f *.o *.a *.e *.so .BASE



####################################
#  Apps
####################################

zztest: zztest.o $(OBJS)
	$(CC) $(CFLAGS) -o zztest zztest.o $(LFLAGS) $(LIBS)



####################################
#  LIBVO dependency libraries.
####################################

lib: objs $(INCS)
	ar x ../lib/libsamp.a
	ar x ../lib/libVOTable.a
	ar x ../lib/libVOClient.a
	ar x ../lib/libVOApps.a
	ar x ../lib/libcurl.a
	chmod 644 *.o
	ar rv libVO.a *.o
	/bin/rm -f *.o __*




###############################################################################
# Leave this stuff alone.
###############################################################################

$(STATICLIB): $(SRCS:%.c=Static/%.o)
	/usr/bin/ar r $@ $?
Static/%.o: %.c $(INCS)
	gcc $(CINCS) $(CFLAGS) -c $< -o $@
Static:
	/bin/mkdir $@
	chmod 777 $@

$(SHAREDLIB): $(SRCS:%.c=Shared/%.o)
	/usr/bin/ld -shared -o $@ $? -lc -ldl
Shared/%.o: %.c $(INCS)
	gcc $(CINCS) $(CFLAGS) -fpic -shared -c $< -o $@
Shared:
	/bin/mkdir $@
	chmod 777 $@
