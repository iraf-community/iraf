#///////////////////////////////////////////////////////////////////////////////
#//
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME 	  	= samp
VERSION   	= 1.0
PLATFORM  	:= $(shell uname -s)
HERE      	:= $(shell /bin/pwd)
APIDIR    	:= $(HERE)/src


# secondary dependencies

LIBBASE 	= lib$(NAME)
STATICLIB 	= $(HERE)/$(LIBBASE).a
SHAREDLIB 	= $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep

.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC 		= gcc
CINCS  		= -I$(HERE) -I../include -I../include/psock
CFLAGS 		= -g -D$(PLATFORM) -m32 -Wall $(CINCS)
LFLAGS		= -L. -L../lib

COMMON_LIBS	= -lpthread -lm
XRPC_LIBS 	= -lxrpc


# list of source and include files
SRCS 	=
OBJS 	=
INCS 	=

# targets

all: xrpc ps1 ps2 ps3 ps4 ps5 ps6 ps7

clean:
	/bin/rm -rf xrpc
	/bin/rm -rf ps[1-7]
	/bin/rm -rf *.o *.a *.e

everything:
	make clean
	make all
	make install

install: all 


# Unit test programs to be built.

zztest: zztest.c
	$(CC) $(CFLAGS) -o zztest zztest.c $(LFLAGS) $(COMMON_LIBS) $(XRPC_LIBS)




#=======================
# leave this stuff alone
#=======================

%.o: %.c $(INCS)
	$(CC) -Wall $(CINCS) $(CFLAGS) -c $< -o $@
