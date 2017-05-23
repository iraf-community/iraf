#///////////////////////////////////////////////////////////////////////////////
#//
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME     	= xrpc
VERSION  	= 1.0
HERE    	:= $(shell /bin/pwd)
PLATFORM        := $(shell uname -s)
PLMACH          := $(shell uname -m)


# secondary dependencies

LIBBASE     = lib$(NAME)
STATICLIB   = $(HERE)/$(LIBBASE).a
SHAREDLIB   = $(HERE)/$(LIBBASE).so.$(VERSION)


# stuff that's precious to keep

.PRECIOUS:
.KEEP_STATE:


# includes, flags and libraries
CINCS   = -I./ -I./include -I../include -Ixmlrpc-c/lib/abyss/src
ifeq ($(PLATFORM),Darwin)
    ifeq ($(PLMACH),x86_64)
        CARCH   = -m64 -mmacosx-version-min=10.5
    else
        CARCH   = -arch i386 -m32 -mmacosx-version-min=10.4
    endif
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),macintel)
            CARCH = -m64 -mmacosx-version-min=10.5
        else
            CARCH = -arch i386 -m32 -mmacosx-version-min=10.5
        endif
    endif
else
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

CFLAGS  = -O2 -Wall -g $(CINCS) $(CARCH)


# list of source and include files

SRCS  = xrClient.c xrServer.c xrMethod.c xrUtil.c xrStruct.c xrArray.c
OBJS  = xrClient.o xrServer.o xrMethod.o xrUtil.o xrStruct.o xrArray.o
INCS  = xrpc.h xrpcP.h


LFLAGS 	= -L. -L./lib 
LIBS 	=


# targets

all:  xrpc

clean:
	(./mkclean)
	/bin/rm -rf Shared Static UnitTests/* *.o *.a *.e

install: xrpc
	(cp libxrpc.a ../libsamp.a)
	mkdir -p ../../include/xmlrpc-c
	cp -p $(INCS)  ../../include
	cp -p ./include/xmlrpc*.h ../../include
	cp -p ./include/xmlrpc-c/*.h ../../include/xmlrpc-c
	cp -p ./include/xmlrpc-c/*.hpp ../../include/xmlrpc-c

World:
everything:
	make clean
	make xrpc
	make base
	make install

include:

#
base:
	(./mklibs)


xrpc: $(SRCS:%.c=%.o)
	cp $(OBJS) lib/build/
	/usr/bin/ar rv libxrpc.a $?
	(cp libxrpc.a ../libsamp.a)
	mkdir -p ../../include/xmlrpc-c
	cp -p $(INCS)  ../../include
	cp -p ./include/xmlrpc*.h ../../include
	cp -p ./include/xmlrpc-c/*.h ../../include/xmlrpc-c
	cp -p ./include/xmlrpc-c/*.hpp ../../include/xmlrpc-c

%.o: %.c $(INCS)
	/usr/bin/gcc -Wall $(CINCS) $(CFLAGS) -c $< -o $@


#=======================
# leave this stuff alone
#=======================

$(STATICLIB): $(SRCS:%.c=Static/%.o)
	/usr/bin/ar rv $@ $?
Static/%.o: %.c $(INCS)
	/usr/bin/gcc $(CINCS) $(CFLAGS) -c $< -o $@
Static:
	/bin/mkdir $@
	chmod 777 $@

$(SHAREDLIB): $(SRCS:%.c=Shared/%.o)
	/usr/bin/ld -shared -o $@ $? -lc -ldl
Shared/%.o: %.c $(INCS)
	/usr/bin/gcc $(CINCS) $(CFLAGS) -fpic -shared -c $< -o $@
Shared:
	/bin/mkdir $@
	chmod 777 $@
