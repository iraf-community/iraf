#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the Vendor Libraries
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	 = vendor
VERSION    	 = 1.0
PLATFORM  	:= $(shell uname -s)
PLMACH  	:= $(shell uname -m)
HERE      	:= $(shell /bin/pwd)
BINDIR    	:= ../bin/
LIBDIR    	:= ../lib/
INCDIR    	:= ../lib/


# includes, flags and libraries
CC 	  	= gcc
CINCS  	  	= -I$(INCDIR) -I./
ifeq  ($(PLATFORM), "Darwin")
    ifeq  ($(IRAFARCH), "macintel")
        CARCH	= -m64 -mmacosx-version-min=10.5
    else
        CARCH	= -arch i386 -m32 -mmacosx-version-min=10.5
    endif
else
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

CFLAGS 		= -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./
LIBS		= -lm -lc -lpthread


all:
	./mklibs

libs:

install:
	(cd cfitsio   ; make install)
	(cd voclient  ; make install)

clean:
	(cd cfitsio   ; make clean)
	(cd voclient  ; make clean)


cfitsio:
	(cd cfitsio   ; ./configure --libdir=../../bin CFLAGS="$CARCH" ; make ; make install)

voclient:
	(cd voclient  ; make World)


###############################################################################
# Leave this stuff alone.
###############################################################################

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
