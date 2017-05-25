#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the VOTable Interface
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	 = VOClient
VERSION    	 = 1.0
PLATFORM  	:= $(shell uname -s)
PLMACH  	:= $(shell uname -m)
HERE      	:= $(shell /bin/pwd)
#BINDIR    	:= ./bin/
#LIBDIR    	:= ./lib/
#INCDIR    	:= ./include/
BINDIR    	:= ../../bin/
LIBDIR    	:= ../../lib/
INCDIR    	:= ../../lib/


# includes, flags and libraries
CC 	  	= gcc
CINCS  	  	= -I$(INCDIR) -I./
ifeq  ($(PLATFORM), "Darwin")
    ifeq  ($(PLATFORM), "x86_64")
        CARCH	= -m64 -mmacosx-version-min=10.5
    else
        CARCH	= -arch i386 -arch ppc -m32 -mmacosx-version-min=10.5
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
	(cd libsamp     ; make all  ; make install)
	(cd libvotable  ; make all  ; make install)
	(cd libvoclient ; make all  ; make install)
	(cd voapps      ; make all  ; make install)
	(cd libvo       ; make all  ; make install)
	cp voclient.jar voclientd bin/
	/bin/rm -rf lib/pkgconfig
	/bin/rm -rf lib/*.dylib lib/*.la lib/*.so

libs:
	(cd libvotable  ; make libs)
	(cd libsamp     ; make libs)
	(cd libvoclient ; make libs)
	(cd voapps      ; make libs)
	(cd libvo       ; make libs)

apps:
	(cd libvotable  ; make apps)
	(cd libsamp     ; make apps)
	(cd libvoclient ; make apps)
	(cd voapps      ; make apps)

examples:
	(cd libvotable  ; make examples)
	(cd libsamp     ; make examples)
	(cd libvoclient ; make examples)
	(cd voapps      ; make examples)

install:
	(cd libvotable  ; make install)
	(cd libsamp     ; make install)
	(cd libvoclient ; make install)
	(cd voapps      ; make install)
	(cd libvo       ; make install)
	cp voclient.jar voclientd bin/
	/bin/rm -rf lib/pkgconfig
	/bin/rm -rf lib/*.dylib lib/*.la lib/*.so

clean:
	(cd libvotable  ; make clean)
	(cd libsamp     ; make clean)
	(cd libvoclient ; make clean)
	(cd voapps      ; make clean)
	(cd libvo       ; make clean)
	/bin/rm -rf voclient/lib/libvoclient.*
	/bin/rm -rf bin/* lib/* include/* *spool* */*spool* */*/*spool*
	/bin/rm -rf */config.log */*/config.log */*/*/config.log
	/bin/rm -rf */*.dylib



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
