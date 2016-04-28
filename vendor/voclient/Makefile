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
PLBITS  	:= $(shell ./getarch -nbits)  # UR
HERE      	:= $(shell /bin/pwd)
#BINDIR    	:= ./bin/
#LIBDIR    	:= ./lib/
#INCDIR    	:= ./include/
BINDIR    	:= ../../bin/
LIBDIR    	:= ../../lib/
INCDIR    	:= ../../lib/
URCFLAGS	:= $(CFLAGS)


# includes, flags and libraries
CC 	  	= gcc
CINCS  	  	= -I$(INCDIR) -I./
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
LIBS		= -lm -lc -lpthread


all:
	(cd common      ; make all  ; make install)
	(cd libvotable  ; make all  ; make install)
	(cd libsamp     ; make all  ; make install)
	(cd libvoclient ; make all  ; make install)
	(cd voapps      ; make all  ; make install)
	(cd libvo       ; make all  ; make install)

libs:
	(cd common      ; make libs)
	(cd libvotable  ; make libs)
	(cd libsamp     ; make libs)
	(cd libvoclient ; make libs)
	(cd voapps      ; make libs)
	(cd libvo       ; make libs)

apps:
	(cd common      ; make apps)
	(cd libvotable  ; make apps)
	(cd libsamp     ; make apps)
	(cd libvoclient ; make apps)
	(cd voapps      ; make apps)

examples:
	(cd common      ; make examples)
	(cd libvotable  ; make examples)
	(cd libsamp     ; make examples)
	(cd libvoclient ; make examples)
	(cd voapps      ; make examples)

install:
	(cd common      ; make install)
	(cd libvotable  ; make install)
	(cd libsamp     ; make install)
	(cd libvoclient ; make install)
	(cd voapps      ; make install)
	(cd libvo       ; make install)

clean:
	(cd common      ; make clean)
	(cd libvotable  ; make clean)
	(cd libsamp     ; make clean)
	(cd libvoclient ; make clean)
	(cd voapps      ; make clean)
	(cd libvo       ; make clean)
	/bin/rm -rf voclient/lib/libvoclient.*
	/bin/rm -rf bin/* lib/* include/* spool



###############################################################################
# Leave this stuff alone.
###############################################################################

$(STATICLIB): $(SRCS:%.c=Static/%.o)
	/usr/bin/ar rv $@ $?
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
