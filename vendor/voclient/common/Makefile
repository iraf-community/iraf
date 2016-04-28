#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the common libraries
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	 = Common
VERSION    	 = 1.0
PLATFORM  	:= $(shell uname -s)
PLMACH  	:= $(shell uname -m)
PLBITS  	:= $(shell ../getarch -nbits)  # UR
HERE      	:= $(shell /bin/pwd)
BINDIR    	:= ../bin/
LIBDIR    	:= ../lib/
INCDIR    	:= ../include/
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
    LIBS	= -lm -lc
else
    CARCH	= 
    LIBS	= -lm -lc -lpthread
endif

CFLAGS 		= $(URCFLAGS) -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./


all::	curl expat

libs::
	(/bin/csh -f mklibs)
	(cd expat ; ./configure --prefix=${HERE}/../; \ make ; make installlib)

curl::
	(/bin/csh -f mklibs)

expat::
	(cd expat ; ./configure --prefix=${HERE}/../; \ make ; make installlib)

apps:

install:
	(cd curl  ; make install)
	#(cd expat ; make install)
	(/bin/rm -rf ../man ../share)

clean:
	(cd curl  	; make clean)
	(cd expat     	; make clean)

distclean:
	(cd curl  	; make distclean)
	(cd expat     	; make distclean)



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
