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
HERE      	:= $(shell /bin/pwd)
BINDIR    	:= ../bin/
LIBDIR    	:= ../lib/
INCDIR    	:= ../include/


# includes, flags and libraries
CC 	  	= gcc
CINCS  	  	= -I$(INCDIR) -I./
ifeq ($(PLATFORM),Darwin)
    ifeq  ($(PLATFORM),x86_64)
        CARCH	= -m64 -mmacosx-version-min=10.5
    else
        CARCH	= -mmacosx-version-min=10.5
    endif
    LIBS	= -lm -lc
else
    CARCH	= 
    LIBS	= -lm -lc -lpthread
endif

CFLAGS 		= -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -L./


all::	expat cfitsio curl

libs::
	(./mklibs)
	(cd expat ; ./configure --prefix=${HERE}/../; \ make ; make installlib)

curl::
	(./mklibs)

expat::
	(cd expat ; ./configure --prefix=${HERE}/../; \ make ; make installlib)

cfitsio::
	(cd cfitsio ; ./mklibs)

apps:

install:
	(if [ "${PLATFORM}" != "Darwin" ]; then (cd curl ; make install); fi)
	(cd expat ; make install)
	(/bin/rm -rf ../man ../share)

clean:
	(if [ "${PLATFORM}" != "Darwin" ]; then (cd curl ; make clean); fi)
	(cd expat     	; make clean)
	(cd cfitsio   	; make clean)

distclean:
	(if [ "${PLATFORM}" != "Darwin" ]; then (cd curl ; make distclean); fi)
	(cd expat     	; make distclean)
	#(cd cfitsio   	; make distclean)



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
