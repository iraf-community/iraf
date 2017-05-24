#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the SAMP Interface
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       	 = samp
VERSION    	 = 1.0
PLATFORM  	:= $(shell uname -s)
#PLMACH  	:= $(shell uname -m)
PLMACH  	:= $(shell ../getarch)
HERE      	:= $(shell /bin/pwd)
BINDIR    	:= ../bin/
LIBDIR    	:= ../lib/
INCDIR    	:= ../include/


# secondary dependencies

LIBBASE		= lib$(NAME)
STATICLIB 	= $(LIBDIR)/$(LIBBASE).a
SHAREDLIB 	= $(LIBDIR)/$(LIBBASE).so.$(VERSION)


# includes, flags and libraries
CC              = gcc
CINCS           = -I$(INCDIR)  -I.

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
    CLIBS       = -lm -lc -lpthread -lcurl
    ifdef IRAFARCH
        ifeq ($(IRAFARCH),linux)
            CARCH = -m32
        endif
    endif
endif

CLIBS		= -lm -lc -lcurl -lpthread
CFLAGS 		= -g -Wall $(CARCH) -D$(PLATFORM) $(CINCS) -DIRAF -L./



# list of source and include files
SRCS 		= samp.c sampHub.c sampCommands.c sampHandlers.c \
		  sampClient.c sampMsg.c sampParam.c sampMTypes.c \
                  sampMethods.c sampList.c sampLog.c sampMap.c sampUtil.c
OBJS 		= samp.o sampHub.o sampCommands.o sampHandlers.o \
		  sampClient.o sampMsg.o sampParam.o sampMTypes.o \
                  sampMethods.o sampList.o sampLog.o sampMap.o sampUtil.o
INCS 		= samp.h sampDecl.h
LIBS		= lib$(NAME).a $(CLIBS)

APPS		= 


all:	.BASE lib install

World:	.BASE lib apps install

install: .BASE lib apps
	mkdir -p $(LIBDIR) $(INCDIR)
	cp lib$(NAME).a $(LIBDIR)
	cp $(INCS) $(INCDIR)
	/bin/rm -rf *.o

objs:	$(OBJS) $(INCS)

clean:
	/bin/rm -f *.o *.a *.e *.so .BASE $(APPS) __*
	(cd examples ; make clean)
	(cd libxrpc  ; make clean)
	/bin/rm -rf libxrpc/lib/build/* libxrpc/lib/*.dylib
	/bin/rm -f SWIG* libvot* php_libvot.h
	/bin/rm -f sampJava.[co]
	/bin/rm -f sampPerl.[co]
	/bin/rm -f sampPython.[co]
	/bin/rm -f sampTcl.[co]
	/bin/rm -f sampPhp.[co]
	/bin/rm -f sampCSharp.[co]
	/bin/rm -f sampRuby.[co]



####################################
#  LIBSAMP Apps
####################################

apps: $(APPS) $(INCS)

zztest: zztest.o $(OBJS) lib
	$(CC) $(CFLAGS) -o zztest zztest.o $(SAMP_OBJS) $(LFLAGS) $(LIBS)



####################################
#  LIBSAMP dependency libraries.
####################################

.BASE:
	(cd libxrpc ; ./mklibs ; make install)
	touch .BASE

lib: objs $(INCS)
	ar r lib$(NAME).a $(OBJS) 2>&1 /dev/null



###############################################################################
#  SWIG Wrapper code generation
###############################################################################

wrap_code: tcl_wrap py_wrap perl_wrap java_wrap # php_wrap ruby_wrap mono_wrap


tcl_wrap:  samp.i
	(swig -tcl    -o sampTcl.c    samp.i     )
	make tcl_lib

py_wrap:  samp.i
	(swig -python -o sampPython.c samp.i     )
	make python_lib

perl_wrap:  samp.i
	(swig -perl   -o sampPerl.c   samp.i     )
	make perl_lib

java_wrap:  samp.i
	(swig -java   -o sampJava.c   samp.i     )
	make java_lib

php_wrap:  samp.i
	(swig -php    -o sampPhp.c    samp.i     )
	make php_lib

ruby_wrap:  samp.i
	(swig -ruby   -o sampRuby.c   samp.i     )
	#make ruby_lib

mono_wrap:  samp.i
	(swig -csharp -o sampCSharp.c samp.i     )
	#make mono_lib



##############################################################################
#
#  SWIG Wrapper share library builds -- highly platform dependent!!
#
##############################################################################

wrap_libs: wrap_code tcl_lib python_lib perl_lib java_lib 

tcl_lib:  samp.i
	$(CC) -fPIC -c $(SRCS) sampTcl.c
	$(CC) -shared $(OBJS) sampTcl.o -o lib$(NAME).so
	$(RM) samp_wrap.*


PY_INCS = -I.   -I/usr/include/python2.2 -I/usr/include/python2.3 \
                -I/usr/include/python2.4 -I/usr/include/python2.5

python_lib:  samp.i
	$(CC) -w -c $(PY_INCS) $(SRCS) sampPython.c
	ld -shared $(OBJS) sampPython.o -o _lib$(NAME).so
	$(RM) samp_wrap.*

perl_lib:  samp.i
	gcc -c $(SRCS) sampPerl.c `perl -MExtUtils::Embed -e ccopts`
	ld -G $(OBJS) -o lib$(NAME).so
	$(RM) samp_wrap.*


JWRAP_FLAGS 	= -I/usr/java/j2sdk/include/ -I/usr/java/j2sdk/include/linux

java_lib:  samp.i
	gcc -fpic -c $(WRAP_SRCS) sampJava.c $(JWRAP_FLAGS)
	gcc -shared $(WRAP_OBJS) sampJava.o -o lib$(NAME).so
	$(RM) samp_wrap.*



PHP_INCS = -I. -I/usr/include/php -I/usr/local/include/php \
	   -I/usr/local/php -I/usr/include/php/Zend \
	   -I/usr/local/include/php/Zend

php_lib:   samp.i
	gcc -fpic -c $(PHP_INCS) $(WRAP_SRCS) sampPhp.c
	gcc -shared $(WRAP_OBJS) sampPhp.o -o lib$(NAME).so
	$(RM) samp_wrap.*





###############################################################################
# Leave this stuff alone.
###############################################################################

$(STATICLIB): $(SRCS:%.c=Static/%.o)
	/usr/bin/ar r $@ $?
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
