#///////////////////////////////////////////////////////////////////////////////
#//
#//  Makefile for the VOClient C API procedures
#//
#//  M. Fitzpatrick, NOAO, June 2006		initial version
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       =  VOClient
VERSION    = 1.0
PLATFORM  := $(shell uname -s)
#PLATFORM  = LINUX
#PLATFORM  = MACOSX
#PLATFORM  = CYGWIN
HERE      := $(shell /bin/pwd)


# secondary dependencies

LIBBASE    = lib$(NAME)
STATICLIB  = $(HERE)/$(LIBBASE).a
SHAREDLIB  = $(HERE)/$(LIBBASE).so


# stuff that's precious to keep

.PRECIOUS:	$(STATICLIB) $(SHAREDLIB)
.KEEP_STATE:


# includes, flags and libraries
CC        = gcc
CINCS     = -I$(HERE)
ifeq ($(PLATFORM),Darwin)
    ifeq ($(PLMACH),x86_64)
        CARCH   = -m64 -mmacosx-version-min=10.5
    else
        CARCH   = -arch i386 -m32 -mmacosx-version-min=10.5
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

#CFLAGS   = -O2 -DDEBUG -D$(PLATFORM)
CFLAGS    = -g -Wall -D$(PLATFORM) $(CARCH)



# list of source and include files

SRCS      = vocDAL.c vocDAL_f77.c vocDAL_spp.c \
       	    vocLib.c vocLib_f77.c vocLib_spp.c \
       	    vocRegistry.c vocRegistry_f77.c vocRegistry_spp.c \
       	    vocSesame.c vocSesame_f77.c vocSesame_spp.c \
       	    vocSkybot.c vocSkybot_f77.c vocSkybot_spp.c \
       	    vocUtil_f77.c vocUtil_spp.c vocMsg.c 
OBJS      = vocDAL.o vocDAL_f77.o vocDAL_spp.o \
       	    vocLib.o vocLib_f77.o vocLib_spp.o \
       	    vocRegistry.o vocRegistry_f77.o vocRegistry_spp.o \
       	    vocSesame.o vocSesame_f77.o vocSesame_spp.o \
       	    vocSkybot.o vocSkybot_f77.o vocSkybot_spp.o \
       	    vocUtil_f77.o vocUtil_spp.o vocMsg.o 
INCS      = VOClient.h

WRAP_SRCS = vocDAL.c vocLib.c vocMsg.c vocSesame.c vocSkybot.c vocRegistry.c
WRAP_OBJS = vocDAL.o vocLib.o vocMsg.o vocSesame.o vocSkybot.o vocRegistry.o




# targets

all: Static Shared $(STATICLIB)

all_static: Static $(STATICLIB)

all_shared: Shared $(SHAREDLIB)

World:
	make clean
	make all
	make wrapper_code
	make wrapper_libs
	make install

clean:
	/bin/rm -rf Shared Static *.o
	/bin/rm -rf lib$(NAME).* *$(NAME).so
	/bin/rm -rf .make.state .nse_depinfo
	/bin/rm -rf dataset.* examples/dataset.*
	/bin/rm -rf SWIG* vocWrap*.c php_*.h

help: HELP

install: all 
	cp VOClient.h ../include
	cp lib$(NAME).* ../lib
	ranlib ../lib/lib$(NAME).a

install-vo-cli: all 
	cp VOClient.h ../include
	cp lib$(NAME).* ../../lib
	ranlib ../../lib/lib$(NAME).a



###############################################################################
#  SWIG Wrapper code generation
###############################################################################

wrapper_code: tcl_wrap py_wrap perl_wrap java_wrap php_wrap ruby_wrap mono_wrap

tcl_wrap:  VOClient.i
	(swig -tcl    -o vocWrapTcl.c	 VOClient.i	)
	make tcl_lib

py_wrap:  VOClient.i
	(swig -python -o vocWrapPython.c VOClient.i	)
	make python_lib

perl_wrap:  VOClient.i
	(swig -perl   -o vocWrapPerl.c	 VOClient.i	)
	make perl_lib

java_wrap:  VOClient.i
	(swig -java   -o vocWrapJava.c	 VOClient.i	)
	make java_lib

php_wrap:  VOClient.i
	(swig -php    -o vocWrapPhp.c	 VOClient.i	)
	make php_lib

ruby_wrap:  VOClient.i
	(swig -ruby   -o vocWrapRuby.c	 VOClient.i	)
	make ruby_lib

mono_wrap:  VOClient.i
	(swig -csharp -o vocWrapCSharp.c VOClient.i	)
	make mono_lib



###############################################################################
#  SWIG Wrapper share library builds -- highly platform dependent!!
###############################################################################

wrapper_libs: wrapper_code tcl_lib python_lib perl_lib java_lib 

tcl_lib:  VOClient.i
	$(CC) -fPIC -c $(SRCS) vocWrapTcl.c
	$(CC) -shared $(OBJS) vocWrapTcl.o -o $(NAME).so
	$(RM) VOClient_wrap.*


PY_INCS = -I.   -I/usr/include/python2.2 -I/usr/include/python2.3 \
		-I/usr/include/python2.4 -I/usr/include/python2.5

python_lib:  VOClient.i
	$(CC) -w -c $(PY_INCS) $(SRCS) vocWrapPython.c
	$(CC) -fPIC -shared $(OBJS) vocWrapPython.o -o _$(NAME).so \
	    -I/usr/include/python2.6/ -lpython2.6
	$(RM) VOClient_wrap.*

perl_lib:  VOClient.i
	$(CC) -c $(SRCS) vocWrapPerl.c `perl -MExtUtils::Embed -e ccopts`
	ld -G $(OBJS) -o $(NAME).so
	$(RM) VOClient_wrap.*


JWRAP_FLAGS = -I/usr/java/j2sdk/include/ -I/usr/java/j2sdk/include/linux

java_lib:  VOClient.i
	$(CC) -fpic -c $(WRAP_SRCS) vocWrapJava.c $(JWRAP_FLAGS)
	$(CC) -shared $(WRAP_OBJS) vocWrapJava.o -o lib$(NAME).so
	$(RM) VOClient_wrap.*


PHP_INCS = -I. -I/usr/include/php -I/usr/local/include/php -I/usr/local/php \
	   -I/usr/include/php/Zend -I/usr/local/include/php/Zend

php_lib:   VOClient.i
	$(CC) -fpic -c $(PHP_INCS) $(WRAP_SRCS) vocWrapPhp.c
	$(CC) -shared $(WRAP_OBJS) vocWrapPhp.o -o $(NAME).so
	$(RM) $(NAME)_wrap.*




###############################################################################
# Leave this stuff alone.
###############################################################################

$(STATICLIB): $(SRCS:%.c=Static/%.o)
	/usr/bin/ar rv $@ $?
Static/%.o: %.c $(INCS)
	$(CC) $(CINCS) $(CFLAGS) -c $< -o $@
Static:
	/bin/mkdir $@
	chmod 777 $@

$(SHAREDLIB): $(SRCS:%.c=Shared/%.o)
	/usr/bin/ld -shared -o $@ $? -lc -ldl
Shared/%.o: %.c $(INCS)
	$(CC) $(CINCS) $(CFLAGS) -fpic -shared -c $< -o $@
Shared:
	/bin/mkdir $@
	chmod 777 $@
