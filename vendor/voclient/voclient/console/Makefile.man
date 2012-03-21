#///////////////////////////////////////////////////////////////////////////////
#//
#//
#///////////////////////////////////////////////////////////////////////////////

# primary dependencies

NAME       = console
VERSION    = 1.0
PLATFORM  := $(shell uname -s)
HERE      := $(shell /bin/pwd)


# includes, flags and libraries
CC 	  = gcc
CINCS  	  = -I$(HERE) -I../include
CLIBS  	  = -I$(HERE) -I../lib
CFLAGS 	  = -g -D$(PLATFORM) $(CINCS) $(CLIBS)
LIBS	  = -lm -lc


# list of source and include files

SRCS = 	console.c
OBJS = 	console.o
INCS =  


# targets

all: 	console

clean:
	/bin/rm -rf console console.o

everything:
	make clean
	make all
	make install

help: HELP

install: all 



###############################################################################
# Unit test programs to be built.
###############################################################################


###########################
#  C Test programs.
###########################

console: console.c
	$(CC) $(CFLAGS) -o console console.c $(LIBS)




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
