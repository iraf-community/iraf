# Make the Sunview GTERM graphics terminal server, IMTOOL image display server.

SRCS=	arrow.c mouse.c notify_read.c screendump.c
OBJS=	arrow.o mouse.o notify_read.o screendump.o
LIBS=	-lsuntool -lsunwindow -lpixrect -lm
SUNOBJS= imtool.o gterm.o gtermio.o screendump.o arrow.o mouse.o notify_read.o

# Set the following to "-f68881" on a Sun-3, to "" on a Sun-4 or 386i.
#FLOAT=	-f68881
FLOAT=

# Make the custom suntools executable only on versions of SunOS prior to 4.0.
default:
	sh -c 'if [ "`grep Release.4 /etc/motd`" != ""\
	    -o `mach` = "i386" ]; then\
	    make gterm.e; make imtool.e;\
	else\
	    make suntools.e;\
	fi'

# Make a custom SUNTOOLS with GTERM, IMTOOL, linked in.
suntools.e: gterm.c imtool.c gtermio.o $(OBJS)
	chmod +x mksuntool.csh; mksuntool.csh
	cc -c -O $(FLOAT) gterm.c imtool.c
	mv -f gterm.o imtool.o suntool
	cp gtermio.o $(OBJS) suntool
	(cd ./suntool; make basetools MOREOBJS="$(SUNOBJS)" CPUFLAG="" ;\
	    mv -f suntools ../suntools.e)

# Make the GTERM test executable.
gterm.e: gterm.o gtermio.o $(OBJS)
	cc $(FLOAT) gterm.o gtermio.o $(OBJS) $(LIBS) -o gterm.e

# Make the IMTOOL test executable.
imtool.e: imtool.o $(OBJS)
	cc $(FLOAT) imtool.o $(OBJS) $(LIBS) -o imtool.e

# Install the executables in iraf$local, which is where the INSTALL script
# expects to find it.

install:
	strip *.e; mv -f *.e $$iraf/unix/bin.`mach`

clean:
	rm -f *.[aoe]
	rm -rf suntool

gterm.o: gterm.c gterm.h gterm.icon
	cc -c -O -DSTANDALONE $(FLOAT) gterm.c

imtool.o: imtool.c imtool.h imtool.icon imtool.cursor
	cc -c -O -DSTANDALONE $(FLOAT) imtool.c

gtermio.o: gtermio.c gterm.h gterm.esc
	cc -c -O $(FLOAT) gtermio.c

arrow.o: arrow.c
	cc -c -O $(FLOAT) arrow.c

mouse.o: mouse.c
	cc -c -O $(FLOAT) mouse.c

notify_read.o: notify_read.c
	cc -c -O $(FLOAT) notify_read.c

screendump.o: screendump.c
	cc -c -O $(FLOAT) screendump.c
