HLIB	= ../../hlib/
IRAFLIB	= ../../../lib/
VGRIND	= csh /usr/ucb/vgrind -W

head:	xyacc 
xyacc:	y1.o y2.o y3.o y4.o
	cc -o xyacc.e y?.o 

y1.o y2.o y3.o y4.o: dextern files

install:
	mv -f xyacc.e $(HLIB)
	cp    yaccpar.x $(IRAFLIB)

clean :
	rm -f *.o 

vgrind:
	cp /dev/null index
	$(VGRIND) -h 'Yacc' dextern files y1.c y2.c y3.c y4.c
	$(VGRIND) -h 'Yacc' -x index
