
.c.o:	; $(CC) $(HSI_CF) -c $*.c

all:	sgidispatch.e sgi2uimp.e sgi2uapl.e sgi2uqms.e sgi2uptx.e sgi2uhplj.e \
	sgi2uhpgl.e sgi2ueps.e sgi2gif.e sgi2xbm.e

sgidispatch.e:	sgidispatch.o
	$(CC) $(HSI_LF) -o $@ sgidispatch.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uimp.e:	sgi2uimp.o
	$(CC) $(HSI_LF) -o $@ sgi2uimp.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uapl.e:	sgi2uapl.o
	$(CC) $(HSI_LF) -o $@ sgi2uapl.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uqms.e:	sgi2uqms.o
	$(CC) $(HSI_LF) -o $@ sgi2uqms.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uptx.e:	sgi2uptx.o
	$(CC) $(HSI_LF) -o $@ sgi2uptx.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uhplj.e:	sgi2uhplj.o
	$(CC) $(HSI_LF) -o $@ sgi2uhplj.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2uhpgl.e:	sgi2uhpgl.o
	$(CC) $(HSI_LF) -o $@ sgi2uhpgl.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2ueps.e:	sgi2ueps.o
	$(CC) $(HSI_LF) -o $@ sgi2ueps.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2gif.e:	sgi2gif.o
	$(CC) $(HSI_LF) -o $@ sgi2gif.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

sgi2xbm.e:	sgi2xbm.o
	$(CC) $(HSI_LF) -o $@ sgi2xbm.o $(HSI_LIBS)
	cp -p $@ ../../bin/.

clean::
	rm -f *.o *.e ../../bin/sgidispatch.e ../../bin/sgi2uimp.e \
		../../bin/sgi2uapl.e ../../bin/sgi2uqms.e \
		../../bin/sgi2uptx.e ../../bin/sgi2uhplj.e \
		../../bin/sgi2uhpgl.e ../../bin/sgi2ueps.e \
		../../bin/sgi2gif.e ../../bin/sgi2xbm.e
