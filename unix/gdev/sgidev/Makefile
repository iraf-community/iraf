EXE := sgidispatch.e sgi2uimp.e sgi2uapl.e sgi2uqms.e sgi2uptx.e	\
    sgi2uhplj.e sgi2uhpgl.e sgi2ueps.e sgi2gif.e sgi2xbm.e sgi2svg.e

%.e: sgiUtil.o %.o
	$(CC) $(LDFLAGS) -o $@ $? $(LIBS)

install: $(EXE)
	cp $? $(bindir)

clean::
	rm -f *.o $(EXE) $(EXE:.e=.o)
