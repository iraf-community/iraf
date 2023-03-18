OBJS := cant.o close.o endst.o getarg.o getlin.o initst.o open.o	\
    putch.o putlin.o r4tocstr.o remark.o

libc.a: $(OBJS)
	ar r $@ $?

clean:
	rm -f $(OBJS) libc.a

.PHONY: clean
