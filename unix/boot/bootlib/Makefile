OBJS = bytmov.o oscmd.o osfdate.o osopen.o osstrpak.o oswrite.o		\
  envinit.o oscreatedir.o osfiletype.o osputenv.o osstrupk.o tape.o	\
  osaccess.o oscrfile.o osfn2vfn.o osread.o ossubdir.o vfn2osfn.o	\
  osamovb.o osdelete.o osfpathname.o ossetfmode.o ossymlink.o		\
  oschdir.o osdir.o osgetenv.o ossetowner.o ossysfile.o osclose.o	\
  osfcopy.o osgetowner.o ossettime.o ostime.o


libboot.a: $(OBJS)
	ar r $@ $?

install: libboot.a
	cp $? $(bindir)

clean:
	rm -f $(OBJS) libboot.a

.PHONY: install clean

