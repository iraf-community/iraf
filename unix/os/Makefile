OBJS := zalloc.o zfdele.o zfiond.o zfmkcp.o zfrnam.o zglobl.o	\
    zlocva.o zpanic.o zzdbg.o dio.o zawset.o zfgcwd.o zfiopl.o		\
    zfmkdr.o zfsubd.o zgmtco.o zmain.o zraloc.o zzepro.o getproc.o	\
    zcall.o zfinfo.o zfiopr.o zfnbrk.o zfunc.o zgtenv.o zmaloc.o	\
    zshlib.o zzexit.o gmttolst.o zdojmp.o zfiobf.o zfiosf.o zfpath.o	\
    zfutim.o zgtime.o zmfree.o zsvjmp.o zzpstr.o irafpath.o zfacss.o	\
    zfioks.o zfiotx.o zfpoll.o zfxdir.o zgtpid.o zopdir.o zttyio.o	\
    zzsetk.o prwait.o zfaloc.o zfiolp.o zfioty.o zfprot.o zgcmdl.o	\
    zintpr.o zopdpr.o zwmsec.o zzstrt.o zfchdr.o zfiomt.o	\
    zflink.o zfrmdr.o zghost.o zlocpr.o zoscmd.o zxwhen.o

libos.a: $(OBJS)
	ar r $@ $?

alloc.e: alloc.o getproc.o
	$(CC) $(LDFLAGS) -o $@ alloc.o getproc.o

install: libos.a alloc.e
	cp -p $? $(bindir)

clean:
	rm -f $(OBJS) alloc.[eo] libos.a

