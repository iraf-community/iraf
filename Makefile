
PREFIX=/opt/local
#PREFIX=/usr/local
#PREFIX=/opt

BINDIR=$(PREFIX)/bin

DESTDIR=
#DESTDIR=root

MACH=auto
#MACH=x86_64-linux-generic
#MACH=i386-linux-generic

########################################################################

SETUP=iraf/unix/scripts/setup.sh

default::
	cat INSTALL

boot::	boot_makefiles clean_unix boot_make

reboot::	reboot_makefiles clean_unix boot_make

boot_makefiles::
	chmod 755 $(SETUP)
	$(SETUP) boot_makefiles $(MACH) $(PREFIX)

reboot_makefiles::
	chmod 755 $(SETUP)
	$(SETUP) reboot_makefiles $(MACH) $(PREFIX)

boot_make::
	(cd iraf/unix/include ; make)
	(cd iraf/unix/config ; make)
	(cd iraf/unix/f2c/src ; make f2c)
	cp -p iraf/unix/f2c/src/f2c iraf/unix/bin/f2c.e
	(cd iraf/unix/f2c/libf2c ; make all)
	cp -p iraf/unix/f2c/libf2c/libf2c.a iraf/unix/lib/.
	(cd iraf/unix/os ; make)
	(cd iraf/unix/boot/bootlib ; make)
	(cd iraf/unix/boot/generic ; make)
	(cd iraf/unix/boot/mkpkg ; make)
	(cd iraf/unix/boot/rmbin ; make)
	(cd iraf/unix/boot/rmfiles ; make)
	(cd iraf/unix/boot/rtar ; make)
	(cd iraf/unix/boot/wtar ; make)
	(cd iraf/unix/boot/spp ; make)
	(cd iraf/unix/boot/spp/xpp ; make)
	(cd iraf/unix/boot/spp/rpp ; make)
	(cd iraf/unix/boot/xyacc ; make)
	(cd iraf/unix/gdev/sgidev ; make)

iraf::
	$(SETUP) make_iraf $(MACH)

check_iraf::
	$(SETUP) make_check_iraf $(MACH)

tables::
	$(SETUP) make_tables $(MACH)

check_tables::
	$(SETUP) make_check_tables $(MACH)

noao::
	$(SETUP) make_noao $(MACH)

check_noao::
	$(SETUP) make_check_noao $(MACH)

clean::	clean_tmp_bin clean_f2c clean_unix clean_iraf clean_tables clean_noao
	find iraf -name 'f2c_proto.h' -exec rm -f {} \;

clean_tmp_bin::
	rm -rf tmp_bin

clean_f2c::
	(cd iraf/unix/f2c/src ; make clean)
	rm -f iraf/unix/bin/f2c.e
	(cd iraf/unix/f2c/libf2c ; make clean)
	rm -f iraf/unix/lib/libf2c.a
	find iraf/unix/f2c -type l -exec rm -f {} \;

clean_unix::
	(cd iraf/unix/include ; make clean)
	(cd iraf/unix/config ; make clean)
	(cd iraf/unix/os ; make clean)
	(cd iraf/unix/boot/bootlib ; make clean)
	(cd iraf/unix/boot/generic ; make clean)
	(cd iraf/unix/boot/mkpkg ; make clean)
	(cd iraf/unix/boot/rmbin ; make clean)
	(cd iraf/unix/boot/rmfiles ; make clean)
	(cd iraf/unix/boot/rtar ; make clean)
	(cd iraf/unix/boot/wtar ; make clean)
	(cd iraf/unix/boot/spp ; make clean)
	(cd iraf/unix/boot/spp/xpp ; make clean)
	(cd iraf/unix/boot/spp/rpp ; make clean)
	(cd iraf/unix/boot/xyacc ; make clean)
	(cd iraf/unix/gdev/sgidev ; make clean)
	rm -f iraf/unix/config/mkpkg.inc
	find iraf/unix/include -type l -exec rm -f {} \;
	find iraf/unix/config -type l -exec rm -f {} \;
	find iraf/unix/os -type l -exec rm -f {} \;
	find iraf/unix/boot -type l -exec rm -f {} \;
	find iraf/unix/gdev -type l -exec rm -f {} \;

clean_iraf::
	find iraf/bin -type l -exec rm -f {} \;
	find iraf/bin -name '*.e' -exec rm -f {} \;
	find iraf/lib -type l -exec rm -f {} \;
	find iraf/lib -name '*.a' -exec rm -f {} \;
	find iraf/lib -name '*.o' -exec rm -f {} \;
	find iraf/math -type l -exec rm -f {} \;
	find iraf/math -name '*.a' -exec rm -f {} \;
	find iraf/math -name '*.e' -exec rm -f {} \;
	find iraf/math -name '*.o' -exec rm -f {} \;
	find iraf/pkg -type l -exec rm -f {} \;
	find iraf/pkg -name '*.a' -exec rm -f {} \;
	find iraf/pkg -name '*.e' -exec rm -f {} \;
	find iraf/pkg -name '*.o' -exec rm -f {} \;
	find iraf/sys -type l -exec rm -f {} \;
	find iraf/sys -name '*.a' -exec rm -f {} \;
	find iraf/sys -name '*.e' -exec rm -f {} \;
	find iraf/sys -name '*.o' -exec rm -f {} \;

clean_tables::
	find iraf/tables -type l -exec rm -f {} \;
	find iraf/tables -name '*.a' -exec rm -f {} \;
	find iraf/tables -name '*.e' -exec rm -f {} \;
	find iraf/tables -name '*.o' -exec rm -f {} \;

clean_noao::
	find iraf/noao -type l -exec rm -f {} \;
	find iraf/noao -name '*.a' -exec rm -f {} \;
	find iraf/noao -name '*.e' -exec rm -f {} \;
	find iraf/noao -name '*.o' -exec rm -f {} \;

install::
	$(SETUP) make_install $(MACH) $(PREFIX) $(BINDIR) $(DESTDIR)

install_devel::
	$(SETUP) make_install_devel $(MACH) $(PREFIX) $(BINDIR) $(DESTDIR)

