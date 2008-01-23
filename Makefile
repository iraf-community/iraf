
PREFIX=/opt/local
BINDIR=/opt/local/bin

#PREFIX=/usr/local
#BINDIR=/usr/local/bin

#PREFIX=/opt
#BINDIR=/opt/bin

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
	make -C iraf/unix/include
	make -C iraf/unix/config
	make -C iraf/unix/f2c/src f2c
	cp -p iraf/unix/f2c/src/f2c iraf/unix/bin/f2c.e
	make -C iraf/unix/f2c/libf2c all
	cp -p iraf/unix/f2c/libf2c/libf2c.a iraf/unix/lib/.
	make -C iraf/unix/os
	make -C iraf/unix/boot/bootlib
	make -C iraf/unix/boot/generic
	make -C iraf/unix/boot/mkpkg
	make -C iraf/unix/boot/rmbin
	make -C iraf/unix/boot/rmfiles
	make -C iraf/unix/boot/rtar
	make -C iraf/unix/boot/wtar
	make -C iraf/unix/boot/spp
	make -C iraf/unix/boot/spp/xpp
	make -C iraf/unix/boot/spp/rpp
	make -C iraf/unix/boot/xyacc
	make -C iraf/unix/gdev/sgidev

iraf::
	$(SETUP) make_iraf $(MACH)

check_iraf::
	$(SETUP) make_check_iraf $(MACH)

tables::
	$(SETUP) make_tables $(MACH)

noao::
	$(SETUP) make_noao $(MACH)

clean::	clean_tmp_bin clean_f2c clean_unix clean_iraf clean_tables clean_noao
	find iraf -name 'f2c_proto.h' -exec rm -f {} \;

clean_tmp_bin::
	rm -rf tmp_bin

clean_f2c::
	make -C iraf/unix/f2c/src clean
	rm -f iraf/unix/bin/f2c.e
	make -C iraf/unix/f2c/libf2c clean
	rm -f iraf/unix/lib/libf2c.a
	find iraf/unix/f2c -type l -exec rm -f {} \;

clean_unix::
	make -C iraf/unix/include clean
	make -C iraf/unix/config clean
	make -C iraf/unix/os clean
	make -C iraf/unix/boot/bootlib clean
	make -C iraf/unix/boot/generic clean
	make -C iraf/unix/boot/mkpkg clean
	make -C iraf/unix/boot/rmbin clean
	make -C iraf/unix/boot/rmfiles clean
	make -C iraf/unix/boot/rtar clean
	make -C iraf/unix/boot/wtar clean
	make -C iraf/unix/boot/spp clean
	make -C iraf/unix/boot/spp/xpp clean
	make -C iraf/unix/boot/spp/rpp clean
	make -C iraf/unix/boot/xyacc clean
	make -C iraf/unix/gdev/sgidev clean
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

