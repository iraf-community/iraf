#! /bin/csh
# MKSUNTOOL -- Configure the suntool subdirectory, used to link the suntools
# executable.  (Only used for SunOS versions prior to 4.0).

# set echo

unset noclobber
unalias cd cmp echo ln mv rm sed set

set OBJS = "imtool.o gterm.o gtermio.o screendump.o arrow.o notify_read.o"
set sundir = /usr/src/sun/suntool

if (! -e ./suntool) then
    mkdir suntool
endif
cd suntool

cmp -s Makefile $sundir/Makefile
if ($status == 0 && `grep gterm basetools.h` != "") then
    echo "suntool build directory is up to date"
    exit 0
else if (! -e $sundir/Makefile) then
    echo "$sundir not found"
    exit 1
else
    echo "rebuilding suntool subdirectory"
endif

set files = "`ls`"
if ("$files" != "") then
    rm -rf *
endif
(cd $sundir; tar -cf - . ) | tar -xpf -
echo '"gterm",gterm_main,' >> basetools.h
echo '"imtool",imtool_main', >> basetools.h
echo '/cmdtool_main/i\' > Temp
echo 'extern imtool_main();\' >> Temp
echo 'extern gterm_main();' >> Temp
sed -f Temp toolmerge.c > Temp2; mv -f Temp2 toolmerge.c; rm Temp
