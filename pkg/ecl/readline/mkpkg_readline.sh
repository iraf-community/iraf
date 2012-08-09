#!/bin/sh
#
# a shell script to run the readline compile in a way that does not
# clutter up the stdout.
#
# the readline build is able to return a proper exit code, so we don't
# necessarily need to see all the output in our regular log.  Since mkpkg
# does NOT normally return a reliable exit code, we grep for "unusual"
# patterns in the output.  This readline script will only make one line
# of output, 

echo READLINE:

st=0

case "$IRAFARCH"
in
macosx)
	./configure --disable-dynamic --enable-static CFLAGS="-arch i386" > spool.config 2>&1
	;;

macintel)
	./configure --disable-dynamic --enable-static > spool.config 2>&1
	;;

linux)
	./configure --disable-dynamic --enable-static CFLAGS="-m32" > spool.config 2>&1
	;;

freebsd)
	./configure --disable-dynamic --enable-static CFLAGS="-m32" > spool.config 2>&1
	;;

*)
	./configure --disable-dynamic --enable-static > spool.config 2>&1
	;;
esac
st=$st$?

make libreadline.a > spool.make 2>&1
st=$st$?

cp libreadline.a ../ 
st=$st$?

make clean > spool.make_clean 2>&1
st=$st$?

st=`echo $st | tr -d 0`

if [ "$st" != "" ]
then
	echo ERROR BUILDING READLINE `pwd`/mkpkg_readline.sh
fi

exit $st

