This directory tree started life as iraf.lnux.x86.tar.gz downloaded
from iraf.net on 2012-06-25

Before importing the source code, I removed all the object files that I found:

	bin.*/*
	*.o
	*.a	when not symlink
	*.e
	*.mip
	*.jar
	*.class

For every directory where there was a Makefile, make clean in that directory

