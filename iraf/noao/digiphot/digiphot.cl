#{ DIGIPHOT -- Digital photometric reduction package.

set	apphot		= "digiphot$apphot/"
set	daophot		= "digiphot$daophot/"
set	photcal		= "digiphot$photcal/"
set	ptools		= "digiphot$ptools/"

package	digiphot

task	apphot.pkg	= apphot$apphot.cl
task	daophot.pkg	= daophot$daophot.cl
task	photcal.pkg	= photcal$photcal.cl
task	ptools.pkg	= ptools$ptools.cl

clbye()
