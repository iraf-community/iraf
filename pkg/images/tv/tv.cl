#{ TV -- Image Display Control package.

set	tv		= "images$tv/"
set	iis		= "tv$iis/"

package	tv

# Tasks.

task	_dcontrol,
	display,
	imedit,
	imexamine,
	tvmark,
	wcslab		= "tv$x_tv.e"

# Sub-packages.

task	iis.pkg		= "iis$iis.cl"

# Imexamine psets.

task    cimexam = tv$cimexam.par;    hidetask cimexam
task    eimexam = tv$eimexam.par;    hidetask eimexam
task    himexam = tv$himexam.par;    hidetask himexam
task    jimexam = tv$jimexam.par;    hidetask jimexam
task    limexam = tv$limexam.par;    hidetask limexam
task    rimexam = tv$rimexam.par;    hidetask rimexam
task    simexam = tv$simexam.par;    hidetask simexam
task    vimexam = tv$vimexam.par;    hidetask vimexam

# Wcslab psets.

task    wcspars = tv$wcspars.par;    hidetask wcspars
task     wlpars = tv$wlpars.par;     hidetask wlpars


clbye()
