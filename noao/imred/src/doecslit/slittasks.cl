#{ ECSLITPROC tasks

task	doecslit	= "doecslit$doecslit.cl"
task	sproc		= "doecslit$sproc.cl"
task	sarcrefs	= "doecslit$sarcrefs.cl"
task	sdoarcs		= "doecslit$sdoarcs.cl"
task	sfluxcal	= "doecslit$sfluxcal.cl"
task	sbatch		= "doecslit$sbatch.cl"
task	slistonly	= "doecslit$slistonly.cl"
task	sgetspec	= "doecslit$sgetspec.cl"

task	sparams		= "doecslit$sparams.par"

task	apslitproc	= "doecslit$x_apextract.e"

hidetask sproc, sbatch, sarcrefs, sdoarcs, sfluxcal, slistonly, sgetspec
hidetask sparams, apslitproc

keep
