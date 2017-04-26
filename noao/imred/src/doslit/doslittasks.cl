#{ SLITPROC tasks

task	doslit		= "doslit$doslit.cl"
task	sproc		= "doslit$sproc.cl"
task	sarcrefs	= "doslit$sarcrefs.cl"
task	sdoarcs		= "doslit$sdoarcs.cl"
task	sfluxcal	= "doslit$sfluxcal.cl"
task	sbatch		= "doslit$sbatch.cl"
task	slistonly	= "doslit$slistonly.cl"
task	sgetspec	= "doslit$sgetspec.cl"

task	apslitproc	= "doslit$x_apextract.e"

hidetask sproc, sbatch, sarcrefs, sdoarcs, sfluxcal, slistonly, sgetspec
hidetask apslitproc

keep
