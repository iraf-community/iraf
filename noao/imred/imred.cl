#{ IMRED -- Image reduction package.

# Define directories.

set	argus		= "imred$argus/"
set	biasdir		= "imred$bias/"
set	ccdred		= "imred$ccdred/"
set	crutil		= "imred$crutil/"
set	ctioslit	= "imred$ctioslit/"
set	dtoi		= "imred$dtoi/"
set	echelle		= "imred$echelle/"
set	generic		= "imred$generic/"
set	hydra		= "imred$hydra/"
set	iids		= "imred$iids/"
set	irred		= "imred$irred/"
set	irs		= "imred$irs/"
set	kpnocoude	= "imred$kpnocoude/"
set	kpnoslit	= "imred$kpnoslit/"
set	quadred		= "imred$quadred/"
set	specred		= "imred$specred/"

set	apextract	= "twodspec$apextract/"
set	doecslit	= "imred$src/doecslit/"
set	dofoe		= "imred$src/dofoe/"
set	doslit		= "imred$src/doslit/"
set	srcfibers	= "imred$src/fibers/"

# Define the package.

package imred

# Tasks
#task	tutor		= "imred$tutor.cl"

# Packages
task	argus.pkg	= "argus$argus.cl"
task	bias.pkg	= "biasdir$bias.cl"
task	ccdred.pkg	= "ccdred$ccdred.cl"
task	crutil.pkg	= "crutil$crutil.cl"
task	ctioslit.pkg	= "ctioslit$ctioslit.cl"
task	dtoi.pkg	= "dtoi$dtoi.cl"
task	echelle.pkg	= "echelle$echelle.cl"
task	generic.pkg	= "generic$generic.cl"
task	hydra.pkg	= "hydra$hydra.cl"
task	iids.pkg	= "iids$iids.cl"
task	irred.pkg	= "irred$irred.cl"
task	irs.pkg		= "irs$irs.cl"
task	kpnocoude.pkg	= "kpnocoude$kpnocoude.cl"
task	kpnoslit.pkg	= "kpnoslit$kpnoslit.cl"
task	quadred.pkg	= "quadred$quadred.cl"
task	specred.pkg	= "specred$specred.cl"

clbye
