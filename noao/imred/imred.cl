#{ IMRED -- Image reduction package.

# Define directories.

set	bias		= "imred$bias/"
set	ccdred		= "imred$ccdred/"
set	coude		= "imred$coude/"
set	dtoi		= "imred$dtoi/"
set	echelle		= "imred$echelle/"
set	generic		= "imred$generic/"
set	iids		= "imred$iids/"
set	irred		= "imred$irred/"
set	irs		= "imred$irs/"
set	msred		= "imred$msred/"
set	specphot	= "imred$specphot/"
set	vtel		= "imred$vtel/"

# Define the package.

package imred

	# Tasks
	task	observatory	= imred$observatory.cl
	task	setairmass	= astutil$x_astutil.e
#	task	tutor		= imred$tutor.cl

	# Packages
	task	bias.pkg	= bias$bias.cl
	task	ccdred.pkg	= ccdred$ccdred.cl
	task	coude.pkg	= coude$coude.cl
	task	dtoi.pkg	= dtoi$dtoi.cl
	task	echelle.pkg	= echelle$echelle.cl
	task	generic.pkg	= generic$generic.cl
	task	iids.pkg	= iids$iids.cl
	task	irred.pkg	= irred$irred.cl
	task	irs.pkg		= irs$irs.cl
	task	msred.pkg	= msred$msred.cl
	task	specphot.pkg	= specphot$specphot.cl
	task	vtel.pkg	= vtel$vtel.cl

clbye
