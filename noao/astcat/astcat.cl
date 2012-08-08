#{ ASTCAT.CL -- The Astronomical Catalogs and Surveys Access Package

package	astcat

# Executable Tasks

task	aclist,
	agetcat,
	aimfind,
	afiltcat,
	aslist,
	agetim,
	ahedit,
        adumpcat,
	adumpim,
	acqctest,
	acqftest,
	acqitest = "astcat$src/x_astcat.e"

# Pset Tasks

task	aregpars = "astcat$src/aregpars.par"
task	acatpars = "astcat$src/acatpars.par"
task	afiltpars = "astcat$src/afiltpars.par"
task	awcspars = "astcat$src/awcspars.par"
task	aimpars = "astcat$src/aimpars.par"

# CL tasks

task	asttest = "astcat$src/asttest.cl"

# Hidden tasks

hidetask acqctest, acqftest, acqitest

keep

clbye()
