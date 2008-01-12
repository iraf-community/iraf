#{ RV -- Radial Velocity Analysis Package

# Define the package
package		rv

# Executables
task	fxcor,
	rvidlines,
	rvreidlines	= "rv$x_rv.e"

task	rvcorrect	= "astutil$x_astutil.e"

# PSET Tasks
task	filtpars	= "rv$filtpars.par"
task	continpars 	= "rv$continpars.par"
task	keywpars	= "rv$keywpars.par"

# Hidden tasks
task	rvdebug	= 	"rv$rvdebug.par"
    hidetask ("rvdebug")

keep
clbye()
