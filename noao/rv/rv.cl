#{ RV0 -- Radial Velocity Analysis Package -- Level 0

# Define the package
package		rv

# Reset min_lenuserarea to handle large 2-D images
s1 = envget ("min_lenuserarea")
if (s1 == "")
    reset min_lenuserarea = 40000
else if (int (s1) < 40000)
    reset min_lenuserarea = 40000

# Executables
task	fxcor,
	rvcorrect,
	rvidlines,
	rvreidlines	= "rv$x_rv.e"

# PSET Tasks
task	filtpars	= "rv$filtpars.par"
task	continpars 	= "rv$continpars.par"
task	keywpars	= "rv$keywpars.par"

# Hidden tasks
task	rvdebug	= 	"rv$rvdebug.par"
    hidetask ("rvdebug")

keep
clbye()
