#{ tbplot package -- TABLES graphics utilities.

package	tbplot

task	igi,
	sgraph = "tbplot$x_tbplot.e"


# Plot parameters pset for sgraph
task	pltpar		= "tbplot$pltpar.par"
 
# Axis attributes parameters pset
task    axispar         = "tbplot$axispar.par"
 
# Device parameters pset (device name, append, and viewport)
task	dvpar		= "tbplot$dvpar.par"

clbye()

