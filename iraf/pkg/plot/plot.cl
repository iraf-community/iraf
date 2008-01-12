#{ Plot package -- vector graphics utilities.

package	plot

task	implot,
	gdevices,
	graph,
	prow,
	prows,
	pcol,
	pcols,
	phistogram,
	pradprof,
	pvector,
	gkidir,
	gkimosaic,
	gkiextract.tb,
	crtpict		= "plot$x_plot.e"

task	contour,
	surface,
	hafton,
	velvect		= "plot$x_ncar.e"

# Graphics Kernels.

task	stdgraph,
	gkidecode,
	$showcap	= "plot$x_stdgraph.e"

task	stdplot,
	sgikern,
	sgidecode	= "plot$x_sgikern.e"

task	imdkern		= "plot$x_imdkern.e"

task	nsppkern	= "plot$x_nsppkern.e"
task	calcomp		= "plot$x_calcomp.e"


clbye()
