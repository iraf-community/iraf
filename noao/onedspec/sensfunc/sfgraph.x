include	<gset.h>
include	<error.h>
include	<math/curfit.h>
include	"sensfunc.h"

define	NCURVE		50		# Number of vectors in curve


# SF_GRAPH -- Graph the desired data on the output graphics device.
# This entry procedure determines the graph types, sets the device viewports
# for each graph, and calls a procedure to make the graph.

procedure sf_graph (gp, stds, nstds, cv, wextn, extn, nextn, ecv)

pointer	gp			# Graphics structure
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
pointer	cv			# Sensitivity function curve
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of values in extinction table
pointer	ecv			# Residual extinction curve.

int	i, image, ngraphs, strlen()
real	fa[8]
pointer	sp, id, gio

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	# Clear the graphs, write the title, and set the viewports based on
	# the number of graphs.

	call smark (sp)
	call salloc (id, SZ_LINE, TY_CHAR)
	call sysid (Memc[id], SZ_LINE)

	gio = GP_GIO(gp)
	call gclear (gio)
	call gseti (gio, G_FACOLOR, 0)
	call gseti (gio, G_NMINOR, 0)
	ngraphs = strlen (GP_GRAPHS(gp,1))
	switch (ngraphs) {
	case 1:
	    call gseti (gio, G_WCS, 1)
	    call gsview (gio, .10, .97, .1, .9) 
	    GP_SZMARK(gp) = 2.
	    GP_SZMDEL(gp) = 2.
	case 2:
	    call gseti (gio, G_WCS, 1)
	    call gsview (gio, .10, .97, .55, .9) 
	    call gseti (gio, G_WCS, 2)
	    call gsview (gio, .10, .97, .10, .45) 
	    GP_SZMARK(gp) = 2.
	    GP_SZMDEL(gp) = 2.
	case 3:
	    call gseti (gio, G_WCS, 1)
	    call gsview (gio, .10, .97, .55, .9) 
	    call gseti (gio, G_WCS, 2)
	    call gsview (gio, .10, .50, .1, .45) 
	    call gseti (gio, G_WCS, 3)
	    call gsview (gio, .57, .97, .1, .45) 
	    GP_SZMARK(gp) = 2.
	    GP_SZMDEL(gp) = 2.
	default:
	    call gseti (gio, G_WCS, 1)
	    call gsview (gio, .10, .50, .55, .9) 
	    call gseti (gio, G_WCS, 2)
	    call gsview (gio, .57, .97, .55, .9) 
	    call gseti (gio, G_WCS, 3)
	    call gsview (gio, .10, .50, .1, .45) 
	    call gseti (gio, G_WCS, 4)
	    call gsview (gio, .57, .97, .1, .45) 
	    GP_SZMARK(gp) = .01
	    GP_SZMDEL(gp) = .01
	}

	# For each graph select the viewport and call a procedure to make
	# the graph.

	image = 0
	for (i = 1; GP_GRAPHS(gp,i) != EOS; i = i + 1) {
	    call gseti (gio, G_WCS, i)
	    if (i > 1)
		call gfill (gio, fa, fa[5], 4, GF_SOLID)
	    switch (GP_GRAPHS(gp,i)) {
	    case 'a', 's', 'r':
		call sf_data (stds, nstds, GP_GRAPHS(gp,i))
		call sf_graph1 (gp, stds, nstds, GP_GRAPHS(gp,i))
	    case 'e':
		call sf_egraph (gp, wextn, extn, nextn, ecv)
	    case 'c':
		call sf_cgraph (gp, stds, nstds, cv)
	    case 'i', 'l':
		if (GP_GRAPHS(gp,i) == 'i')
		    GP_LOG(gp) = NO
		else
		    GP_LOG(gp) = YES
		image = image + 1
		iferr (call sf_image (gp, image, stds, nstds, cv, wextn, extn,
		    nextn, ecv))
		    call erract (EA_WARN)
	    }
	}

	call gseti (gio, G_WCS, 0)
	call gtext (gio, 0.5, 1., Memc[id], "h=c,v=t,f=b")
	call gtext (gio, 0.5, 0.97, GP_TITLE(gp), "h=c,v=t,f=b")

	call sfree (sp)
end


# SF_GRAPH1 -- Make graph of sensitivity or residual sensitivity vs wavelength.

procedure sf_graph1 (gp, stds, nstds, graph)

pointer	gp			# Graphics structure
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
char	graph			# Graph type

int	i, j, n, mark, mdel, cdel, color
real	szmark, szmdel, ymin, ymax, y1, y2
pointer	x, y, w, gio

begin
	gio = GP_GIO(gp)

	# Autoscale the included data in Y and set wavelength range.
	j = 0
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    j = j + 1
	    n = STD_NWAVES(stds[i])
	    x = STD_X(stds[i])
	    y = STD_Y(stds[i])
	    if (j == 1)
		call alimr (Memr[y], n, ymin, ymax)
	    else {
		call alimr (Memr[y], n, y1, y2)
		ymin = min (ymin, y1)
		ymax = max (ymax, y2)
	    }
	}
	y1 = 0.05 * (ymax - ymin)
	ymin = ymin - y1
	ymax = ymax + y1

	# Draw axes and title based on type of graph.
	switch (graph) {
	case 'a':
	    call gswind (gio, GP_AIRMASS(gp,1), GP_AIRMASS(gp,2), ymin, ymax)
	    call glabax (gio, "Sensitivity Residuals vs Airmass", "", "")
	case 's':
	    call gswind (gio, GP_WSTART(gp), GP_WEND(gp), ymin, ymax)
	    call glabax (gio, "Sensitivity vs Wavelength", "", "")
	case 'r':
	    call gswind (gio, GP_WSTART(gp), GP_WEND(gp), ymin, ymax)
	    call glabax (gio, "Sensitivity Residuals vs Wavelength", "", "")
	}

	# Plot the data with appropriate mark types and sizes.
	mdel = GP_MDEL(gp)
	cdel = GP_CDEL(gp)
	szmdel = GP_SZMDEL(gp)
	szmark = GP_SZMARK(gp)
	do i = 1, nstds {
	    if (STD_FLAG(stds[i]) != SF_INCLUDE)
		next
	    if (i != nstds-1) {
		mark = GP_MARK(gp)
		color = GP_CMARK(gp)
	    } else {
		mark = GP_MADD(gp)
		color = GP_CADD(gp)
	    }
	    n = STD_NWAVES(stds[i])
	    x = STD_X(stds[i]) - 1
	    y = STD_Y(stds[i]) - 1
	    w = STD_WTS(stds[i]) - 1
	    do j = 1, n {
		if (Memr[w+j] == 0.) {
		    call gseti (gio, G_PLCOLOR, cdel)
		    call gmark (gio, Memr[x+j], Memr[y+j], mdel, szmdel, szmdel)
		} else {
		    call gseti (gio, G_PLCOLOR, color)
	            call gmark (gio, Memr[x+j], Memr[y+j], mark, szmark, szmark)
		}
	    }
	}
end


# SF_EGRAPH -- Graph extinction curves with and without residual extinction
# correction.

procedure sf_egraph (gp, wextn, extn, nextn, ecv)

pointer	gp			# Graphics structure
real	wextn[nextn]		# Extinction table wavelengths
real	extn[nextn]		# Extinction table values
int	nextn			# Number of extinction table values
pointer	ecv			# Residual extinction curve

int	i, j
real	xmin, xmax, dx, x, cveval()
pointer	sp, ext, extnew, gio

begin
	call smark (sp)
	call salloc (ext, NCURVE, TY_REAL)

	# Interpolate extinction table to a grid of wavelengths within
	# the range of the sensitivity data.

	gio = GP_GIO(gp)
	xmin = GP_WSTART(gp)
	xmax = GP_WEND(gp)
	dx = (xmax - xmin) / (NCURVE - 1)
	x = xmin
	do i = 0, NCURVE-1 {
	    call intrp (1, wextn, extn, nextn, x, Memr[ext+i], j)
	    x = x + dx
	}
	call gswind (gio, xmin, xmax, INDEF, INDEF)
	call gascale (gio, Memr[ext], NCURVE, 2)

	# If there is a residual extinction curve determine a new extinction
	# curve.

	if (ecv != NULL) {
	    call salloc (extnew, NCURVE, TY_REAL)
	    call amovr (Memr[ext], Memr[extnew], NCURVE)
	    x = xmin
	    do i = 0, NCURVE-1 {
	        Memr[extnew+i] = Memr[extnew+i] + cveval (ecv, x)
		x = x + dx
	    }
	    call grscale (gio, Memr[extnew], NCURVE, 2)
	}

	# Draw the axes and title and extinction curves.
	call glabax (gio, "Extinction vs Wavelength", "", "")
	call gseti (gio, G_PLCOLOR, GP_PLCOLOR(gp))
	call gvline (gio, Memr[ext], NCURVE, xmin, xmax)
	if (ecv != NULL) {
	    call gseti (gio, G_PLTYPE, 2)
	    call gseti (gio, G_PLCOLOR, GP_PLCOLOR(gp)+1)
	    call gvline (gio, Memr[extnew], NCURVE, xmin, xmax)
	    call gseti (gio, G_PLTYPE, 1)
	}

	call sfree (sp)
end


# SF_FITGRAPH -- Overplot the fitted sensitivity curve.

procedure sf_fitgraph (gp, cv)

pointer	gp			# Graphics structure
pointer	cv			# Sensitivity function curve

int	i, j
real	x1, x2, y1, y2, cveval()
pointer	gio

begin
	gio = GP_GIO(gp)
	call gseti (gio, G_PLCOLOR, GP_PLCOLOR(gp))

	# Only plot on sensitivity curve graph types.
	for (j = 1; GP_GRAPHS(gp,j) != EOS; j = j + 1) {
	    if (GP_GRAPHS(gp,j) != 's')
		next
	    call gseti (gio, G_WCS, j)
	    call ggwind (gio, x1, x2, y1, y2)
	    x2 = (x2 - x1) / NCURVE
	    y1 = cveval (cv, x1)
	    call gamove (gio, x1, y1)
	    do i = 1, NCURVE {
	        x1 = x1 + x2
	        y1 = cveval (cv, x1)
	        call gadraw (gio, x1, y1)
	    }
	}
end
