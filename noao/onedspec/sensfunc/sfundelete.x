include	<gset.h>
include "sensfunc.h"


# SF_UNDELETE -- Unelete point, star, or wavelength.

procedure sf_undelete (gp, stds, nstds, key, istd, ipt)

pointer	gp			# GIO pointer
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
int	key			# Delete point, star, or wavelength
int	istd			# Index of standard star
int	ipt			# Index of point

int	i, j, n, wcs, mark, mdel, color, stridx()
real	wave, szmark, szmdel
pointer	x, y, z, w, w1, gio

begin
	gio = GP_GIO(gp)
	mdel = GP_MDEL(gp)
	szmdel = GP_SZMDEL(gp)
	szmark = GP_SZMARK(gp)

	# Undelete points from each displayed graph.
	for (wcs = 1; GP_GRAPHS(gp,wcs) != EOS; wcs = wcs + 1) {
	    if (stridx (GP_GRAPHS(gp,wcs), "ars") == 0)
	        next

	    call gseti (gio, G_WCS, wcs)
	    call gseti (gio, G_PMLTYPE, 0)
	    call sf_data (stds, nstds, GP_GRAPHS(gp,wcs))
	    switch (key) {
	    case 'p':
		if (istd != nstds-1) {
		    mark = GP_MARK(gp)
		    color = GP_CMARK(gp)
		} else {
		    mark = GP_MADD(gp)
		    color = GP_CADD(gp)
		}
	        x = STD_X(stds[istd])+ipt-1
	        y = STD_Y(stds[istd])+ipt-1
	        w = STD_WTS(stds[istd])+ipt-1
	        w1 = STD_IWTS(stds[istd])+ipt-1
		call gseti (gio, G_PMLTYPE, 0)
	        call gmark (gio, Memr[x], Memr[y], mdel, szmdel, szmdel)
		call gseti (gio, G_PMLTYPE, 1)
		call gseti (gio, G_PLCOLOR, color)
	        call gmark (gio, Memr[x], Memr[y], mark, szmark , szmark)
	    case 's':
		if (istd != nstds-1) {
		    mark = GP_MARK(gp)
		    color = GP_CMARK(gp)
		} else {
		    mark = GP_MADD(gp)
		    color = GP_CADD(gp)
		}
	        n = STD_NWAVES(stds[istd])
	        x = STD_X(stds[istd])
	        y = STD_Y(stds[istd])
	        w = STD_WTS(stds[istd])
		do j = 1, n {
		    if (Memr[w] == 0.) {
			call gseti (gio, G_PMLTYPE, 0)
	        	call gmark (gio, Memr[x], Memr[y], mdel, szmdel, szmdel)
			call gseti (gio, G_PMLTYPE, 1)
			call gseti (gio, G_PLCOLOR, color)
	        	call gmark (gio, Memr[x], Memr[y], mark, szmark, szmark)
		    }
		    x = x + 1
		    y = y + 1
		    w = w + 1
	        }
	    case 'w':
	        wave = Memr[STD_WAVES(stds[istd])+ipt-1]
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
	            x = STD_X(stds[i])
	            y = STD_Y(stds[i])
		    z = STD_WAVES(stds[i])
	            w = STD_WTS(stds[i])
		    do j = 1, n {
		        if ((Memr[z] == wave) && (Memr[w] == 0.)) {
			    call gseti (gio, G_PMLTYPE, 0)
	    		    call gmark (gio, Memr[x], Memr[y], mdel, szmdel,
				szmdel)
			    call gseti (gio, G_PMLTYPE, 1)
			    call gseti (gio, G_PLCOLOR, color)
	    		    call gmark (gio, Memr[x], Memr[y], mark, szmark,
				szmark)
		        }
			x = x + 1
			y = y + 1
			z = z + 1
			w = w + 1
		    }
	        }
	    }
	}

	# Now actually undelete the points by resetting the weights.
	switch (key) {
	case 'p':
	    w = STD_WTS(stds[istd])+ipt-1
	    w1 = STD_IWTS(stds[istd])+ipt-1
	    Memr[w] = Memr[w1]
	case 's':
	    n = STD_NWAVES(stds[istd])
	    w = STD_WTS(stds[istd])
	    w1 = STD_IWTS(stds[istd])
	    call amovr (Memr[w1], Memr[w], n)
	case 'w':
	    wave = Memr[STD_WAVES(stds[istd])+ipt-1]
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
	        n = STD_NWAVES(stds[i])
		z = STD_WAVES(stds[i])
	        w = STD_WTS(stds[i])
	        w1 = STD_IWTS(stds[i])
		do j = 1, n {
		    if (Memr[z] == wave)
			Memr[w] = Memr[w1]
		    z = z + 1
		    w = w + 1
		    w1 = w1 + 1
		}
	    }
	}
end
