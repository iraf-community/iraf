include	<gset.h>
include "sensfunc.h"

# SF_DELETE -- Delete point, star, or wavelength identified by the
# star index and index within the array of values.

procedure sf_delete (gp, stds, nstds, key, istd, ipt)

pointer	gp			# GIO pointer
pointer	stds[nstds]		# Standard star data
int	nstds			# Number of standard stars
int	key			# Delete point, star, or wavelength
int	istd			# Index of standard star
int	ipt			# Index of point

int	i, j, n, wcs, mark, mdel, cdel, stridx()
real	wave, szmark, szmdel
pointer	x, y, z, w, gio

begin
	gio = GP_GIO(gp)
	mdel = GP_MDEL(gp)
	cdel = GP_CDEL(gp)
	szmdel = GP_SZMDEL(gp)
	szmark = GP_SZMARK(gp)

	# Delete the point or points from each displayed graph.
	# When deleting multiple points check if point already deleted.
	for (wcs = 1; GP_GRAPHS(gp,wcs) != EOS; wcs = wcs + 1) {
	    if (stridx (GP_GRAPHS(gp,wcs), "ars") == 0)
	        next

	    call gseti (gio, G_WCS, wcs)
	    call sf_data (stds, nstds, GP_GRAPHS(gp,wcs))
	    switch (key) {
	    case 'p':
		if (istd != nstds-1)
		    mark = GP_MARK(gp)
		else
		    mark = GP_MADD(gp)
	        x = STD_X(stds[istd])+ipt-1
	        y = STD_Y(stds[istd],1)+ipt-1
		call gseti (gio, G_PMLTYPE, 0)
	        call gmark (gio, Memr[x], Memr[y], mark, szmark, szmark)
		call gseti (gio, G_PMLTYPE, 1)
		call gseti (gio, G_PLCOLOR, cdel)
	        call gmark (gio, Memr[x], Memr[y], mdel, szmdel, szmdel)
	    case 's':
		if (istd != nstds-1)
		    mark = GP_MARK(gp)
		else
		    mark = GP_MADD(gp)
	        n = STD_NWAVES(stds[istd])
	        x = STD_X(stds[istd])
	        y = STD_Y(stds[istd])
	        w = STD_WTS(stds[istd])
		do i = 1, n {
		    if (Memr[w] != 0.) {
		        call gseti (gio, G_PMLTYPE, 0)
	                call gmark (gio, Memr[x], Memr[y], mark, szmark, szmark)
		        call gseti (gio, G_PMLTYPE, 1)
			call gseti (gio, G_PLCOLOR, cdel)
	                call gmark (gio, Memr[x], Memr[y], mdel, szmdel, szmdel)
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
		    if (i != nstds-1)
			mark = GP_MARK(gp)
		    else
			mark = GP_MADD(gp)
	            n = STD_NWAVES(stds[i])
	            x = STD_X(stds[i])
	            y = STD_Y(stds[i])
		    z = STD_WAVES(stds[i])
	            w = STD_WTS(stds[i])
		    do j = 1, n {
		        if ((Memr[z] == wave) && (Memr[w] != 0.)) {
		            call gseti (gio, G_PMLTYPE, 0)
	                    call gmark (gio, Memr[x], Memr[y], mark, szmark,
				    szmark)
		            call gseti (gio, G_PMLTYPE, 1)
			    call gseti (gio, G_PLCOLOR, cdel)
	                    call gmark (gio, Memr[x], Memr[y], mdel, szmdel,
				szmdel)
		        }
			x = x + 1
			y = y + 1
			z = z + 1
			w = w + 1
		    }
	        }
	    }
	}

	# Mark the points as deleted by setting their weights to zero.
	switch (key) {
	case 'p':
	    w = STD_WTS(stds[istd])+ipt-1
	    Memr[w] = 0.
	case 's':
	    n = STD_NWAVES(stds[istd])
	    w = STD_WTS(stds[istd])
	    call aclrr (Memr[w], n)
	case 'w':
	    wave = Memr[STD_WAVES(stds[istd])+ipt-1]
	    do i = 1, nstds {
	        if (STD_FLAG(stds[i]) != SF_INCLUDE)
		    next
	        n = STD_NWAVES(stds[i])
		z = STD_WAVES(stds[i])
	        w = STD_WTS(stds[i])
		do j = 1, n {
		    if (Memr[z] == wave)
	                Memr[w] = 0.
		    w = w + 1
		    z = z + 1
		}
	    }
	}
end
