include	<gset.h>
include	<mach.h>
include	"specfocus.h"

define	HELP		"specfocus$specfocus.key"
define	PROMPT		"specfocus options"

define	VX1	.15		# Minimum X viewport
define	VX2	.95		# Maximum X viewport
define	VY1	.10		# Minimum Y viewport for bottom graph
define	VY2	.44		# Minimum Y viewport for bottom graph
define	VY3	.54		# Minimum Y viewport for top graph
define	VY4	.88		# Maximum Y viewport for top graph

define	NMAX	5		# Maximum number of samples
define	HLCOLOR	2		# Highlight color
define	HLWIDTH	4.		# Highlight width


# SPF_GRAPH -- Interactive graphing of results

procedure spf_graph (sfavg, sfbest, sfs, nimages, lag)

pointer	sfavg			# Average image
pointer	sfbest			# Best image
pointer	sfs[nimages]		# Images
int	nimages			# Number of images
int	lag			# Maximum lag

char	cmd[10]
real	wx, wy, f, w, r2, r2min, fa[8]
int	i, j, k, l, i1, j1, nx, ny, nxgrid, nygrid, nsfd
int	wcs, key, pkey, del, clgcur()
pointer	sp, sysidstr, title, gp, gopen()
pointer	sf, sfd, sfcur

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	call smark (sp)
	call salloc (sysidstr, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)

	# Set system id label
	call sysid (Memc[sysidstr], SZ_LINE)

	# Set current image and sample
	nsfd = SF_NSFD(sfavg)
	nx = SF_NX(sfavg)
	ny = SF_NY(sfavg)
	i = (nx + 1) / 2
	j = (ny + 1) / 2
	for (k=1; k<nimages && sfs[k]!=sfbest; k=k+1)
	    ;

	# Set grid layout
	if (nimages < 3) {
	    nxgrid = nimages
	    nygrid = 1
	} else {
	    nxgrid = nint (sqrt (real (nimages)))
	    if (mod (nimages, nxgrid+1) >= mod (nimages, nxgrid))
		nxgrid = nxgrid + 1
	    nygrid = (nimages-1) / nxgrid + 1
	}

	# Open graphics and enter interactive graphics loop
	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	key = 'b'
	wcs = 0
	repeat {
	    # Verify keys, check for '?', and 'q', set redraw.
	    switch (key) {
	    case '?':
		call gpagefile (gp, HELP, PROMPT)
		next
	    case 'b', 'p', 's', 'w', 'z':
		pkey = key
		del = NO
	    case 'q':
		break
	    case 'r':
		key = pkey
		del = NO
	    case ' ', 'd':
		del = NO
	    case 'u':
		del = YES
	    default:
		call printf ("\007")
		next
	    }

	    # Map the cursor position to an image and sample.
	    switch (wcs) {
	    case 1:
		k = 1
		call spf_sample (sfavg, 1, del, wx, wy, i, j, k)
		f = SF_FOC(SFD(sfavg,i,j))
		r2min = MAX_REAL
		do l = 1, nimages {
		    sf = sfs[l]
		    sfd = SFD(sf,i,j)
		    if (SF_DEL(sfd) == del) {
			r2 = abs (f - SF_FOC(sfd))
			if (r2 < r2min) {
			    r2min = r2
			    k = l
			}
		    }
		}
		call spf_sample (sfs, nimages, del, wx, wy, i, j, k)
	    case 2, 6:
		call spf_sample (sfs, nimages, del, wx, wy, i, j, k)
	    case 3:
		r2min = MAX_REAL
		call gctran (gp, wx, wy, wx, wy, wcs, 0)
		do l = 1, nimages {
		    sf = sfs[l]
		    do j1 = 1, ny {
			do i1 = 1, nx {
			    sfd = SFD(sf,i1,j1)
			    if (SF_DEL(sfd) == del) {
				f = SF_FOC(sfd)
				w = SF_WID(sfd)
				call gctran (gp, f, w, f, w, wcs, 0)
				r2 = (f-wx)**2 + (w-wy)**2
				if (r2 < r2min) {
				    r2min = r2
				    i = i1
				    j = j1
				    k = l
				}
			    }
			}
		    }
		}
	    case 4, 5, 8:
		i1 = max (1, min (nxgrid, nint(wx)))
		j1 = max (1, min (nygrid, nint(wy)))
		k = max (1, min (nimages, (j1-1) * nxgrid + i1))
		call spf_sample (sfs, nimages, del, real(i), real(j), i, j, k)
		if (wcs == 8) {
		    wx = nx * (wx - nint (wx) + 0.5) + 0.5
		    wy = -(ny+1) * (wy - nint (wy) + 0.5) + (ny+1.5)
		    call spf_sample (sfs, nimages, del, wx, wy, i, j, k)
		}
	    }

	    # Switch on action key
	    switch (key) {
	    case ' ':
		if (wcs == 1)
		    sf = sfavg
		else
		    sf = sfs[k]
		sfd = SFD(sf,i,j)
		call printf (
		    "Image %s at (%d, %d), Focus = %.3g, Width = %.2f")
		    call pargstr (SF_IMAGE(sf))
		    call pargr (SF_X(sfd))
		    call pargr (SF_Y(sfd))
		    call pargr (SF_FOC(sfd))
		    call pargr (SF_WID(sfd))
		if (abs (SF_POS(sfd)) > .01) {
		    call printf (", Shift = %.2f")
			call pargr (SF_POS(sfd))
		}
		call printf ("\n")
		next
	    case 'd':
		repeat {
		    switch (key) {
		    case 'i':
			do j1 = 1, ny
			   do i1 = 1, nx
				SF_DEL(SFD(sfs[k],i1,j1)) = YES
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
		    case 's':
			do l = 1, nimages {
			    sfd = SFD(sfs[l],i,j)
			    SF_DEL(sfd) = YES
			}
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
		    case 'p':
			sfd = SFD(sfs[k],i,j)
			SF_DEL(sfd) = YES
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
		    default:
			call printf ("Delete image, sample, or point?")
			next
		    }
		    call spf_fitfocus (sfs, nimages, sfavg, sfbest)
		    key = pkey
		    break
		} until (clgcur ("gcur", wx, wy, wcs, key, cmd, 10) == EOF)
	    case 'u':
		repeat {
		    switch (key) {
		    case 'i':
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
			do j1 = 1, ny
			   do i1 = 1, nx
				SF_DEL(SFD(sfs[k],i1,j1)) = NO
		    case 's':
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
			do l = 1, nimages {
			    sfd = SFD(sfs[l],i,j)
			    SF_DEL(sfd) = NO
			}
		    case 'p':
			call spf_sample (sfs, nimages, del, real(i), real(j),
			    i, j, k)
			sfd = SFD(sfs[k],i,j)
			SF_DEL(sfd) = NO
		    default:
			call printf ("Undelete image, sample or point?")
			next
		    }
		    call spf_fitfocus (sfs, nimages, sfavg, sfbest)
		    key = pkey
		    break
		} until (clgcur ("gcur", wx, wy, wcs, key, cmd, 10) == EOF)
	    }
	    sfcur = sfs[k]
	    sfd = SFD(sfcur,i,j)

	    # Make the graphs.
	    call gclear (gp)
	    call gseti (gp, G_FACOLOR, 0)

	    if (nimages > 1 && nsfd > 1) {
		switch (key) {
		case 'p':
		    call gseti (gp, G_WCS, 4)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g4 (gp, sfavg, sfbest, sfcur, sfs, nimages, i, j,
			nxgrid, nygrid, lag)
		    if (nx > NMAX || ny > NMAX) {
			call gseti (gp, G_WCS, 1)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g1 (gp, sfcur, i, j, NO, NO)
		    } else {
			call gseti (gp, G_WCS, 2)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g2 (gp, sfavg, sfbest, sfcur, i, j, lag)
		    }
		case 's':
		    call gseti (gp, G_WCS, 5)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g5 (gp, sfcur, sfs, nimages, i, j, nxgrid, nygrid)
		    if (nx > NMAX || ny > NMAX) {
			call gseti (gp, G_WCS, 1)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g1 (gp, sfcur, i, j, NO, NO)
		    } else {
			call gseti (gp, G_WCS, 6)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g6 (gp, sfcur, i, j)
		    }
		case 'w':
		    call gseti (gp, G_WCS, 3)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g3 (gp, sfavg, sfcur, sfs, nimages, i, j)
		    if (nx > NMAX || ny > NMAX) {
			call gseti (gp, G_WCS, 1)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g1 (gp, sfcur, i, j, NO, NO)
		    } else {
			call gseti (gp, G_WCS, 8)
			call gsview (gp, VX1, VX2, VY1, VY2)
			call gfill (gp, fa, fa[5], 4, GF_SOLID)
			call spf_g8 (gp, sfavg, sfcur, sfd, sfs, nimages,
			    nxgrid, nygrid)
		    }
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g7 (gp, sfbest, sfcur, i, j, lag)
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call spf_g9 (gp, sfcur, i, j)
		default:
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX2, VY1, VY4)
		    call spf_g1 (gp, sfavg, i, j, YES, YES)
		}
	    } else if (nimages > 1) {
		switch (key) {
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g7 (gp, sfbest, sfcur, i, j, lag)
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call spf_g9 (gp, sfcur, i, j)
		case 's':
		    call gseti (gp, G_WCS, 3)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g3 (gp, sfavg, sfcur, sfs, nimages, i, j)
		    call gseti (gp, G_WCS, 5)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call spf_g5 (gp, sfcur, sfs, nimages, i, j, nxgrid, nygrid)
		default:
		    call gseti (gp, G_WCS, 3)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g3 (gp, sfavg, sfcur, sfs, nimages, i, j)
		    call gseti (gp, G_WCS, 4)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call spf_g4 (gp, sfavg, sfbest, sfcur, sfs, nimages, i, j,
			nxgrid, nygrid, lag)
		}
	    } else if (nsfd > 1) {
		switch (key) {
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g7 (gp, sfbest, sfcur, i, j, lag)
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call spf_g9 (gp, sfcur, i, j)
		default:
		    call gseti (gp, G_WCS, 2)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call spf_g2 (gp, sfavg, sfbest, sfcur, i, j, lag)
		    call gseti (gp, G_WCS, 6)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call spf_g6 (gp, sfcur, i, j)
		}
	    } else {
		call gseti (gp, G_WCS, 7)
		call gsview (gp, VX1, VX2, VY3, VY4)
		call spf_g7 (gp, sfbest, sfcur, i, j, lag)
		call gseti (gp, G_WCS, 9)
		call gsview (gp, VX1, VX2, VY1, VY2)
		call gfill (gp, fa, fa[5], 4, GF_SOLID)
		call spf_g9 (gp, sfcur, i, j)
	    }

	    call sprintf (Memc[title], SZ_LINE,
		"Best Average Focus at %.3g with Width of %.3g at %d%% of Peak")
		call pargr (SF_FOCUS(sfavg))
		call pargr (SF_WIDTH(sfavg))
		call pargr (100 * SF_LEVEL(sfavg))
	    call gseti (gp, G_WCS, 0)
	    call gsetr (gp, G_PLWIDTH, 2.0)
	    call gline (gp, 0., 0., 0., 0.)
	    call gtext (gp, 0.5, 0.99, Memc[sysidstr], "h=c,v=t")
	    call gtext (gp, 0.5, 0.96, Memc[title], "h=c,v=t")

	    pkey = key
	} until (clgcur ("gcur", wx, wy, wcs, key, cmd, 10) == EOF)

	call gclose (gp)
	call sfree (sp)
end


# SPF_G1 -- Best Focus at each sample

procedure spf_g1 (gp, sf, ix, iy, focplot, posplot)

pointer	gp		# GIO pointer
pointer	sf		# SF pointer
int	ix, iy		# Sample
int	focplot		# Focus plot?
int	posplot		# Position plot?

int	i, j, n, nx, ny
real	wx1, wx2, wy1, wy2, ww1, ww2, wf1, wf2, wp1, wp2
real	vx[3,2], vy[3,2], dvx, dvy, fa[8]
real	x, y, z, last
pointer	sp, str, sfd

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	nx = SF_NX(sf)
	ny = SF_NY(sf)
	wx1 = SF_X1(sf)
	wy1 = SF_Y1(sf)
	wx2 = wx1 + (nx + .3) * SF_DX(sf)
	wy2 = wy1 + ny * SF_DY(sf)

	# Determine the range of WID, FOC, and POS.
	ww1 = MAX_REAL
	wf1 = MAX_REAL
	wp1 = MAX_REAL
	ww2 = -MAX_REAL
	wf2 = -MAX_REAL
	wp2 = -MAX_REAL
	do j = 1, ny {
	    do i = 1, nx {
		sfd = SFD(sf,i,j)
		if (SF_DEL(sfd) == NO) {
		    ww1 = min (ww1, SF_WID(sfd))
		    wf1 = min (wf1, SF_FOC(sfd))
		    wp1 = min (wp1, SF_POS(sfd))
		    ww2 = max (ww2, SF_WID(sfd))
		    wf2 = max (wf2, SF_FOC(sfd))
		    wp2 = max (wp2, SF_POS(sfd))
		}
	    }
	}
	ww2 = max (2., ww2 - ww1)

	# Set view ports
	call ggview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	dvx = vx[3,2] - vx[1,1]
	dvy = vy[3,2] - vy[1,1]
	vx[1,2] = vx[1,1] + 0.20 * dvx
	vx[2,1] = vx[1,1] + 0.25 * dvx
	vx[2,2] = vx[1,1] + 0.75 * dvx
	vx[3,1] = vx[1,1] + 0.80 * dvx
	vy[1,2] = vy[1,1] + 0.20 * dvy
	vy[2,1] = vy[1,1] + 0.25 * dvy
	vy[2,2] = vy[1,1] + 0.75 * dvy
	vy[3,1] = vy[1,1] + 0.80 * dvy

	z = abs ((wf1 + wf2) / 2.)
	if (z == 0.)
	    z = max (abs(wf1), abs(wf2))
	if (z == 0.)
	    z = 1.
	if (focplot == NO || abs (wf2 - wf1) / z <= 0.01) {
	    vx[2,1] = vx[1,1]
	    vy[2,1] = vy[1,1]
	}
	if (posplot == NO || abs (wp2 - wp1) <= .05) {
	    vx[2,2] = vx[3,2]
	    vy[2,2] = vy[3,2]
	}
	if (nx == 1) {
	    vy[2,1] = vy[1,1]
	    vy[2,2] = vy[3,2]
	}
	if (ny == 1) {
	    vx[2,1] = vx[1,1]
	    vx[2,2] = vx[3,2]
	}

	call gseti (gp, G_DRAWAXES, 3)
	call gseti (gp, G_DRAWTICKS, YES)

	# FOC plot
	if (focplot == YES && abs (wf2 - wf1) / z > 0.01) {
	    z = wf2 - wf1
	    wf1 = wf1 - 0.05 * z
	    wf2 = wf2 + 0.05 * z

	    if (nx > 1) {
		call gseti (gp, G_LABELTICKS, YES)
		call gseti (gp, G_NMAJOR, 6)
		call gseti (gp, G_NMINOR, 4)
		call gseti (gp, G_YNMAJOR, 4)
		call gseti (gp, G_YNMINOR, 0)
		call gsview (gp, vx[2,1], vx[2,2], vy[1,1], vy[1,2])
		call gswind (gp, 0., 1., 0., 1.)
		call gfill (gp, fa, fa[5], 4, GF_SOLID)
		call gswind (gp, wx1, wx2, wf1, wf2)
		if (SF_AXIS(sf) == 1)
		    call glabax (gp, "", "Column (Dispersion)", "Focus")
		else
		    call glabax (gp, "", "Column (Slit)", "Focus")

		call gswind (gp, 0.5, 0.8+nx, wf1, wf2)
		last = INDEF
		do i = 1, nx {
		    x = i
		    do j = 1, ny {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = SF_WID(sfd)
			    z = 0.008 * (1 + (z - ww1) / ww2 * 3)
			    call gmark (gp, x, SF_FOC(sfd), GM_CIRCLE, z, z)
			}
		    }

		    n = 0
		    z = 0.
		    do j = 1, ny {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = z + SF_FOC(SFD(sf,i,j))
			    n = n + 1
			}
		    }
		    if (n > 0) {
			if (!IS_INDEF(last))
			    call gline (gp, last, y, x, z/n)
			y = z / n
			last = x
		    }
		}

		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, 0.5, SF_FOCUS(sf), 0.8+nx, SF_FOCUS(sf))
		call gseti (gp, G_PLTYPE, 1)
	    }

	    if (ny > 1) {
		call gseti (gp, G_LABELTICKS, YES)
		call gseti (gp, G_NMAJOR, 6)
		call gseti (gp, G_NMINOR, 4)
		call gseti (gp, G_XNMAJOR, 4)
		call gseti (gp, G_XNMINOR, 0)
		call gsview (gp, vx[1,1], vx[1,2], vy[2,1], vy[2,2])
		call gswind (gp, 0., 1., 0., 1.)
		call gfill (gp, fa, fa[5], 4, GF_SOLID)
		call gswind (gp, wf1, wf2, wy1, wy2)
		if (SF_AXIS(sf) == 1)
		    call glabax (gp, "", "Focus", "Line (Slit)")
		else
		    call glabax (gp, "", "Focus", "Line (Dispersion)")

		call gswind (gp, wf1, wf2, 0.5, 0.5+ny)
		last = INDEF
		do j = 1, ny {
		    y = j
		    do i = 1, nx {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = SF_WID(sfd)
			    z = 0.008 * (1 + (z - ww1) / ww2 * 3)
			    call gmark (gp, SF_FOC(sfd), y, GM_CIRCLE, z, z)
			}
		    }

		    n = 0
		    z = 0.
		    do i = 1, nx {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = z + SF_FOC(SFD(sf,i,j))
			    n = n + 1
			}
		    }
		    if (n > 0) {
			if (!IS_INDEF(last))
			    call gline (gp, x, last, z/n, y)
			x = z / n
			last = y
		    }
		}

		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, SF_FOCUS(sf), 0.5, SF_FOCUS(sf), 0.5+ny)
		call gseti (gp, G_PLTYPE, 1)
	    }
	}

	# POS plot
	if (posplot == YES && abs (wp2 - wp1) > .05) {
	    z = wp2 - wp1
	    wp1 = wp1 - 0.05 * z
	    wp2 = wp2 + 0.05 * z

	    if (nx > 1) {
		call gseti (gp, G_XLABELTICKS, NO)
		call gseti (gp, G_YLABELTICKS, YES)
		call gseti (gp, G_NMAJOR, 6)
		call gseti (gp, G_NMINOR, 4)
		call gseti (gp, G_YNMAJOR, 4)
		call gseti (gp, G_YNMINOR, 0)
		call gsview (gp, vx[2,1], vx[2,2], vy[3,1], vy[3,2])
		call gswind (gp, 0., 1., 0., 1.)
		call gfill (gp, fa, fa[5], 4, GF_SOLID)
		call gswind (gp, wx1, wx2, wp1, wp2)
		call glabax (gp, "", "", "Shift")

		call gswind (gp, 0.5, 0.8+nx, wp1, wp2)
		last = INDEF
		do i = 1, nx {
		    x = i
		    do j = 1, ny {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = SF_WID(sfd)
			    z = 0.008 * (1 + (z - ww1) / ww2 * 3)
			    call gmark (gp, x, SF_POS(sfd), GM_CIRCLE, z, z)
			}
		    }

		    n = 0
		    z = 0.
		    do j = 1, ny {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = z + SF_POS(SFD(sf,i,j))
			    n = n + 1
			}
		    }
		    if (n > 0) {
			if (!IS_INDEF(last))
			    call gline (gp, last, y, x, z/n)
			y = z / n
			last = x
		    }
		}

		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, 0.5, 0., 0.8+nx, 0.)
		call gseti (gp, G_PLTYPE, 1)
	    }

	    if (ny > 1) {
		call gseti (gp, G_XLABELTICKS, YES)
		call gseti (gp, G_YLABELTICKS, NO)
		call gseti (gp, G_NMAJOR, 6)
		call gseti (gp, G_NMINOR, 4)
		call gseti (gp, G_XNMAJOR, 4)
		call gseti (gp, G_XNMINOR, 0)
		call gsview (gp, vx[3,1], vx[3,2], vy[2,1], vy[2,2])
		call gswind (gp, 0., 1., 0., 1.)
		call gfill (gp, fa, fa[5], 4, GF_SOLID)
		call gswind (gp, wp1, wp2, wy1, wy2)
		call glabax (gp, "", "Shift", "")

		call gswind (gp, wp1, wp2, 0.5, 0.5+ny)
		last = INDEF
		do j = 1, ny {
		    y = j
		    do i = 1, nx {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = SF_WID(sfd)
			    z = 0.008 * (1 + (z - ww1) / ww2 * 3)
			    call gmark (gp, SF_POS(sfd), y, GM_CIRCLE, z, z)
			}
		    }

		    n = 0
		    z = 0.
		    do i = 1, nx {
			sfd = SFD(sf,i,j)
			if (SF_DEL(sfd) == NO) {
			    z = z + SF_POS(sfd)
			    n = n + 1
			}
		    }
		    if (n > 0) {
			if (!IS_INDEF(last))
			    call gline (gp, x, last, z/n, y)
			x = z / n
			last = y
		    }
		}

		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, 0., 0.5, 0., 0.5+ny)
		call gseti (gp, G_PLTYPE, 1)
	    }
	}

	# Spatial plot
	call gsview (gp, vx[2,1], vx[2,2], vy[2,1], vy[2,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)
	call gswind (gp, wx1, wx2, wy1, wy2)
	call gseti (gp, G_NMAJOR, 6)
	call gseti (gp, G_NMINOR, 4)
	if (vx[2,1] == vx[1,1] && vy[2,1] == vy[1,1]) {
	    call gseti (gp, G_LABELTICKS, YES)
	    if (SF_AXIS(sf) == 1)
		call glabax (gp, "", "Column (Dispersion)", "Line (Slit)")
	    else
		call glabax (gp, "", "Column (Slit)", "Line (Dispersion)")
	} else if (vx[2,1] == vx[1,1]) {
	    call gseti (gp, G_XLABELTICKS, NO)
	    call gseti (gp, G_YLABELTICKS, YES)
	    if (SF_AXIS(sf) == 1)
		call glabax (gp, "", "", "Line (Slit)")
	    else
		call glabax (gp, "", "", "Line (Dispersion)")
	} else if (vy[2,1] == vy[1,1]) {
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gseti (gp, G_YLABELTICKS, NO)
	    if (SF_AXIS(sf) == 1)
		call glabax (gp, "", "Column (Dispersion)", "")
	    else
		call glabax (gp, "", "Column (Slit)", "")
	} else {
	    call gseti (gp, G_LABELTICKS, NO)
	    call glabax (gp, "", "", "")
	}

	call gswind (gp, 0.5, 0.8+nx, 0.5, 0.5+ny)

	do j = 1, ny {
	    do i = 1, nx {
		sfd = SFD(sf,i,j)
		if (SF_DEL(sfd) == NO) {
		    x = i
		    y = j
		    z = SF_WID(sfd)
		    z = 0.008 * (1 + (z - ww1) / ww2 * 3)
		    call gmark (gp, x, y, GM_CIRCLE, z, z)
		    if (i == ix && j == iy) {
			call gseti (gp, G_PLCOLOR, HLCOLOR)
			call gmark (gp, x, y, GM_BOX, -.8, -.8)
			call gseti (gp, G_PLCOLOR, 1)
		    }
		    if (abs (wp2 - wp1) > .05) {
			if (SF_AXIS(sf) == 1) {
			    z =  0.25 * SF_POS(sfd) / max (abs(wp1), abs(wp2))
			    call gadraw (gp, x+z, y)
			} else {
			    z = 0.25 * SF_POS(sfd) / max (abs(wp1), abs(wp2))
			    call gadraw (gp, x, y+z)
			}
		    }
		}
	    }
	}

	if (nx <= 3 && ny <= 3) {
	    call gsetr (gp, G_PLWIDTH, 2.)
	    call gline (gp, wx1, wy1, wx1, wy1)
	    do j = 1, ny {
		do i = 1, nx {
		    sfd = SFD(sf,i,j)
		    if (SF_DEL(sfd) == NO) {
			x = i
			y = j
			
			call sprintf (Memc[str], SZ_LINE, "%.3g")
			    call pargr (SF_FOC(sfd))
			call gtext (gp, x+0.2, y+0.2, Memc[str], "h=l;v=c")
			call sprintf (Memc[str], SZ_LINE, "%.2f")
			    call pargr (SF_WID(sfd))
			call gtext (gp, x+0.2, y-0.2, Memc[str], "h=l;v=c")
			if (abs (SF_POS(sfd)) >= .01) {
			    call sprintf (Memc[str], SZ_LINE, "%.2f")
				call pargr (SF_POS(sfd))
			    call gtext (gp, x+0.2, y, Memc[str], "h=l;v=c")
			}
		    }
		}
	    }
	    call gsetr (gp, G_PLWIDTH, 1.)
	}

	if (SF_DATA(sf) == NULL) {
	    call strcpy ("Best Focus at Each Sample", Memc[str], SZ_LINE)
	} else {
	    call sprintf (Memc[str], SZ_LINE, "Image %s with Focus %.3g")
		call pargstr (SF_IMAGE(sf))
		call pargr (SF_FOCUS(sf))
	}
	call gseti (gp, G_DRAWAXES, 0)
	call gsview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	call glabax (gp, Memc[str], "", "")

	call sfree (sp)
end


# SPF_G2 -- Profiles at each sample for a given image

procedure spf_g2 (gp, sfavg, sfbest, sfcur, ix, iy, lag)

pointer	gp		# GIO pointer
pointer	sfavg		# Average image
pointer	sfbest		# Best image
pointer	sfcur		# Current image
int	ix, iy		# Sample
int	lag		# Maximum lag

int	i, j, nx, ny
real	x1, x2, y1, y2, z1, z2, dz, vx, dvx, vy, dvy, p, x, fa[10], asieval()
pointer	sp, str, sfd, asi

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set range and draw axes
	i = min (lag, nint (3 * SF_WIDTH(sfavg)))
	x1 = -i
	x2 = i
	y1 = -0.05
	y2 = 1.55
	fa[1] = x1; fa[6] = y1
	fa[2] = x2; fa[7] = y1
	fa[3] = x2; fa[8] = y2
	fa[4] = x1; fa[9] = y2
	fa[5] = x1; fa[10] = y1
	call gswind (gp, x1, x2, y1, y2)

	call gseti (gp, G_DRAWTICKS, NO)
	call sprintf (Memc[str], SZ_LINE, "Image %s with Focus %.3g")
	    call pargstr (SF_IMAGE(sfcur))
	    call pargr (SF_FOCUS(sfcur))
	call glabax (gp, Memc[str], "", "")

	# Set subviewport
	nx = SF_NX(sfcur)
	ny = SF_NY(sfcur)
	call ggview (gp, vx, dvx, vy, dvy)
	dvx = (dvx - vx) / nx
	dvy = (dvy - vy) / ny

	# Draw correlation profiles
	do j = 1, ny {
	    do i = 1, nx {
		call gsview (gp, vx+(i-1)*dvx, vx+i*dvx, vy+(j-1)*dvy, vy+j*dvy)
		call glabax (gp, "", "", "")

		if (i == ix && j == iy) {
		    call gsview (gp, vx+(i-1)*dvx+.005, vx+i*dvx-.005,
			vy+(j-1)*dvy+.005, vy+j*dvy-.005)
		    call gsetr (gp, G_PLWIDTH, HLWIDTH)
		    call gseti (gp, G_PLCOLOR, HLCOLOR)
		    call gpline (gp, fa, fa[6], 5)
		    call gsetr (gp, G_PLWIDTH, 1.)
		    call gseti (gp, G_PLCOLOR, 1)
		}

		sfd = SFD(sfcur,i,j)
		if (SF_DEL(sfd) == NO) {
		    asi = SF_ASI(sfd)
		    p = sqrt (2.) * SF_POS(sfd)
		    z1 = max (x1, x1-p)
		    z2 = min (x2, x2-p)
		    dz = 1
		    for (x=nint(z1); x<=nint(z2); x=x+dz)
			call gmark (gp, x+p, asieval (asi, x+lag+1),
			    GM_PLUS, 2., 2.)
		    call gamove (gp, z1+p, asieval (asi, z1+lag+1))
		    dz = .1
		    for (x=z1+dz; x<=z2; x=x+dz)
			call gadraw (gp, x+p, asieval (asi, x+lag+1))
		    if (sfcur != sfbest) {
			asi = SF_ASI(SFD(sfbest,i,j))
			call gamove (gp, z1+p, asieval (asi, z1+lag+1))
			for (x=z1+dz; x<=z2; x=x+dz)
			    call gadraw (gp, x+p, asieval (asi, x+lag+1))
		    }

		    call gseti (gp, G_PLTYPE, 3)
		    call gline (gp, 0., 0., 0., 1.)
		    call gseti (gp, G_PLTYPE, 1)

		    if (nx <= NMAX && ny <= NMAX)
			call spf_label (gp, sfd, 21, 2)
		}
	    }
	}

	call gswind (gp, 0.5, 0.5+nx, 0.5, 0.5+ny)
	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gamove (gp, 1., 1.)

	call sfree (sp)
end


# SPF_G3 -- Width vs. Focus

procedure spf_g3 (gp, sfavg, sfcur, sfs, nimages, ix, iy)

pointer	gp			# GIO pointer
pointer	sfavg			# Average image
pointer	sfcur			# Current image
pointer	sfs[nimages]		# Images
int	nimages			# Number of images
int	ix, iy			# Current sample

int	i, j, k, mark
real	x, x1, x2, dx, y, y1, y2, dy, size
pointer	sf, sfd

begin
	# Determine data range
	x1 = MAX_REAL
	y1 = MAX_REAL
	x2 = -MAX_REAL
	y2 = -MAX_REAL
	do i = 1, nimages {
	    sf = sfs[i]
	    do j = 1, SF_NSFD(sf) {
		sfd = SFD(sf,j,1)
		if (SF_DEL(sfd) == NO) {
		    x = SF_FOC(sfd)
		    y = SF_WID(sfd)
		    x1 = min (x1, x)
		    x2 = max (x2, x)
		    y1 = min (y1, y)
		    y2 = max (y2, y)
		}
	    }
	}

	dx = (x2 - x1)
	dy = (y2 - y1)
	x1 = x1 - dx * 0.05
	x2 = x2 + dx * 0.05
	y1 = y1 - dy * 0.05
	y2 = y2 + dy * 0.05
	call gswind (gp, x1, x2, y1, y2)
	call glabax (gp, "Profile Width vs. Focus", "", "")

	do k = 1, nimages {
	    sf = sfs[k]
	    do j = 1, SF_NY(sf) {
		do i = 1, SF_NX(sf) {
		    call gseti (gp, G_PLCOLOR, 1)
		    if (sf == sfcur)
			mark = GM_PLUS
		    else
			mark = GM_CROSS
		    size = 2.
		    if (sf == sfcur && i == ix && j == iy) {
			call gseti (gp, G_PLCOLOR, HLCOLOR)
			mark = mark + GM_BOX
			size = 3.
		    }
		    sfd = SFD(sf,i,j)
		    if (SF_DEL(sfd) == NO) {
			x = SF_FOC(sfd)
			y = SF_WID(sfd)
			call gmark (gp, x, y, mark, size, size)
		    }
		}
	    }
	}

	call gseti (gp, G_PLTYPE, 3)
	x = INDEF
	do k = 1, nimages {
	    sf = sfs[k]
	    sfd = SFD(sf,ix,iy)
	    if (SF_DEL(sfd) == NO) {
		if (!IS_INDEF(x))
		    call gline (gp, x, y, SF_FOC(sfd), SF_WID(sfd))
		x = SF_FOC(sfd)
		y = SF_WID(sfd)
	    }
	}

	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, SF_FOCUS(sfavg), y1, SF_FOCUS(sfavg), y2)
	call gline (gp, x1, SF_WIDTH(sfavg), x2, SF_WIDTH(sfavg))
	call gseti (gp, G_PLTYPE, 1)
end


# SPF_G4 -- Profiles at a given sample

procedure spf_g4 (gp, sfavg, sfbest, sfcur, sfs, nimages, ix, iy, nx, ny, lag)

pointer	gp		# GIO pointer
pointer	sfavg		# Average image
pointer	sfbest		# Best image
pointer	sfcur		# Current image
pointer	sfs[nimages]	# Images
int	nimages		# Number of images
int	ix, iy		# Sample
int	nx, ny		# Grid layout
int	lag		# Maximum lag

int	i, j, k
real	x1, x2, y1, y2, z1, z2, dz, p, x, fa[10], asieval()
real	vx, dvx, vy, dvy
pointer	sp, str, sf, sfd, asi

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	if (SF_NSFD(sfcur) > 1) {
	    call sprintf (Memc[str], SZ_LINE,
		"All Images at Sample [%d:%d,%d:%d]")
		call pargi (SF_X1(sfcur)+(ix-1)*SF_DX(sfcur))
		call pargi (SF_X1(sfcur)+ix*SF_DX(sfcur)-1)
		call pargi (SF_Y1(sfcur)+(iy-1)*SF_DY(sfcur))
		call pargi (SF_Y1(sfcur)+iy*SF_DY(sfcur)-1)
	} else
	     Memc[str] = EOS

	# Set windows
	i = min (lag, nint (3 * SF_WIDTH(sfavg)))
	x1 = -i
	x2 = i
	y1 = -0.05
	y2 = 1.55
	call gswind (gp, x1, x2, y1, y2)
	fa[1] = x1; fa[6] = y1
	fa[2] = x2; fa[7] = y1
	fa[3] = x2; fa[8] = y2
	fa[4] = x1; fa[9] = y2
	fa[5] = x1; fa[10] = y1

	# Set subview port
	call ggview (gp, vx, dvx, vy, dvy)
	dvx = (dvx - vx) / nx
	dvy = (dvy - vy) / ny

	# Draw correlation profiles
	k = 0
	do i =  1, ny {
	    do j = 1, nx {
		k = k + 1
		if (k > nimages)
		    break
		sf = sfs[k]
		sfd = SFD(sf,ix,iy)

		call gsview (gp, vx+(j-1)*dvx, vx+j*dvx,
		    vy+(ny-i)*dvy, vy+(ny-i+1)*dvy)
		call gfill (gp, fa, fa[6], 4, GF_SOLID)
		if (sf == sfcur) {
		    call gsview (gp, vx+(j-1)*dvx+.005, vx+j*dvx-.005,
			vy+(ny-i)*dvy+.005, vy+(ny-i+1)*dvy-.005)
		    call gsetr (gp, G_PLWIDTH, HLWIDTH)
		    call gseti (gp, G_PLCOLOR, HLCOLOR)
		    call gpline (gp, fa, fa[6], 5)
		    call gsetr (gp, G_PLWIDTH, 1.)
		    call gseti (gp, G_PLCOLOR, 1)
		    call gsview (gp, vx+(j-1)*dvx, vx+j*dvx,
			vy+(ny-i)*dvy, vy+(ny-i+1)*dvy)
		}
		call gseti (gp, G_DRAWAXES, 3)
		call gseti (gp, G_DRAWTICKS, NO)
		call glabax (gp, "", "", "")


		if (SF_DEL(sfd) == NO) {
		    asi = SF_ASI(sfd)
		    p = sqrt (2.) * SF_POS(sfd)
		    z1 = max (x1, x1-p)
		    z2 = min (x2, x2-p)
		    dz = 1
		    for (x=nint(z1); x<=nint(z2); x=x+dz)
			call gmark (gp, x+p, asieval (asi, x+lag+1),
			    GM_PLUS, 2., 2.)
		    call gamove (gp, z1+p, asieval (asi, z1+lag+1))
		    dz = .1
		    for (x=z1+dz; x<=z2; x=x+dz)
			call gadraw (gp, x+p, asieval (asi, x+lag+1))
		    if (sf != sfbest) {
			asi = SF_ASI(SFD(sfbest,ix,iy))
			call gamove (gp, z1+p, asieval (asi, z1+lag+1))
			for (x=z1+dz; x<=z2; x=x+dz)
			    call gadraw (gp, x+p, asieval (asi, x+lag+1))
		    }

		    call gseti (gp, G_PLTYPE, 3)
		    call gline (gp, 0., 0., 0., 1.)
		    call gseti (gp, G_PLTYPE, 1)

		    if (nx <= NMAX && ny <= NMAX)
			call spf_label (gp, sfd, 31, 2)
		}
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

	# Draw label
	call gseti (gp, G_DRAWAXES, 0)
	call glabax (gp, Memc[str], "", "")

	call sfree (sp)
end


# SPF_G5 -- Spectra at a given sample

procedure spf_g5 (gp, sfcur, sfs, nimages, ix, iy, nx, ny)

pointer	gp		# GIO pointer
pointer	sfcur		# Current image
pointer	sfs[nimages]	# Images
int	nimages		# Number of images
int	ix, iy		# Sample
int	nx, ny		# Grid layout

int	i, j, k, npts
real	x1, x2, y1, y2, vx, dvx, vy, dvy, fa[10]
pointer	sp, str, sf, sfd, spec

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set subviewport parameters
	call ggview (gp, vx, dvx, vy, dvy)
	dvx = (dvx - vx) / nx
	dvy = (dvy - vy) / ny

	# Draw bounding box and label
	if (SF_NSFD(sfcur) > 1) {
	    call sprintf (Memc[str], SZ_LINE,
		"All Images at Sample [%d:%d,%d:%d]")
		call pargi (SF_X1(sfcur)+(ix-1)*SF_DX(sfcur))
		call pargi (SF_X1(sfcur)+ix*SF_DX(sfcur)-1)
		call pargi (SF_Y1(sfcur)+(iy-1)*SF_DY(sfcur))
		call pargi (SF_Y1(sfcur)+iy*SF_DY(sfcur)-1)
	} else
		 Memc[str] = EOS

	call gseti (gp, G_DRAWAXES, 0)
	call gseti (gp, G_DRAWTICKS, NO)
	call glabax (gp, Memc[str], "", "")
	call gseti (gp, G_DRAWAXES, 3)

	# Draw spectra
	k = 0
	do j = 1, ny {
	    do i = 1, nx {
		k = k + 1
		if (k > nimages)
		    break

		sf = sfs[k]
		sfd = SFD(sf, ix, iy)
		spec = SF_SPEC(sfd)
		if (SF_AXIS(sf) == 1)
		    npts = SF_DX(sf)
		else
		    npts = SF_DY(sf)

		x1 = 0.; x2 = 1.
		call alimr (Memr[spec], npts, y1, y2)
		y2 = y1 + (y2 - y1) * 1.5
		fa[1] = x1; fa[6] = y1
		fa[2] = x2; fa[7] = y1
		fa[3] = x2; fa[8] = y2
		fa[4] = x1; fa[9] = y2
		fa[5] = x1; fa[10] = y1
		call gswind (gp, x1, x2, y1, y2)

		if (sf == sfcur) {
		    call gsview (gp, vx+(i-1)*dvx+.005, vx+i*dvx-.005,
			vy+(ny-j)*dvy+.005, vy+(ny-j+1)*dvy-.005)
		    call gsetr (gp, G_PLWIDTH, HLWIDTH)
		    call gseti (gp, G_PLCOLOR, HLCOLOR)
		    call gpline (gp, fa, fa[6], 5)
		    call gsetr (gp, G_PLWIDTH, 1.)
		    call gseti (gp, G_PLCOLOR, 1)
		}

		call gsview (gp, vx+(i-1)*dvx, vx+i*dvx,
		    vy+(ny-j)*dvy, vy+(ny-j+1)*dvy)
		call glabax (gp, "", "", "")

		if (SF_DEL(sfd) == NO) {
		    call gvline (gp, Memr[spec], npts, x1, x2)

		    if (nx <= NMAX && ny <= NMAX)
			call spf_label (gp, sfd, 31, 2)
		}
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

	call sfree (sp)
end


# SPF_G6 -- Spectra at a given image

procedure spf_g6 (gp, sfcur, ix, iy)

pointer	gp		# GIO pointer
pointer	sfcur		# Current image
int	ix, iy		# Sample

int	i, j, nx, ny, npts
real	x1, x2, y1, y2, vx, dvx, vy, dvy, fa[10]
pointer	sp, str, sfd, spec

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	nx = SF_NX(sfcur)
	ny = SF_NY(sfcur)
	if (SF_AXIS(sfcur) == 1)
	    npts = SF_DX(sfcur)
	else
	    npts = SF_DY(sfcur)

	# Set subviewport parameters
	call ggview (gp, vx, dvx, vy, dvy)
	dvx = (dvx - vx) / nx
	dvy = (dvy - vy) / ny

	# Draw bounding box and label
	call sprintf (Memc[str], SZ_LINE, "Image %s with Focus %.3g")
	    call pargstr (SF_IMAGE(sfcur))
	    call pargr (SF_FOCUS(sfcur))

	call gseti (gp, G_DRAWTICKS, NO)
	call glabax (gp, Memc[str], "", "")

	# Draw spectra
	do j = 1, ny {
	    do i = 1, nx {
		sfd = SFD(sfcur,i,j)
		spec = SF_SPEC(sfd)

		x1 = 0.; x2 = 1.
		call alimr (Memr[spec], npts, y1, y2)
		y2 = y1 + (y2 - y1) * 1.5
		fa[1] = x1; fa[6] = y1
		fa[2] = x2; fa[7] = y1
		fa[3] = x2; fa[8] = y2
		fa[4] = x1; fa[9] = y2
		fa[5] = x1; fa[10] = y1
		call gswind (gp, x1, x2, y1, y2)

		if (i == ix && j == iy) {
		    call gsview (gp, vx+(i-1)*dvx+.005, vx+i*dvx-.005,
			vy+(j-1)*dvy+.005, vy+j*dvy-.005)
		    call gsetr (gp, G_PLWIDTH, HLWIDTH)
		    call gseti (gp, G_PLCOLOR, HLCOLOR)
		    call gpline (gp, fa, fa[6], 5)
		    call gsetr (gp, G_PLWIDTH, 1.)
		    call gseti (gp, G_PLCOLOR, 1)
		}

		call gsview (gp, vx+(i-1)*dvx, vx+i*dvx, vy+(j-1)*dvy, vy+j*dvy)
		call glabax (gp, "", "", "")

		if (SF_DEL(sfd) == NO) {
		    call gvline (gp, Memr[spec], npts, x1, x2)

		    if (nx <= NMAX && ny <= NMAX)
			call spf_label (gp, sfd, 21, 2)
		}
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5, 0.5+ny)
	call gamove (gp, 1., 1.)

	call sfree (sp)
end


# SPF_G7 -- Profile at a given image and sample

procedure spf_g7 (gp, sfbest, sfcur, ix, iy, lag)

pointer	gp		# GIO pointer
pointer	sfbest		# Best image
pointer	sfcur		# Current image
int	ix, iy		# Sample
int	lag		# Maximum lag

real	x1, x2, y1, y2, z1, z2, dz, x, p, s, asieval()
pointer	sp, str, sf, sfd, asi

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	sf = sfcur
	sfd = SFD(sf,ix,iy)
	asi = SF_ASI(sfd)

	s = 1 / sqrt (2.)
	x2 = s * min (lag, nint (3 * SF_WIDTH(sf)))
	x1 = -x2
	y1 = -0.05
	y2 = 1.05
	call gswind (gp, x1, x2, y1, y2)

	if (abs (SF_POS(sfd)) > .01) {
	    call sprintf (Memc[str], SZ_LINE,
	    "%s at (%d, %d), Focus %.3g, Width %.2f, Shift %.2f")
	    call pargstr (SF_IMAGE(sf))
	    call pargr (SF_X(sfd))
	    call pargr (SF_Y(sfd))
	    call pargr (SF_FOC(sfd))
	    call pargr (SF_WID(sfd))
	    call pargr (SF_POS(sfd))
	} else {
	    call sprintf (Memc[str], SZ_LINE,
		"%s at (%d, %d), Focus %.3g, Width %.2f")
	    call pargstr (SF_IMAGE(sf))
	    call pargr (SF_X(sfd))
	    call pargr (SF_Y(sfd))
	    call pargr (SF_FOC(sfd))
	    call pargr (SF_WID(sfd))
	}
	call glabax (gp, Memc[str], "Pixel", "Correlation")

	# Draw correlation profiles
	if (SF_DEL(sfd) == NO) {
	    p = SF_POS(sfd)
	    z1 = max (x1, x1-p)
	    z2 = min (x2, x2-p)
	    dz = s
	    for (x=s*nint(z1/s); x<=s*nint(z2/s); x=x+dz)
		call gmark (gp, x+p, asieval (asi, x/s+lag+1), GM_PLUS, 2., 2.)
	    call gamove (gp, z1+p, asieval (asi, z1/s+lag+1))
	    dz = .1 * s
	    for (x=z1+dz; x<=z2; x=x+dz)
		call gadraw (gp, x+p, asieval (asi, x/s+lag+1))
	    if (sf != sfbest) {
		asi = SF_ASI(SFD(sfbest,ix,iy))
		call gamove (gp, z1+p, asieval (asi, z1/s+lag+1))
		for (x=z1+dz; x<=z2; x=x+dz)
		    call gadraw (gp, x+p, asieval (asi, x/s+lag+1))
	    }

	    call gseti (gp, G_PLTYPE, 3)
	    call gline (gp, 0., y1, 0., y2)
	    call gseti (gp, G_PLTYPE, 1)
	}

	call sfree (sp)
end


# SPF_G8 -- Spatial distribution of widths

procedure spf_g8 (gp, sfavg, sfcur, sfdcur, sfs, nimages, nxgrid, nygrid)

pointer	gp		# GIO pointer
pointer	sfavg		# Average image
pointer	sfcur		# Current image
pointer	sfdcur		# Current sample
pointer	sfs[nimages]	# Images
int	nimages		# Number of images
int	nxgrid, nygrid	# Grid layout

int	nx, ny
int	i, j, k, l, m
real	x, y, z, x1, x2, y1, y2, z1, z2, fa[10]
pointer	sp, str, sf, sfd, kmin, kptr

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Set subviewport parameters
	nx = SF_NX(sfavg)
	ny = SF_NY(sfavg)
	call ggview (gp, x1, x2, y1, y2)
	x2 = (x2 - x1) / nxgrid
	y2 = (y2 - y1) / nygrid
	fa[1] = 0.; fa[6] = 0.
	fa[2] = 1.; fa[7] = 0.
	fa[3] = 1.; fa[8] = 1.
	fa[4] = 0.; fa[9] = 1.
	fa[5] = 0.; fa[10] = 0.

	call gseti (gp, G_DRAWTICKS, NO)

	# Find best focus at each sample and range of WID
	call salloc (kmin, nx*ny, TY_INT)
	kptr = kmin
	z1 = MAX_REAL
	z2 = -MAX_REAL
	do j = 1, ny {
	    do i = 1, nx {
		l = 0
		sfd = SFD(sfavg,i,j)
		if (SF_DEL(sfd) == NO) {
		    x = SF_FOC(sfd)
		    y = MAX_REAL
		    do k = 1, nimages {
			sfd = SFD(sfs[k],i,j)
			if (SF_DEL(sfd) == NO) {
			    z1 = min (z1, SF_WID(sfd))
			    z2 = max (z2, SF_WID(sfd))
			    z = abs (SF_FOC(sfd) - x)
			    if (z < y) {
				l = k
				y = z
			    }
			}
		    }
		}
		Memi[kptr] = l
		kptr = kptr + 1
	    }
	}
	z2 = max (2., z2 - z1)

	# Make graphs
	k = 0
	do j = 1, nygrid {
	    do i = 1, nxgrid {
		k = k + 1
		if (k > nimages)
		    break

		sf = sfs[k]
		nx = SF_NX(sfs[1])
		ny = SF_NY(sfs[1])

		if (sf == sfcur) {
		    call gsview (gp, x1+(i-1)*x2+.005, x1+i*x2-.005,
			y1+(nygrid-j)*y2+.005, y1+(nygrid-j+1)*y2-.005)
		    call gswind (gp, 0., 1., 0., 1.)
		    call gsetr (gp, G_PLWIDTH, HLWIDTH)
		    call gseti (gp, G_PLCOLOR, HLCOLOR)
		    call gpline (gp, fa, fa[6], 5)
		    call gsetr (gp, G_PLWIDTH, 1.)
		    call gseti (gp, G_PLCOLOR, 1)
		}

		call gsview (gp, x1+(i-1)*x2, x1+i*x2,
		    y1+(nygrid-j)*y2, y1+(nygrid-j+1)*y2)
		call gswind (gp, 0.5, 0.5+nx, 0.5, 1.5+ny)
		call glabax (gp, "", "", "")

		kptr = kmin
		do m = 1, ny {
		    do l = 1, nx {
			sfd = SFD(sf,l,m)
			if (SF_DEL(sfd) == NO) {
			    x = l
			    y = m
			    z = SF_WID(sfd)
			    z = 0.008 * (1 + (z - z1) / z2 * 3)
			    call gmark (gp, x, y, GM_CIRCLE, z, z)
			    if (Memi[kptr] == k)
				call gmark (gp, x, y, GM_PLUS, z, z)
			    if (sfd == sfdcur) {
				call gseti (gp, G_PLCOLOR, HLCOLOR)
				call gmark (gp, x, y, GM_BOX, -.8, -.8)
				call gseti (gp, G_PLCOLOR, 1)
			    }
			}
			kptr = kptr + 1
		    }
		}
			
		if (nxgrid <= NMAX && nygrid <= NMAX)
		    call spf_label (gp, sfd, 11, 2)
	    }
	}

	call gsview (gp, x1, x1+nxgrid*x2, y1, y1+nygrid*y2)
	call gswind (gp, 0.5, 0.5+nxgrid, 0.5+nygrid, 0.5)
	call gamove (gp, 1., 1.)

	call sfree (sp)
end


# SPF_G9 -- Spectrum at a given image and sample

procedure spf_g9 (gp, sfcur, ix, iy)

pointer	gp		# GIO pointer
pointer	sfcur		# Current image
int	ix, iy		# Sample

int	npts
real	x1, x2
pointer	sf, sfd, spec

begin
	sf = sfcur
	sfd = SFD(sf,ix,iy)
	spec = SF_SPEC(sfd)
	if (SF_AXIS(sf) == 1) {
	    npts = SF_DX(sf)
	    x1 = SF_X1(sf) + (ix - 1) * npts
	} else {
	    npts = SF_DY(sf)
	    x1 = SF_Y1(sf) + (iy - 1) * npts
	}
	x2 = x1 + npts - 1
	call gswind (gp, x1, x2, INDEF, INDEF)
	call gascale (gp, Memr[spec], npts, 2)

	if (SF_AXIS(sf) == 1)
	    call glabax (gp, "", "Column", "")
	else
	    call glabax (gp, "", "Line", "")
	if (SF_DEL(sfd) == NO)
	    call gvline (gp, Memr[spec], npts, x1, x2)
end


# SPF_LABEL -- Label

procedure spf_label (gp, sfd, type, width)

pointer	gp		# GIO pointer
pointer	sfd		# Sample pointer
int	type		# Label type
int	width		# Line width

int	bkup, gstati()
real	x1, x2, y1, y2, xs, ys, x, y
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	bkup = gstati (gp, G_PLWIDTH)
	call gseti (gp, G_PLWIDTH, width)
	call ggwind (gp, x1, x2, y1, y2)
	call ggscale (gp, x1, y1, xs, ys)
	call gline (gp, x1, y1, x1, y1)

	switch (type) {
	case 11: # 1 across the top
	    x = x2 - 0.01 * xs; y = y2 - 0.01 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SF_FOC(sfd))
	    call gtext (gp, x, y, Memc[str], "h=r;v=t")
	case 21: # 2 across the top
	    x = x1 + 0.01 * xs; y = y2 - 0.01 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.2f")
		call pargr (SF_WID(sfd))
	    call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    if (abs (SF_POS(sfd)) >= .01) {
		x = x2 - 0.01 * xs; y = y2 - 0.01 * ys
		call sprintf (Memc[str], SZ_LINE, "%.2f")
		    call pargr (SF_POS(sfd))
		call gtext (gp, x, y, Memc[str], "h=r;v=t")
	    }
	case 31: # 3 across the top
	    x = x1 + 0.01 * xs; y = y2 - 0.01 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.2f")
		call pargr (SF_WID(sfd))
	    call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    x = x2 - 0.01 * xs; y = y2 - 0.01 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SF_FOC(sfd))
	    call gtext (gp, x, y, Memc[str], "h=r;v=t")
	    if (abs (SF_POS(sfd)) >= .01) {
		x = (x1 + x2) / 2; y = y2 - 0.01 * ys
		call sprintf (Memc[str], SZ_LINE, "%.2f")
		    call pargr (SF_POS(sfd))
		call gtext (gp, x, y, Memc[str], "h=c;v=t")
	    }
	case 12: # 2 along the left
	    x = x1 + 0.02 * xs; y = y2 - 0.02 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.2f")
		call pargr (SF_WID(sfd))
	    call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    if (abs (SF_POS(sfd)) >= .01) {
		x = x1 + 0.02 * xs; y = y2 - 0.06 * ys
		call sprintf (Memc[str], SZ_LINE, "%.2f")
		    call pargr (SF_POS(sfd))
		call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    }
	case 13: # 3 along the left
	    x = x1 + 0.02 * xs; y = y2 - 0.02 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.2f")
		call pargr (SF_WID(sfd))
	    call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    x = x1 + 0.02 * xs; y = y2 - 0.10 * ys
	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SF_FOC(sfd))
	    call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    if (abs (SF_POS(sfd)) >= .01) {
		x = x1 + 0.02 * xs; y = y2 - 0.06 * ys
		call sprintf (Memc[str], SZ_LINE, "%.2f")
		    call pargr (SF_POS(sfd))
		call gtext (gp, x, y, Memc[str], "h=l;v=t")
	    }
	}

	call gseti (gp, G_PLWIDTH, bkup)

	call sfree (sp)
end


# SPF_SAMPLE -- Find the nearest sample to the cursor position

procedure spf_sample (sfs, nimages, del, wx, wy, i, j, k)

pointer	sfs[nimages]	#I Images
int	nimages		#I Number of images
int	del		#I Deletion flag
real	wx, wy		#I Cursor coordinate
int	i, j, k		#O Nearest sample and image

int	i1, j1, k1, k2
real	r, rmin

begin
	rmin = MAX_REAL
	k1 = k
	do k2 = 0, 2 * nimages {
	    if (mod (k2, 2) == 0)
		k1 = k1 + k2
	    else
		k1 = k1 - k2
	    if (k1 < 1 || k1 > nimages)
		next
	    do j1 = 1, SF_NY(sfs[k1]) {
		do i1 = 1, SF_NX(sfs[k1]) {
		    if (SF_DEL(SFD(sfs[k1],i1,j1)) == del) {
			r = (i1 - wx) ** 2 + (j1 - wy) ** 2
			if (r < rmin) {
			    i = i1
			    j = j1
			    k = k1
			    rmin = r
			}
		    }
		}
	    }

	    if (rmin < MAX_REAL)
		break
	}
end
