include	<error.h>
include	<gset.h>
include	<mach.h>
include	"starfocus.h"

# Interactive help files.  There is one for STARFOCUS and one for PSFMEASUE.
define	STFHELP		"starfocus$stfhelp.key"
define	PSFHELP		"starfocus$psfhelp.key"
define	PROMPT		"Options"

# View ports for all plots.
define	VX1	.15			# Minimum X viewport for left graph
define	VX2	.47			# Maximum X viewport for left graph
define	VX3	.63			# Minimum X viewport for right graph
define	VX4	.95			# Maximum X viewport for right graph
define	VY1	.10			# Minimum Y viewport for bottom graph
define	VY2	.44			# Minimum Y viewport for bottom graph
define	VY3	.54			# Minimum Y viewport for top graph
define	VY4	.88			# Maximum Y viewport for top graph

# Miscellaneous graphics parameters.
define	NMAX	5			# Maximum number of samples for labeling
define	HLCOLOR	2			# Highlight color
define	HLWIDTH	4.			# Highlight width
define	GM_MARK	GM_CROSS		# Point marker
define	GM_MAG	GM_PLUS+GM_CROSS	# Magnitude marker


# STF_GRAPH -- Interactive graphing of results.

procedure stf_graph (sf)

pointer	sf			#I Starfocus structure

real	wx, wy, x, y, r2, r2min, fa[8]
int	i, j, ix, iy, nx, ny, wcs, key, pkey, skey, redraw, clgcur()
pointer	sp, sysidstr, title, cmd, gp, gopen()
pointer	sfd, sfs, sff, current, nearest

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	call smark (sp)
	call salloc (sysidstr, SZ_LINE, TY_CHAR)
	call salloc (title, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	# Set system id label
	call sysid (Memc[sysidstr], SZ_LINE)

	# Open graphics and enter interactive graphics loop
	SF_GP(sf) = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	gp = SF_GP(sf)
	wcs = 0
	if (SF_NF(sf) > 1)
	    key = 'f'
	else if (SF_NS(sf) > 1)
	    key = 'a'
	else
	    key = 'z'
	pkey = 0
	skey = 1
	current = SF_BEST(sf)
	repeat {
	    switch (key) {
	    case 'q':	# Quit
		break
	    case '?':	# Help
		if (SF_TASK(sf) == PSFMEASURE)
		    call gpagefile (gp, PSFHELP, PROMPT)
		else
		    call gpagefile (gp, STFHELP, PROMPT)
		next
	    case ':':	# Colon commands
		iferr (call stf_colon (sf, Memc[cmd], redraw))
		    redraw = NO
		if (redraw == NO)
		    next
	    case 'a', 'b', 'e', 'f', 'g', 'm', 'p', 't', 'z':	# Plots
		# When there is not enough data for the requested plot
		# map the key to another one.  This is done mostly to
		# avoid redrawing the same graph when different keys
		# map to the same pkey.  The 'e', 'g', and 'p' key may
		# select a different object so the check for the same
		# plot is deferred.

		if (SF_NS(sf) > 1 && SF_NF(sf) > 1) {
		    ;
		} else if (SF_NS(sf) > 1) {
		    if (key == 'b')
			key = 'a'
		    if (key == 'f')
			key = 'm'
		} else if (SF_NF(sf) > 1) {
		    if (key == 'a' || key == 'b' || key == 'm' || key == 't')
			key = 'f'
		} else {
		    key = 'z'
		}

		switch (key) {
		case 'e', 'g', 'p':
		    ;
		default:
		    if (key == pkey)
			next
		}
	    case 's':	# Toggle plotting of magnitude symbols
		if (pkey != 'a' && pkey != 'b')
		    next
		skey = mod (skey+1, 2)
	    case 'u':	# Undelete all
		j = 0
		do i = 1, SF_NSFD(sf) {
		    sfd = SF_SFD(sf,i)
		    if (SFD_STATUS(sfd) != 0) {
			SFD_STATUS(sfd) = 0
			j = j + 1
		    }
		}
		if (j == 0)
		    next
		call stf_fitfocus (sf)
	    case 'd', 'n', 'o', 'r', 'x', 'i', ' ':	# Misc
	        ;
	    default:	# Unknown
		call printf ("\007")
		next
	    }

	    # Find the nearest or next object if needed.
	    switch (key) {
	    case 'r', 's', 'u', ':':	# Redraw last graph
		pkey = pkey
		nearest = current
	    case 'n', 'o':		# Renormalize enclosed flux profile
		if (wcs != 7 || pkey == 'p')
		    next
		pkey = pkey
		nearest = current
		if (key == 'n')
		    call stf_norm (sf, nearest, wx, INDEF)
		else
		    call stf_norm (sf, nearest, wx, wy)
		call stf_widths (sf, nearest)
		call stf_fwhms (sf, nearest)
		call stf_fitfocus (sf)
	    case ' ':			# Select next focus or star
		switch (pkey) {
		case 'a', 'm', 't':
		    sff = SFD_SFF(current)
		    for (i=1; SF_SFF(sf,i)!=sff; i=i+1)
			;
		    j = SF_NFOCUS(sf)
		    i = mod (i, j) + 1
		    for (; SFF_N(SF_SFF(sf,i))==0; i=mod(i,j)+1)
			;
		    if (SF_SFF(sf,i) == sff)
			next
		    sff = SF_SFF(sf,i)
		    do i = 1, SFF_NSFD(sff) {
			nearest = SFF_SFD(sff,i)
			if (SFD_STATUS(nearest) == 0)
			    break
		    }
		case 'e', 'g', 'p', 'z':
		    switch (wcs) {
		    case 7, 8, 11:
			for (i=1; SF_SFD(sf,i)!=current; i=i+1)
			    ;
			j = SF_NSFD(sf)
			i = mod (i, j) + 1
			for (; SFD_STATUS(SF_SFD(sf,i))!=0; i=mod(i,j)+1)
			    ;
			nearest = SF_SFD(sf,i)
		    case 9:
			sfs = SFD_SFS(current)
			for (i=1; SFS_SFD(sfs,i)!=current; i=i+1)
			    ;
			j = SFS_NSFD(sfs)
			i = mod (i, j) + 1
			for (; SFD_STATUS(SFS_SFD(sfs,i))!=0; i=mod(i,j)+1)
			    ;
			nearest = SFS_SFD(sfs,i)
			if (nearest == current)
			    next
		    case 10:
			sff = SFD_SFF(current)
			for (i=1; SFF_SFD(sff,i)!=current; i=i+1)
			    ;
			j = SFF_NSFD(sff)
			i = mod (i, j) + 1
			for (; SFD_STATUS(SFF_SFD(sff,i))!=0; i=mod(i,j)+1)
			    ;
			nearest = SFF_SFD(sff,i)
			if (nearest == current)
			    next
		    }
		default:
		    next
		}
	    default:			# Select nearest to cursor
		switch (pkey) {
		case 'a':
		    r2min = MAX_REAL
		    call gctran (gp, wx, wy, wx, wy, wcs, 0)
		    sff = SFD_SFF(current)
		    do i = 1, SFF_NSFD(sff) {
			sfd = SFF_SFD(sff,i)
			if (SFD_STATUS(sfd) != 0)
			    next
			switch (wcs) {
			case 1:
			    x = SFD_X(sfd)
			    y = SFD_Y(sfd)
			case 2:
			    x = SFD_X(sfd)
			    y = SFD_W(sfd)
			case 3:
			    x = SFD_W(sfd)
			    y = SFD_Y(sfd)
			case 4:
			    x = SFD_X(sfd)
			    y = SFD_E(sfd)
			case 5:
			    x = SFD_E(sfd)
			    y = SFD_Y(sfd)
			}
			call gctran (gp, x, y, x, y, wcs, 0)
			r2 = (x-wx)**2 + (y-wy)**2
			if (r2 < r2min) {
			    r2min = r2
			    nearest = sfd
			}
		    }
		case 'b': 
		    r2min = MAX_REAL
		    call gctran (gp, wx, wy, wx, wy, wcs, 0)
		    do i = 1, SF_NSTARS(sf) {
			sfs = SF_SFS(sf,i)
			if (SFS_N(sfs) == 0)
			    next
			switch (wcs) {
			case 1:
			    x = SFD_X(SFS_SFD(sfs,1))
			    y = SFD_Y(SFS_SFD(sfs,1))
			case 2:
			    x = SFD_X(SFS_SFD(sfs,1))
			    y = SFS_W(sfs)
			case 3:
			    x = SFS_W(sfs)
			    y = SFD_Y(SFS_SFD(sfs,1))
			case 4:
			    x = SFD_X(SFS_SFD(sfs,1))
			    y = SFS_F(sfs)
			case 5:
			    x = SFS_F(sfs)
			    y = SFD_Y(SFS_SFD(sfs,1))
			}
			call gctran (gp, x, y, x, y, wcs, 0)
			r2 = (x-wx)**2 + (y-wy)**2
			if (r2 < r2min) {
			    r2min = r2
			    nearest = sfs
			}
		    }
		    sfs = nearest
		    r2min = MAX_REAL
		    do i = 1, SFS_NSFD(sfs) {
			sfd = SFS_SFD(sfs,i)
			if (SFD_STATUS(sfd) != 0)
			    next
			r2 = SFD_W(sfd)
			if (r2 < r2min) {
			    r2min = r2
			    nearest = sfd
			}
		    }
		case 'e', 'g', 'p':
		    switch (wcs) {
		    case 9:
			sfs = SFD_SFS(current)
			i = SFS_N(sfs)
			if (i < 4) {
			    nx = i
			    ny = 1
			} else {
			    nx = nint (sqrt (real (i)))
			    if (mod (i-1, nx+1) >= mod (i-1, nx))
				nx = nx + 1
			    ny = (i - 1) / nx + 1
			}
			ix = max (1, min (nx, nint(wx)))
			iy = max (1, min (ny, nint(wy)))

			j = 0
			do i = 1, SFS_NSFD(sfs) {
			    sfd = SFS_SFD(sfs, i)
			    if (SFD_STATUS(sfd) != 0)
				next
			    if (ix == 1 + mod (j, nx) && iy == 1 + j / nx) {
				nearest = sfd
				break
			    }
			    j = j + 1
			}
		    case 10:
			sff = SFD_SFF(current)
			i = SFF_N(sff)
			if (i < 4) {
			    nx = i
			    ny = 1
			} else {
			    nx = nint (sqrt (real (i)))
			    if (mod (i-1, nx+1) >= mod (i-1, nx))
				nx = nx + 1
			    ny = (i - 1) / nx + 1
			}
			ix = max (1, min (nx, nint(wx)))
			iy = max (1, min (ny, nint(wy)))

			j = 0
			do i = 1, SFF_NSFD(sff) {
			    sfd = SFF_SFD(sff, i)
			    if (SFD_STATUS(sfd) != 0)
				next
			    if (ix == 1 + mod (j, nx) && iy == 1 + j / nx) {
				nearest = sfd
				break
			    }
			    j = j + 1
			}
		    }
		    if (key == pkey && nearest == current)
			next
		default:
		    switch (wcs) {
		    case 1, 2:
			r2min = MAX_REAL
			call gctran (gp, wx, wy, wx, wy, wcs, 0)
			do i = 1, SF_NSFD(sf) {
			    sfd = SF_SFD(sf,i)
			    if (SFD_STATUS(sfd) != 0)
				next
			    switch (wcs) {
			    case 1:
				x = SFD_F(sfd)
				y = SFD_W(sfd)
			    case 2:
				x = SFD_F(sfd)
				y = SFD_E(sfd)
			    }
			    call gctran (gp, x, y, x, y, wcs, 0)
			    r2 = (x-wx)**2 + (y-wy)**2
			    if (r2 < r2min) {
				r2min = r2
				nearest = sfd
			    }
			}
		    case 3, 4, 5, 6:
			r2min = MAX_REAL
			call gctran (gp, wx, wy, wx, wy, wcs, 0)
			sff = SFD_SFF(current)
			do i = 1, SFF_NSFD(sff) {
			    sfd = SFF_SFD(sff,i)
			    if (SFD_STATUS(sfd) != 0)
				next
			    switch (wcs) {
			    case 3:
				x = -2.5 * log10 (SFS_M(SFD_SFS(sfd))/SF_M(sf))
				y = SFD_W(sfd)
			    case 4:
				x = -2.5 * log10 (SFS_M(SFD_SFS(sfd))/SF_M(sf))
				y = SFD_E(sfd)
			    case 5:
				x = sqrt ((SFD_X(sfd) - SF_XF(sf)) ** 2 +
				    (SFD_Y(sfd) - SF_YF(sf)) ** 2)
				y = SFD_W(sfd)
			    case 6:
				x = sqrt ((SFD_X(sfd) - SF_XF(sf)) ** 2 +
				    (SFD_Y(sfd) - SF_YF(sf)) ** 2)
				y = SFD_E(sfd)
			    }
			    call gctran (gp, x, y, x, y, wcs, 0)
			    r2 = (x-wx)**2 + (y-wy)**2
			    if (r2 < r2min) {
				r2min = r2
				nearest = sfd
			    }
			}
		    default:
			nearest = current
		    }
		}

		# Act on selection for delete or info.
		switch (key) {
		case 'd':
		    if (SF_NS(sf) > 1) {
			sfs = SFD_SFS(nearest)
			do i = 1, SFS_NSFD(sfs)
			    SFD_STATUS(SFS_SFD(sfs,i)) = 1
		    } else
			SFD_STATUS(nearest) = 1
		    call stf_fitfocus (sf)
		case 'x':
		    repeat {
			switch (key) {
			case 'f':
			    sff = SFD_SFF(nearest)
			    do i = 1, SFF_NSFD(sff)
				SFD_STATUS(SFF_SFD(sff,i)) = 1
			case 'i':
			    sfd = SFD_SFI(nearest)
			    do i = 1, SFI_NSFD(sfd)
				SFD_STATUS(SFI_SFD(sfd,i)) = 1
			case 'p':
			    SFD_STATUS(nearest) = 1
			case 's':
			    sfs = SFD_SFS(nearest)
			    do i = 1, SFS_NSFD(sfs)
				SFD_STATUS(SFS_SFD(sfs,i)) = 1
			default:
			    call printf (
			    "Delete image, star, focus, or point? (i|s|f|p)")
			    next
			}
			call stf_fitfocus (sf)
			break
		    } until (clgcur ("graphcur",
			wx, wy, wcs, key, Memc[cmd], SZ_LINE) == EOF)
		case 'i':
		    switch (pkey) {
		    case 'b':
			sfs = SFD_SFS(nearest)
			call stf_title (sf, NULL, sfs, NULL,
			    Memc[title], SZ_LINE)
		    default:
			call stf_title (sf, nearest, NULL, NULL,
			    Memc[title], SZ_LINE)
		    }
		    call printf ("%s\n")
			call pargstr (Memc[title])
		    next
		default:
		    pkey = key
		}
	    }

	    # If current object has been deleted select another.
	    if (SFD_STATUS(nearest) == 0)
		current = nearest
	    else
		current = SF_BEST(sf)

	    # Make the graphs.  The graph depends on the number of stars
	    # and number of focus values.  Note that the pkey has already
	    # been mapped but all the keys are shown for clarity.

	    call gclear (gp)
	    call gseti (gp, G_FACOLOR, 0)

	    if (SF_NS(sf) > 1 && SF_NF(sf) > 1) {
		switch (pkey) {
		case 'a':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX4, VY1, VY4)
		    call stf_g11 (sf, current, skey, Memc[title])
		case 'b':
		    call sprintf (Memc[title], SZ_LINE,
			"Best focus estimates for each star")
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX4, VY1, VY4)
		    call stf_g12 (sf, current, skey, Memc[title])
		case 'e':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g2 (sf, current, Memc[title])
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call stf_g3 (sf, current, Memc[title])
		case 'f':
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 'f', 'r', "", "", SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 2)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 'f', 'e', "", "Focus",
			"Ellipticity")
		case 'g':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g9 (sf, current, Memc[title])
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call stf_g10 (sf, current, Memc[title])
		case 'm':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 3)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 'm', 'r', Memc[title],
			"", SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 4)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 'm', 'e', "", "Magnitude",
			"Ellipticity")
		case 'p':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g4 (sf, current, Memc[title])
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call stf_g5 (sf, current, Memc[title])
		case 't':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 5)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 't', 'r', Memc[title],
			"", SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 6)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 't', 'e', "", "Field radius",
			"Ellipticity")
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call stf_g6 (sf, current, "", "", "Enclosed flux")
		    call gseti (gp, G_WCS, 8)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, "", "Radius", "Profile")
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX3, VX4, VY3, VY4)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, "", "Enclosed flux", "FWHM")

		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 0)
		    call gsetr (gp, G_PLWIDTH, 2.0)
		    call gline (gp, 0., 0., 0., 0.)
		    call gtext (gp, 0.5, 0.93, Memc[title], "h=c,v=t")
		}
	    } else if (SF_NS(sf) > 1) {
		switch (pkey) {
		case 'a', 'b':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX4, VY1, VY4)
		    call stf_g11 (sf, current, skey, Memc[title])
		case 'e':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g3 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g6 (sf, current, Memc[title], "Radius",
			"Enclosed flux")
		case 'f', 'm':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 3)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 'm', 'r', Memc[title], "",
			SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 4)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 'm', 'e', "", "Magnitude",
			"Ellipticity")
		case 'g':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g10 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, Memc[title], "Enclosed flux",
			"FWHM")
		case 'p':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 10)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g5 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, Memc[title], "Radius", "Profile")
		case 't':
		    sff = SFD_SFF(current)
		    call stf_title (sf, NULL, NULL, sff, Memc[title], SZ_LINE)
		    call gseti (gp, G_WCS, 5)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 't', 'r', Memc[title], "",
			SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 6)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 't', 'e', "", "Field radius",
			"Ellipticity")
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call stf_g6 (sf, current, "", "", "Enclosed flux")
		    call gseti (gp, G_WCS, 8)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, "", "Radius", "Profile")
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX3, VX4, VY3, VY4)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, "", "Enclosed flux", "FWHM")

		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 0)
		    call gsetr (gp, G_PLWIDTH, 2.0)
		    call gline (gp, 0., 0., 0., 0.)
		    call gtext (gp, 0.5, 0.93, Memc[title], "h=c,v=t")
		}
	    } else if (SF_NF(sf) > 1) {
		switch (pkey) {
		case 'a', 'b', 'f', 'm', 't':
		    call gseti (gp, G_WCS, 1)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g1 (sf, current, 'f', 'r', "", "", SF_WTYPE(sf))
		    call gseti (gp, G_WCS, 2)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g1 (sf, current, 'f', 'e', "", "Focus",
			"Ellipticity")
		case 'e':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g2 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g6 (sf, current, Memc[title], "Radius",
			"Enclosed flux")
		case 'g':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g9 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, Memc[title], "Enclosed flux",
			"FWHM")
		case 'p':
		    sfs = SFD_SFS(current)
		    call sprintf (Memc[title], SZ_LINE,
			"Star: x=%.2f, y=%.2f, m=%.2f")
			call pargr (SFD_X(current))
			call pargr (SFD_Y(current))
			call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
		    call gseti (gp, G_WCS, 9)
		    call gsview (gp, VX1, VX4, VY3, VY4)
		    call stf_g4 (sf, current, Memc[title])
		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX4, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, Memc[title], "Radius",
			"profile")
		case 'z':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call stf_g6 (sf, current, "", "", "Enclosed flux")
		    call gseti (gp, G_WCS, 8)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, "", "Radius", "Profile")
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX3, VX4, VY3, VY4)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, "", "Enclosed flux", "FWHM")

		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 0)
		    call gsetr (gp, G_PLWIDTH, 2.0)
		    call gline (gp, 0., 0., 0., 0.)
		    call gtext (gp, 0.5, 0.93, Memc[title], "h=c,v=t")
		}
	    } else {
		switch (pkey) {
		case 'a', 'b', 'f', 'm', 'p', 'z', 'e', 't':
		    call gseti (gp, G_WCS, 7)
		    call gsview (gp, VX1, VX2, VY3, VY4)
		    call stf_g6 (sf, current, "", "", "Enclosed flux")
		    call gseti (gp, G_WCS, 8)
		    call gsview (gp, VX1, VX2, VY1, VY2)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g7 (sf, current, "", "Radius", "Profile")
		    call gseti (gp, G_WCS, 11)
		    call gsview (gp, VX3, VX4, VY3, VY4)
		    call gfill (gp, fa, fa[5], 4, GF_SOLID)
		    call stf_g8 (sf, current, "", "Enclosed flux", "FWHM")

		    call stf_title (sf, current, NULL, NULL, Memc[title],
			SZ_LINE)
		    call gseti (gp, G_WCS, 0)
		    call gsetr (gp, G_PLWIDTH, 2.0)
		    call gline (gp, 0., 0., 0., 0.)
		    call gtext (gp, 0.5, 0.93, Memc[title], "h=c,v=t")
		}
	    }

	    # Add banner title.
	    call stf_title (sf, NULL, NULL, NULL, Memc[title], SZ_LINE)
	    call gseti (gp, G_WCS, 0)
	    call gsetr (gp, G_PLWIDTH, 2.0)
	    call gline (gp, 0., 0., 0., 0.)
	    call gtext (gp, 0.5, 0.99, Memc[sysidstr], "h=c,v=t")
	    call gtext (gp, 0.5, 0.96, Memc[title], "h=c,v=t")

	    if (SF_NSFD(sf) == 1)
		break

	} until (clgcur ("graphcur", wx, wy, wcs, key, Memc[cmd], SZ_LINE)==EOF)

	call gclose (gp)
	call sfree (sp)
end


# List of colon commands.
define	CMDS		"|show|level|size|scale|radius|xcenter|ycenter\
			|overplot|beta|"
define	SHOW		1	# Show current results
define	LEVEL		2	# Measurement level
define	SIZE		3	# Size type
define	SCALE		4	# Pixel scale
define	RADIUS		5	# Maximum radius
define	XCENTER		6	# X field center
define	YCENTER		7	# Y field center
define	OVERPLOT	8	# Overplot best profile
define	BETA		9	# Beta value for Moffat function

# STF_COLON -- Respond to colon command.

procedure stf_colon (sf, cmd, redraw)

pointer	sf			#I Starfocus pointer
char	cmd[ARB]		#I Colon command
int	redraw			#O Redraw?

bool	bval
real	rval, stf_r2i()
int	i, j, ncmd, nscan(), strdic(), open(), btoi()
pointer	sp, str, sfd
errchk	open, delete, stf_log, stf_norm, stf_radius, stf_fitfocus

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Scan the command string and get the first word.
	call sscan (cmd)
	call gargwrd (Memc[str], SZ_FNAME)
	ncmd = strdic (Memc[str], Memc[str], SZ_FNAME, CMDS)

	switch (ncmd) {
	case SHOW:
	    call gargwrd (Memc[str], SZ_FNAME)
	    iferr {
		if (nscan() == 1) {
		    call mktemp ("tmp$iraf", Memc[str], SZ_FNAME)
		    i = open (Memc[str], APPEND, TEXT_FILE)
		    call stf_log (sf, i)
		    call close (i)
		    call gpagefile (SF_GP(sf), Memc[str], "starfocus")
		    call delete (Memc[str])
		} else {
		    i = open (Memc[str], APPEND, TEXT_FILE)
		    call stf_log (sf, i)
		    call close (i)
		}
	    } then
		call erract (EA_WARN)
	    redraw = NO
	case LEVEL:
	    call gargr (rval)
	    if (nscan() == 2) {
		if (rval > 1.)
		    rval = rval / 100.
		SF_LEVEL(sf) = max (0.05, min (0.95, rval))
		do i = 1, SF_NSFD(sf) {
		    sfd = SF_SFD(sf,i)
		    call stf_radius (sf, sfd, SF_LEVEL(sf), SFD_R(sfd))
		}
		if (SF_WCODE(sf) == 1)
		    call stf_fitfocus (sf) 
		redraw = YES
	    } else {
		call printf ("level %g\n")
		    call pargr (SF_LEVEL(sf))
		redraw = NO
	    }
	case SIZE:
	    call gargwrd (Memc[str], SZ_FNAME)
	    if (nscan() == 2) {
		ncmd = strdic (Memc[str], Memc[str], SZ_FNAME, SF_WTYPES)
		if (ncmd == 0) {
		    call eprintf ("Invalid size type\n")
		    redraw = NO
		} else {
		    call strcpy (Memc[str], SF_WTYPE(sf), SF_SZWTYPE)
		    SF_WCODE(sf) = ncmd
		    do i = 1, SF_NSFD(sf) {
			sfd = SF_SFD(sf,i)
			switch (SF_WCODE(sf)) {
			case 1:
			    SFD_W(sfd) = SFD_R(sfd)
			case 2:
			    SFD_W(sfd) = SFD_DFWHM(sfd)
			case 3:
			    SFD_W(sfd) = SFD_GFWHM(sfd)
			case 4:
			    SFD_W(sfd) = SFD_MFWHM(sfd)
			}
			call stf_fwhms (sf, sfd)
		    }
		    call stf_fitfocus (sf) 
		    redraw = YES
		}
	    } else {
		call printf ("size %s\n")
		    call pargstr (SF_WTYPE(sf))
		redraw = NO
	    }
	case SCALE:
	    call gargr (rval)
	    if (nscan() == 2) {
		rval = rval / SF_SCALE(sf)
		SF_SCALE(sf) = SF_SCALE(sf) * rval
		do i = 1, SF_NSFD(sf) {
		    sfd = SF_SFD(sf,i)
		    switch (SF_WCODE(sf)) {
		    case 1:
			SFD_R(sfd) = SFD_R(sfd) * rval
			SFD_W(sfd) = SFD_R(sfd)
		    case 2:
			SFD_DFWHM(sfd) = SFD_DFWHM(sfd) * rval
			SFD_W(sfd) = SFD_DFWHM(sfd)
		    case 3:
			SFD_SIGMA(sfd) = SFD_SIGMA(sfd) * rval
			SFD_GFWHM(sfd) = SFD_GFWHM(sfd) * rval
			SFD_W(sfd) = SFD_GFWHM(sfd)
		    case 4:
			SFD_ALPHA(sfd) = SFD_ALPHA(sfd) * rval
			SFD_MFWHM(sfd) = SFD_MFWHM(sfd) * rval
			SFD_W(sfd) = SFD_MFWHM(sfd)
		    }
		    do j = 1, 19
			SFD_FWHM(sfd,j) = SFD_FWHM(sfd,j) * rval
		}
		do i = 1, SF_NSTARS(sf) {
		    sfd = SF_SFS(sf,i)
		    SFS_W(sfd) = SFS_W(sfd) * rval
		}
		do i = 1, SF_NFOCUS(sf) {
		    sfd = SF_SFF(sf,i)
		    SFF_W(sfd) = SFF_W(sfd) * rval
		}
		SF_W(sf) = SF_W(sf) * rval
		redraw = YES
	    } else {
		call printf ("scale %g\n")
		    call pargr (SF_SCALE(sf))
		redraw = NO
	    }
	case RADIUS:
	    call gargr (rval)
	    if (nscan() == 2) {
		j = stf_r2i (rval) + 1
		SF_RADIUS(sf) = rval
		do i = 1, SF_NSFD(sf) {
		    sfd = SF_SFD(sf,i)
		    if (j > SFD_NPMAX(sfd))
			next
		    SFD_NP(sfd) = j
		    SFD_RADIUS(sf) = SF_RADIUS(sf)
		    call stf_norm (sf, sfd, INDEF, INDEF)
		    call stf_widths (sf, sfd)
		    call stf_fwhms (sf, sfd)
		}
		call stf_fitfocus (sf)
		redraw = YES
	    } else {
		call printf ("radius %g\n")
		    call pargr (SF_RADIUS(sf))
		redraw = NO
	    }
	case XCENTER:
	    call gargr (rval)
	    if (nscan() == 2) {
		if (IS_INDEF(rval))
		    SF_XF(sf) = (SF_NCOLS(sf) + 1) / 2.
		else
		    SF_XF(sf) = rval
		redraw = NO
	    } else {
		call printf ("xcenter %g\n")
		    call pargr (SF_XF(sf))
		redraw = NO
	    }
	case YCENTER:
	    call gargr (rval)
	    if (nscan() == 2) {
		if (IS_INDEF(rval))
		    SF_YF(sf) = (SF_NLINES(sf) + 1) / 2.
		else
		    SF_YF(sf) = rval
		redraw = NO
	    } else {
		call printf ("ycenter %g\n")
		    call pargr (SF_YF(sf))
		redraw = NO
	    }
	case OVERPLOT:
	    call gargb (bval)
	    if (nscan() == 2) {
		SF_OVRPLT(sf) = btoi (bval)
		redraw = YES
	    } else {
		call printf ("overplot %b\n")
		    call pargi (SF_OVRPLT(sf))
		redraw = NO
	    }
	case BETA:
	    call gargr (rval)
	    if (nscan() == 2) {
		SF_BETA(sf) = rval
		do i = 1, SF_NSFD(sf) {
		    sfd = SF_SFD(sf,i)
		    call stf_widths (sf, sfd)
		    switch (SF_WCODE(sf)) {
		    case 1:
			SFD_W(sfd) = SFD_R(sfd)
		    case 2:
			SFD_W(sfd) = SFD_DFWHM(sfd)
		    case 3:
			SFD_W(sfd) = SFD_GFWHM(sfd)
		    case 4:
			SFD_W(sfd) = SFD_MFWHM(sfd)
		    }
		    call stf_fwhms (sf, sfd)
		}
		call stf_fitfocus (sf) 
		redraw = YES
	    } else {
		call printf ("beta %g\n")
		    call pargr (SF_BETA(sf))
		redraw = NO
	    }
	default:
	    call printf ("Unrecognized or ambiguous command\007")
	    redraw = NO
	}

	call sfree (sp)
end


# STF_G1 -- Plot of size/ellip vs. focus/mag/radius.

procedure stf_g1 (sf, current, xkey, ykey, title, xlabel, ylabel)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
int	xkey			#I X axis key
int	ykey			#I Y axis key
char	title[ARB]		#I Title
char	xlabel[ARB]		#I X label
char	ylabel[ARB]		#I Y label

int	i, j
bool	hl
real	x, x1, x2, dx, y, y1, y2, dy
pointer	gp, sff, sfd

begin
	# Determine data range
	x1 = MAX_REAL
	x2 = -MAX_REAL
	switch (ykey) {
	case 'r':
	    y1 = SF_W(sf)
	    y2 = 1.5 * SF_W(sf)
	case 'e':
	    y1 = 0
	    y2 = 1
	}
	do j = 1, SF_NFOCUS(sf) {
	    sff = SF_SFF(sf,j)
	    if (xkey != 'f' && sff != SFD_SFF(current))
		next
	    do i = 1, SFF_NSFD(sff) {
		sfd = SFF_SFD(sff,i)
		if (SFD_STATUS(sfd) == 0) {
		    switch (xkey) {
		    case 'f':
			x = SFD_F(sfd)
		    case 'm':
			x = -2.5 * log10 (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		    case 't':
			x = sqrt ((SFD_X(sfd) - SF_XF(sf)) ** 2 +
			    (SFD_Y(sfd) - SF_YF(sf)) ** 2)
		    }
		    switch (ykey) {
		    case 'r':
			y = SFD_W(sfd)
		    case 'e':
			y = SFD_E(sfd)
		    }
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
	gp = SF_GP (sf)
	call gswind (gp, x1, x2, y1, y2)
	call glabax (gp, title, xlabel, ylabel)

	do j = 1, SF_NFOCUS(sf) {
	    sff = SF_SFF(sf,j)
	    if (xkey != 'f' && sff != SFD_SFF(current))
		next
	    do i = 1, SFF_NSFD(sff) {
		sfd = SFF_SFD(sff,i)
		if (SFD_STATUS(sfd) == 0) {
		    hl = false
		    switch (xkey) {
		    case 'f':
			x = SFD_F(sfd)
			#hl = (SFD_SFS(sfd) == SFD_SFS(current))
		    case 'm':
			x = -2.5 * log10 (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
			#hl = (SFD_SFF(sfd) != SFD_SFF(current))
		    case 't':
			x = sqrt ((SFD_X(sfd) - SF_XF(sf)) ** 2 +
			    (SFD_Y(sfd) - SF_YF(sf)) ** 2)
			#hl = (SFD_SFF(sfd) != SFD_SFF(current))
		    }
		    switch (ykey) {
		    case 'r':
			y = SFD_W(sfd)
		    case 'e':
			y = SFD_E(sfd)
		    }
		    if (hl) {
			call gseti (gp, G_PLCOLOR, HLCOLOR)
			if (sfd == current)
			    call gmark (gp, x, y, GM_BOX, 3., 3.)
			call gmark (gp, x, y, GM_PLUS, 3., 3.)
			call gseti (gp, G_PLCOLOR, 1)
		    } else
			call gmark (gp, x, y, GM_MARK, 2., 2.)
		}
	    }
	}

	call gseti (gp, G_PLTYPE, 2)
	if (xkey == 'f')
	    call gline (gp, SF_F(sf), y1, SF_F(sf), y2)
	if (ykey == 'r')
	    call gline (gp, x1, SF_W(sf), x2, SF_W(sf))
	call gseti (gp, G_PLTYPE, 1)
end


# STF_G2 -- Enclosed flux profiles for a given star.

procedure stf_g2 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, np, np1, nx, ny, ix, iy
real	vx, dvx, vy, dvy, x1, x2, y1, y2, z, z1, r, r1, r2, dr, fa[10]
pointer	sp, str, gp, sfs, sfd, asi
real	stf_i2r(), stf_r2i(), asieval()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sfs = SFD_SFS(current)
	np = SFD_NP(current)

	# Set grid layout
	i = SFS_N(sfs)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	x1 = -0.05
	x2 = 1.05
	y1 = -0.15
	y2 = 1.05
	call gswind (gp, x1, x2, y1, y2)

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw profiles.
	j = 0
	do i = 1, SFS_NSFD(sfs) {
	    sfd = SFS_SFD(sfs, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    np1 = SFD_NP(sfd)
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    asi = SFD_ASI1(sfd)
	    r2 = stf_i2r (real(np))
	    call gamove (gp, 0., 0.)
	    for (z = 1.; z <= np1; z = z + 0.1)
		call gadraw (gp, stf_i2r(z)/r2, asieval(asi,z))
	    if (SF_OVRPLT(sf) == YES && sfd != SF_BEST(sf)) {
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		np1 = SFD_NP(SF_BEST(sf))
		asi = SFD_ASI1(SF_BEST(sf))
		r1 = stf_i2r (1.)
		r2 = stf_i2r (real(np))
		dr = 0.05 * (r2 - r1)
		for (r = r1; r <= r2; r = r + dr) {
		    z = stf_r2i (r)
		    z1 = stf_r2i (r+0.7*dr)
		    if (z > 1. && z1 <= np1)
			call gline (gp, r/r2, asieval(asi,z),
			    (r+0.7*dr)/r2, asieval(asi,z1))
		}
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, -0.1, Memc[str], "h=r;v=b")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%.4g")
		    call pargr (SFD_F(sfd))
		call gtext (gp, 0.05, -0.1, Memc[str], "h=l;v=b")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G3 -- Enclosed flux profiles for a given focus.

procedure stf_g3 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, np, np1, nx, ny, ix, iy
real	vx, dvx, vy, dvy, x1, x2, y1, y2, z, z1, r, r1, r2, dr, fa[10]
pointer	sp, str, gp, sff, sfd, asi
real	stf_i2r(), stf_r2i(), asieval()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sff = SFD_SFF(current)
	np = SFD_NP(current)

	# Set grid layout
	i = SFF_N(sff)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	x1 = -0.05
	x2 = 1.05
	y1 = -0.2
	y2 = 1.05
	call gswind (gp, x1, x2, y1, y2)

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw profiles.
	j = 0
	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    np1 = SFD_NP(sfd)
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    asi = SFD_ASI1(sfd)
	    r2 = stf_i2r (real(np))
	    call gamove (gp, 0., 0.)
	    for (z = 1.; z <= np1; z = z + 0.1)
		call gadraw (gp, stf_i2r(z)/r2, asieval(asi,z))
	    if (SF_OVRPLT(sf) == YES && sfd != SF_BEST(sf)) {
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		np1 = SFD_NP(SF_BEST(sf))
		asi = SFD_ASI1(SF_BEST(sf))
		r1 = stf_i2r (1.)
		r2 = stf_i2r (real(np))
		dr = 0.05 * (r2 - r1)
		for (r = r1; r <= r2; r = r + dr) {
		    z = stf_r2i (r)
		    z1 = stf_r2i (r+0.7*dr)
		    if (z > 1. && z1 <= np1)
			call gline (gp, r/r2, asieval(asi,z),
			    (r+0.7*dr)/r2, asieval(asi,z1))
		}
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, -.1, Memc[str], "h=r;v=b")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%d %d")
		    call pargr (SFD_X(sfd))
		    call pargr (SFD_Y(sfd))
		call gtext (gp, 0.05, -.1, Memc[str], "h=l;v=b")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G4 -- Radial profiles (derivative of enclosed flux) for a given star.

procedure stf_g4 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, np, np1, nx, ny, ix, iy
real	vx, dvx, vy, dvy, x1, x2, y1, y2, z, z1, r, r1, r2, dr, rmax, fa[10]
pointer	sp, str, gp, sfs, sfd, asi
real	stf_i2r(), stf_r2i(), asieval()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sfs = SFD_SFS(current)
	np = SFD_NP(current)

	# Set grid layout
	i = SFS_N(sfs)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	x1 = -0.05
	x2 = 1.05
	z = SF_YP2(sf) - SF_YP1(sf)
	y1 = SF_YP1(sf) - 0.05 * z
	y2 = SF_YP2(sf) + 0.15 * z

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw profiles.
	j = 0
	do i = 1, SFS_NSFD(sfs) {
	    sfd = SFS_SFD(sfs, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    np1 = SFD_NP(sfd)
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gswind (gp, x1, x2, y1, y2)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    asi = SFD_ASI2(sfd)
	    rmax = stf_i2r (real(np))
	    z = SF_XP1(sf)
	    call gamove (gp, stf_i2r(z)/rmax, asieval(asi,z))
	    for (; z <= SF_XP2(sf); z = z + 0.1)
		call gadraw (gp, stf_i2r(z)/rmax, asieval(asi,z))
	    if (SF_OVRPLT(sf) == YES && sfd != SF_BEST(sf)) {
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		np1 = SFD_NP(SF_BEST(sf))
		asi = SFD_ASI2(SF_BEST(sf))
		rmax = stf_i2r (real(np))
		r1 = stf_i2r (SF_XP1(sf))
		r2 = stf_i2r (SF_XP2(sf))
		dr = 0.05 * (rmax - stf_i2r(1.))
		for (r = r1; r <= r2; r = r + dr) {
		    z = stf_r2i (r)
		    z1 = stf_r2i (r+0.7*dr)
		    if (z > 1. && z1 <= np1)
			call gline (gp, r/rmax, asieval(asi,z),
			    (r+0.7*dr)/rmax, asieval(asi,z1))
		}
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gswind (gp, 0., 1., 0., 1.)
	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, 0.98, Memc[str], "h=r;v=t")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%.4g")
		    call pargr (SFD_F(sfd))
		call gtext (gp, 0.05, 0.98, Memc[str], "h=l;v=t")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G5 -- Radial profiles (derivative of enclosed flux) for a given focus.

procedure stf_g5 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, np, np1, nx, ny, ix, iy
real	vx, dvx, vy, dvy, x1, x2, y1, y2, z, z1, r, r1, r2, dr, rmax, fa[10]
pointer	sp, str, gp, sff, sfd, asi
real	stf_i2r(), stf_r2i(), asieval()

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sff = SFD_SFF(current)
	np = SFD_NP(current)

	# Set grid layout
	i = SFF_N(sff)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	x1 = -0.05
	x2 = 1.05
	z = SF_YP2(sf) - SF_YP1(sf)
	y1 = SF_YP1(sf) - 0.05 * z
	y2 = SF_YP2(sf) + 0.15 * z

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw profiles.
	j = 0
	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    np1 = SFD_NP(sfd)
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gswind (gp, x1, x2, y1, y2)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    asi = SFD_ASI2(sfd)
	    rmax = stf_i2r (real(np))
	    z = SF_XP1(sf)
	    call gamove (gp, stf_i2r(z)/rmax, asieval(asi,z))
	    for (; z <= SF_XP2(sf); z = z + 0.1)
		call gadraw (gp, stf_i2r(z)/rmax, asieval(asi,z))
	    if (SF_OVRPLT(sf) == YES && sfd != SF_BEST(sf)) {
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		np1 = SFD_NP(SF_BEST(sf))
		asi = SFD_ASI2(SF_BEST(sf))
		rmax = stf_i2r (real(np))
		r1 = stf_i2r (SF_XP1(sf))
		r2 = stf_i2r (SF_XP2(sf))
		dr = 0.05 * (rmax - stf_i2r (1.))
		for (r = r1; r <= r2; r = r + dr) {
		    z = stf_r2i (r)
		    z1 = stf_r2i (r+0.7*dr)
		    if (z > 1. && z1 <= np1)
			call gline (gp, r/rmax, asieval(asi,z),
			    (r+0.7*dr)/rmax, asieval(asi,z1))
		}
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gswind (gp, 0., 1., 0., 1.)
	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, 0.98, Memc[str], "h=r;v=t")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%d %d")
		    call pargr (SFD_X(sfd))
		    call pargr (SFD_Y(sfd))
		call gtext (gp, 0.05, 0.98, Memc[str], "h=l;v=t")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G6 -- Enclosed flux profile of a star.

procedure stf_g6 (sf, current, title, xlabel, ylabel)

pointer	sf			#I Starfocus pointer
pointer	current			#I Star pointer
char	title[ARB]		#I Title
char	xlabel[ARB]		#I X label
char	ylabel[ARB]		#I Y label

int	np, np1
real	scale, level, radius, flux, profile
pointer	gp, asi

real	x1, x2, y1, y2, z, z1, r, r1, r2, dr
real	stf_i2r(), stf_r2i(), asieval()

begin
	gp = SF_GP(sf)
	level = SF_LEVEL(sf)
	scale = SF_SCALE(sf)
	np = SFD_NP(current)
	asi = SFD_ASI1(current)

	x1 = -0.5 * scale
	x2 = (stf_i2r (real(np)) + 0.5) * scale
	y1 = -0.05
	y2 = 1.05
	call gswind (gp, x1, x2, y1, y2)

        call gseti (gp, G_DRAWTICKS, YES)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call glabax (gp, title, xlabel, ylabel)

	# Draw profiles.
	if (SFD_STATUS(current) == 0) {
	    call gseti (gp, G_PLCOLOR, 1)
	    for (z = 1.; z <= np; z = z + 1)
		call gmark (gp, stf_i2r(z)*scale, asieval(asi,z),
		    GM_PLUS, 2., 2.)
	    call gamove (gp, 0., 0.)
	    for (z = 1.; z <= np; z = z + 0.1)
		call gadraw (gp, stf_i2r(z)*scale, asieval(asi,z))
	    switch (SF_WCODE(sf)) {
	    case 1:
		radius = SFD_W(current)
		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, x1, level, radius, level)
		call gline (gp, radius, level, radius, y1)
		call gseti (gp, G_PLTYPE, 1)
	    default:
		radius = SFD_W(current) / 2.
		call gseti (gp, G_PLTYPE, 2)
		call gline (gp, radius, y1, radius, y2)
		call gseti (gp, G_PLTYPE, 1)
	    }

	    call gseti (gp, G_PLCOLOR, HLCOLOR)
	    call stf_model (sf, current, 0., profile, flux)
	    call gamove (gp, 0., flux)
	    for (z = 1.; z <= np; z = z + 0.1) {
		r = stf_i2r(z) * scale
		call stf_model (sf, current, r, profile, flux)
		call gadraw (gp, r, flux)
	    }
	    call gseti (gp, G_PLCOLOR, 1)
	    if (SF_OVRPLT(sf) == YES && current != SF_BEST(sf)) {
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		np1 = SFD_NP(SF_BEST(sf))
		asi = SFD_ASI1(SF_BEST(sf))
		r1 = stf_i2r(1.)
		r2 = stf_i2r (real(np))
		dr = 0.05 * (r2 - r1)
		for (r = r1; r <= r2; r = r + dr) {
		    z = stf_r2i (r)
		    z1 = stf_r2i (r+0.7*dr)
		    if (z > 1. && z1 <= np1)
			call gline (gp, r*scale, asieval(asi,z),
			    (r+0.7*dr)*scale, asieval(asi,z1))
		}
		call gseti (gp, G_PLCOLOR, 1)
	    }
	}
end


# STF_G7 -- Radial profile (derivative of enclosed flux) for a star.

procedure stf_g7 (sf, current, title, xlabel, ylabel)

pointer	sf			#I Starfocus pointer
pointer	current			#I Star pointer
char	title[ARB]		#I Title
char	xlabel[ARB]		#I X label
char	ylabel[ARB]		#I Y label

int	np, np1
real	scale, level, radius, profile, flux
pointer	gp, asi

real	x1, x2, y1, y2, z, z1, r, r1, r2, dr
real	stf_i2r(), stf_r2i(), asieval()

begin
	gp = SF_GP(sf)
	level = SF_LEVEL(sf)
	scale = SF_SCALE(sf)
	np = SFD_NP(current)
	asi = SFD_ASI2(current)

	x1 = -0.5 * scale
	x2 = (stf_i2r (real(np)) + 0.5) * scale
	z = SFD_YP2(current) - SFD_YP1(current)
	y1 = SFD_YP1(current) - 0.05 * z
	y2 = SFD_YP2(current) + 0.05 * z
	call gswind (gp, x1, x2, y1, y2)

        call gseti (gp, G_XDRAWTICKS, YES)
        call gseti (gp, G_YDRAWTICKS, NO)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call glabax (gp, title, xlabel, ylabel)

	# Draw profile
	call gseti (gp, G_PLCOLOR, 1)
	for (z = SF_XP1(sf); z <= SF_XP2(sf); z = z + 1)
	    call gmark (gp, stf_i2r(z)*scale, asieval(asi,z),
		GM_PLUS, 2., 2.)
	z = SF_XP1(sf)
	call gamove (gp, stf_i2r(z)*scale, asieval(asi,z))
	for (; z <= SF_XP2(sf); z = z + 0.1)
	    call gadraw (gp, stf_i2r (z)*scale, asieval(asi,z))

	switch (SF_WCODE(sf)) {
	case 1:
	    radius = SFD_W(current)
	default:
	    radius = SFD_W(current) / 2.
	}
	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, radius, y1, radius, y2)
	call gseti (gp, G_PLTYPE, 1)

	call gseti (gp, G_PLCOLOR, HLCOLOR)
	z = SF_XP1(sf)
	r = stf_i2r(z) * scale
	call stf_model (sf, current, r, profile, flux)
	call gamove (gp, r, profile)
	for (; z <= np; z = z + 0.1) {
	    r = stf_i2r(z) * scale
	    call stf_model (sf, current, r, profile, flux)
	    call gadraw (gp, r, profile)
	}
	call gseti (gp, G_PLCOLOR, 1)
	if (SF_OVRPLT(sf) == YES && current != SF_BEST(sf)) {
	    call gseti (gp, G_PLCOLOR, HLCOLOR)
	    np1 = SFD_NP(SF_BEST(sf))
	    asi = SFD_ASI2(SF_BEST(sf))
	    r1 = stf_i2r (SF_XP1(sf))
	    r2 = stf_i2r (SF_XP2(sf))
	    dr = 0.05 * (r2 - r1)
	    for (r = r1; r <= r2; r = r + dr) {
		z = stf_r2i (r)
		z1 = stf_r2i (r+0.7*dr)
		if (z > 1. && z1 <= np1)
		    call gline (gp, r*scale, asieval(asi,z),
			(r+0.7*dr)*scale, asieval(asi,z1))
	    }
	    call gseti (gp, G_PLCOLOR, 1)
	}
end


# STF_G8 -- FWHM vs level.

procedure stf_g8 (sf, current, title, xlabel, ylabel)

pointer	sf			#I Starfocus pointer
pointer	current			#I Star pointer
char	title[ARB]		#I Title
char	xlabel[ARB]		#I X label
char	ylabel[ARB]		#I Y label

real	y1, y2, level, fwhm
pointer	gp

begin
	level = SF_LEVEL(sf)
	if (SF_WCODE(sf) == 1)
	    fwhm = SFD_MFWHM(current)
	else
	    fwhm = SFD_W(current)

	call alimr (SFD_FWHM(current,2), 17, y1, y2)
	y2 = y2 - y1
	y1 = y1 - 0.05 * y2
	y2 = y1 + 1.10 * y2
	y1 = min (y1, 0.9 * fwhm)
	y2 = max (y2, 1.1 * fwhm)

	gp = SF_GP(sf)
        call gseti (gp, G_DRAWTICKS, YES)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call gswind (gp, 0., 1., y1, y2)
	call glabax (gp, title, xlabel, ylabel)
	call gvline (gp, SFD_FWHM(current,2), 17, 0.1, 0.9)
	call gvmark (gp, SFD_FWHM(current,2), 17, 0.1, 0.9, GM_PLUS, 2., 2.)

	switch (SF_WCODE(sf)) {
	case 1:
	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, 0., fwhm, level, fwhm)
	    call gline (gp, level, y1, level, fwhm)
	    call gseti (gp, G_PLTYPE, 1)
	default:
	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, 0., fwhm, 1., fwhm)
	    call gseti (gp, G_PLTYPE, 1)
	}
end


# STF_G9 -- FWHM vs level for a given star.

procedure stf_g9 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, nx, ny, ix, iy
real	level, fwhm, vx, dvx, vy, dvy, x1, x2, y1, y2, fa[10]
pointer	sp, str, gp, sfs, sfd

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sfs = SFD_SFS(current)
	level = SF_LEVEL(sf)
	if (SF_WCODE(sf) == 1)
	    fwhm = SFD_MFWHM(current)
	else
	    fwhm = SFD_W(current)

	# Set grid layout
	i = SFS_N(sfs)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	y1 = 0.9 * fwhm
	y2 = 1.1 * fwhm
	do i = 1, SFS_NSFD(sfs) {
	    sfd = SFS_SFD(sfs,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    call alimr (SFD_FWHM(sfd,2), 17, x1, x2)
	    x2 = x2 - x1
	    x1 = x1 - 0.05 * x2
	    x2 = x1 + 1.10 * x2
	    y1 = min (x1, y1)
	    y2 = max (x2, y2)
	}
	x2 = y2 - y1
	y1 = min (y1, fwhm - 0.2 * x2)
	y2 = max (y2, fwhm + 0.2 * x2)
	x1 = 0.
	x2 = 1.
	call gswind (gp, x1, x2, y1, y2)

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw profiles.
	j = 0
	do i = 1, SFS_NSFD(sfs) {
	    sfd = SFS_SFD(sfs, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    call gvline (gp, SFD_FWHM(sfd,2), 17, 0.1, 0.9)
	    #call gseti (gp, G_PLTYPE, 2)
	    #call gline (gp, x1, fwhm, x2, fwhm)
	    #call gseti (gp, G_PLTYPE, 1)

	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, 0.95*y2+0.05*y1, Memc[str], "h=r;v=t")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%.4g")
		    call pargr (SFD_F(sfd))
		call gtext (gp, 0.05, 0.95*y2+0.05*y1, Memc[str], "h=l;v=t")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G10 -- FWHM vs level for a given focus.

procedure stf_g10 (sf, current, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
char	title[ARB]		#I Title

int	i, j, nx, ny, ix, iy
real	level, fwhm, vx, dvx, vy, dvy, x1, x2, y1, y2, fa[10]
pointer	sp, str, gp, sff, sfd

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	gp = SF_GP(sf)
	sff = SFD_SFF(current)
	level = SF_LEVEL(sf)
	if (SF_WCODE(sf) == 1)
	    fwhm = SFD_MFWHM(current)
	else
	    fwhm = SFD_W(current)

	# Set grid layout
	i = SFF_N(sff)
	if (i < 4) {
	    nx = i
	    ny = 1
	} else {
	    nx = nint (sqrt (real (i)))
	    if (mod (i-1, nx+1) >= mod (i-1, nx))
		nx = nx + 1
	    ny = (i - 1) / nx + 1
	}

        # Set subview port parameters
        call ggview (gp, vx, dvx, vy, dvy)
        dvx = (dvx - vx) / nx
        dvy = (dvy - vy) / ny

	# Set data window parameters
	y1 = 0.9 * fwhm
	y2 = 1.1 * fwhm
	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    call alimr (SFD_FWHM(sfd,2), 17, x1, x2)
	    x2 = x2 - x1
	    x1 = x1 - 0.05 * x2
	    x2 = x1 + 1.10 * x2
	    y1 = min (x1, y1)
	    y2 = max (x2, y2)
	}
	x2 = y2 - y1
	y1 = min (y1, fwhm - 0.2 * x2)
	y2 = max (y2, fwhm + 0.2 * x2)
	x1 = 0.
	x2 = 1.
	call gswind (gp, x1, x2, y1, y2)

	# Set fill area
        fa[1] = x1; fa[6] = y1
        fa[2] = x2; fa[7] = y1
        fa[3] = x2; fa[8] = y2
        fa[4] = x1; fa[9] = y2
        fa[5] = x1; fa[10] = y1

	# Draw plots.
	j = 0
	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff, i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    ix = 1 + mod (j, nx)
	    iy = 1 + j / nx
	    j = j + 1
	    call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    call gfill (gp, fa, fa[6], 4, GF_SOLID)
	    call gseti (gp, G_DRAWTICKS, NO)
	    call glabax (gp, "", "", "")
	    if (sfd == current) {
		call gsview (gp, vx+dvx*(ix-1)+.005, vx+dvx*ix-.005,
		    vy+dvy*(ny-iy)+.005, vy+(ny-iy+1)*dvy-.005)
		call gsetr (gp, G_PLWIDTH, HLWIDTH)
		call gseti (gp, G_PLCOLOR, HLCOLOR)
		call gpline (gp, fa, fa[6], 5)
		call gsetr (gp, G_PLWIDTH, 1.)
		call gseti (gp, G_PLCOLOR, 1)
		call gsview (gp, vx+dvx*(ix-1), vx+dvx*ix,
		    vy+dvy*(ny-iy), vy+(ny-iy+1)*dvy)
	    }

	    call gvline (gp, SFD_FWHM(sfd,2), 17, 0.1, 0.9)
	    #call gseti (gp, G_PLTYPE, 2)
	    #call gline (gp, x1, fwhm, x2, fwhm)
	    #call gseti (gp, G_PLTYPE, 1)

	    call sprintf (Memc[str], SZ_LINE, "%.3g")
		call pargr (SFD_W(sfd))
	    call gtext (gp, 0.95, 0.95*y2+0.05*y1, Memc[str], "h=r;v=t")
	    if (nx < NMAX && ny < NMAX) {
		call sprintf (Memc[str], SZ_LINE, "%d %d")
		    call pargr (SFD_X(sfd))
		    call pargr (SFD_Y(sfd))
		call gtext (gp, 0.05, 0.95*y2+0.05*y1, Memc[str], "h=l;v=t")
	    }
	}

	call gsview (gp, vx, vx+nx*dvx, vy, vy+ny*dvy)
	call gswind (gp, 0.5, 0.5+nx, 0.5+ny, 0.5)
	call gamove (gp, 1., 1.)

        # Draw label
        call gseti (gp, G_DRAWAXES, 0)
        call glabax (gp, title, "", "")
        call gseti (gp, G_DRAWAXES, 3)

	call sfree (sp)
end


# STF_G11 -- Spatial plot at one focus.

procedure stf_g11 (sf, current, key, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
int	key			#I Plot magnitude symbol?
char	title[ARB]		#I Title

int	i
real	x, y, z, x1, x2, y1, y2, rbest, rmin, rmax, emin, emax
real	vx[3,2], vy[3,2], dvx, dvy, fa[8]
pointer	gp, sfd, sff

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	gp = SF_GP(sf)
	sff = SFD_SFF(current)

	# Range of X, Y, R, E.
	x1 = 1.
	y1 = 1.
	x2 = SF_NCOLS(sf)
	y2 = SF_NLINES(sf)

	rbest = SFD_W(SF_BEST(sf))
	rmin = SF_W(sf)
	rmax = 1.5 * SF_W(sf)
	emin = 0
	emax = 1
	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    rmin = min (rmin, SFD_W(sfd))
	    rmax = max (rmax, SFD_W(sfd))
	    emin = min (emin, SFD_E(sfd))
	    emax = max (emax, SFD_E(sfd))
	}
	z = rmax - rmin
	rmin = rmin - 0.1 * z
	rmax = rmax + 0.1 * z

	# Set view ports
	call ggview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	dvx = vx[3,2] - vx[1,1]
	dvy = vy[3,2] - vy[1,1]
	vx[1,1] = vx[1,1] + 0.00 * dvx
	vx[1,2] = vx[1,1] + 0.20 * dvx
	vx[2,1] = vx[1,1] + 0.25 * dvx
	vx[2,2] = vx[1,1] + 0.75 * dvx
	vx[3,1] = vx[1,1] + 0.80 * dvx
	vx[3,2] = vx[1,1] + 1.00 * dvx
	vy[1,1] = vy[1,1] + 0.00 * dvy
	vy[1,2] = vy[1,1] + 0.20 * dvy
	vy[2,1] = vy[1,1] + 0.25 * dvy
	vy[2,2] = vy[1,1] + 0.75 * dvy
	vy[3,1] = vy[1,1] + 0.80 * dvy
	vy[3,2] = vy[1,1] + 1.00 * dvy

	# (X,R)
	call gseti (gp, G_WCS, 2)
	call gseti (gp, G_DRAWAXES, 3)
	call gseti (gp, G_XLABELTICKS, YES)
	call gseti (gp, G_YLABELTICKS, YES)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 4)
	call gseti (gp, G_YNMINOR, 0)
	call gsview (gp, vx[2,1], vx[2,2], vy[1,1], vy[1,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)

	call gswind (gp, x1, x2, rmin, rmax)
	call glabax (gp, "", "Column", "")

	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    x = SFD_X(sfd)
	    y = SFD_W(sfd)
	    if (key == 1) {
		z = sqrt (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		z = max (0.005, 0.03 * z)
		call gmark (gp, x, y, GM_MAG, z, z)
	    }
	    if (SFD_W(sfd) < SF_W(sf))
		call gseti (gp, G_PLCOLOR, 2)
	    else
		call gseti (gp, G_PLCOLOR, 3)
	    z = min (2., SFD_W(sfd) / rbest)
	    z = 0.010 * (1 + (z - 1) * 5)
	    call gmark (gp, x, y, GM_CIRCLE, z, z)
	    call gseti (gp, G_PLCOLOR, 1)
	}

	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, x1, SF_W(sf), x2, SF_W(sf))
	call gseti (gp, G_PLTYPE, 1)

	# (R,Y)
	call gseti (gp, G_WCS, 3)
	call gseti (gp, G_XLABELTICKS, YES)
	call gseti (gp, G_YLABELTICKS, YES)
	call gseti (gp, G_XNMAJOR, 4)
	call gseti (gp, G_XNMINOR, 0)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call gsview (gp, vx[1,1], vx[1,2], vy[2,1], vy[2,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)

	call gswind (gp, rmin, rmax, y1, y2)
	call glabax (gp, "", SF_WTYPE(sf), "Line")

	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    x = SFD_W(sfd)
	    y = SFD_Y(sfd)
	    if (key == 1) {
		z = sqrt (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		z = max (0.005, 0.03 * z)
		call gmark (gp, x, y, GM_MAG, z, z)
	    }
	    if (SFD_W(sfd) < SF_W(sf))
		call gseti (gp, G_PLCOLOR, 2)
	    else
		call gseti (gp, G_PLCOLOR, 3)
	    z = min (2., SFD_W(sfd) / rbest)
	    z = 0.010 * (1 + (z - 1) * 5)
	    call gmark (gp, x, y, GM_CIRCLE, z, z)
	    call gseti (gp, G_PLCOLOR, 1)
	}

	call gseti (gp, G_PLTYPE, 2)
	call gline (gp, SF_W(sf), y1, SF_W(sf), y2)
	call gseti (gp, G_PLTYPE, 1)

	# (E,R)
	call gseti (gp, G_WCS, 4)
	call gseti (gp, G_DRAWAXES, 3)
	call gseti (gp, G_XLABELTICKS, NO)
	call gseti (gp, G_YLABELTICKS, YES)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 4)
	call gseti (gp, G_YNMINOR, 0)
	call gsview (gp, vx[2,1], vx[2,2], vy[3,1], vy[3,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)

	call gswind (gp, x1, x2, emin, emax)
	call glabax (gp, "", "", "Ellip")

	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    x = SFD_X(sfd)
	    y = SFD_E(sfd)
	    if (key == 1) {
		z = sqrt (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		z = max (0.005, 0.03 * z)
		call gmark (gp, x, y, GM_MAG, z, z)
	    }
	    if (SFD_W(sfd) < SF_W(sf))
		call gseti (gp, G_PLCOLOR, 2)
	    else
		call gseti (gp, G_PLCOLOR, 3)
	    z = min (2., SFD_W(sfd) / rbest)
	    z = 0.010 * (1 + (z - 1) * 5)
	    call gmark (gp, x, y, GM_CIRCLE, z, z)
	    call gseti (gp, G_PLCOLOR, 1)
	}

	# (E,Y)
	call gseti (gp, G_WCS, 5)
	call gseti (gp, G_XLABELTICKS, YES)
	call gseti (gp, G_YLABELTICKS, NO)
	call gseti (gp, G_XNMAJOR, 4)
	call gseti (gp, G_XNMINOR, 0)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call gsview (gp, vx[3,1], vx[3,2], vy[2,1], vy[2,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)

	call gswind (gp, emin, emax, y1, y2)
	call glabax (gp, "", "Ellip", "")

	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    x = SFD_E(sfd)
	    y = SFD_Y(sfd)
	    if (key == 1) {
		z = sqrt (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		z = max (0.005, 0.03 * z)
		call gmark (gp, x, y, GM_MAG, z, z)
	    }
	    if (SFD_W(sfd) < SF_W(sf))
		call gseti (gp, G_PLCOLOR, 2)
	    else
		call gseti (gp, G_PLCOLOR, 3)
	    z = min (2., SFD_W(sfd) / rbest)
	    z = 0.010 * (1 + (z - 1) * 5)
	    call gmark (gp, x, y, GM_CIRCLE, z, z)
	    call gseti (gp, G_PLCOLOR, 1)
	}

	# Label window.
	call gseti (gp, G_WCS, 1)
	call gseti (gp, G_DRAWAXES, 0)
	call gsview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	call glabax (gp, title, "", "")

	# (X,Y)
	call gseti (gp, G_DRAWAXES, 3)
	call gseti (gp, G_LABELTICKS, NO)
	call gseti (gp, G_XNMAJOR, 6)
	call gseti (gp, G_XNMINOR, 4)
	call gseti (gp, G_YNMAJOR, 6)
	call gseti (gp, G_YNMINOR, 4)
	call gsview (gp, vx[2,1], vx[2,2], vy[2,1], vy[2,2])
	call gswind (gp, 0., 1., 0., 1.)
	call gfill (gp, fa, fa[5], 4, GF_SOLID)

	call gswind (gp, x1, x2, y1, y2)
	call glabax (gp, "", "", "")

	do i = 1, SFF_NSFD(sff) {
	    sfd = SFF_SFD(sff,i)
	    if (SFD_STATUS(sfd) != 0)
		next
	    x = SFD_X(sfd)
	    y = SFD_Y(sfd)
	    if (key == 1) {
		z = sqrt (SFS_M(SFD_SFS(sfd)) / SF_M(sf))
		z = max (0.005, 0.03 * z)
		call gmark (gp, x, y, GM_MAG, z, z)
	    }
	    if (SFD_W(sfd) < SF_W(sf))
		call gseti (gp, G_PLCOLOR, 2)
	    else
		call gseti (gp, G_PLCOLOR, 3)
	    z = min (2., SFD_W(sfd) / rbest)
	    z = 0.010 * (1 + (z - 1) * 5)
	    call gmark (gp, x, y, GM_CIRCLE, z, z)
	    call gseti (gp, G_PLCOLOR, 1)
	}
end


# STF_G9 -- Spatial plots at best focus.

procedure stf_g12 (sf, current, key, title)

pointer	sf			#I Starfocus pointer
pointer	current			#I Current sfd pointer
int	key			#I Plot magnitude symbol?
char	title[ARB]		#I Title

int	i
real	x, y, z, x1, x2, y1, y2, fmin, fmax, rbest, rmin, rmax
real	vx[3,2], vy[3,2], dvx, dvy, fa[8]
pointer	gp, sfs, sfd

data	fa/0.,1.,1.,0.,0.,0.,1.,1./

begin
	gp = SF_GP(sf)

	# Range of X, Y, R, F.
	x1 = 1.
	y1 = 1.
	x2 = SF_NCOLS(sf)
	y2 = SF_NLINES(sf)

	rbest = SFD_W(SF_BEST(sf))
	fmin = MAX_REAL
	fmax = -MAX_REAL
	rmin = SF_W(sf)
	rmax = 1.5 * SF_W(sf)
	do i = 1, SF_NSTARS(sf) {
	    sfs = SF_SFS(sf,i)
	    if (SFS_N(sfs) == 0)
		next
	    fmin = min (fmin, SFS_F(sfs))
	    fmax = max (fmax, SFS_F(sfs))
	    rmin = min (rmin, SFS_W(sfs))
	    rmax = max (rmax, SFS_W(sfs))
	}
	z = fmax - fmin
	fmin = fmin - 0.1 * z
	fmax = fmax + 0.1 * z
	z = rmax - rmin
	rmin = rmin - 0.1 * z
	rmax = rmax + 0.1 * z

	# Set view ports
	call ggview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	dvx = vx[3,2] - vx[1,1]
	dvy = vy[3,2] - vy[1,1]
	vx[1,1] = vx[1,1] + 0.00 * dvx
	vx[1,2] = vx[1,1] + 0.20 * dvx
	vx[2,1] = vx[1,1] + 0.25 * dvx
	if (SF_NF(sf) > 1) {
	    vx[2,2] = vx[1,1] + 0.75 * dvx
	    vx[3,1] = vx[1,1] + 0.80 * dvx
	    vx[3,2] = vx[1,1] + 1.00 * dvx
	} else {
	    vx[2,2] = vx[1,1] + 1.00 * dvx
	    vx[3,1] = vx[1,1] + 1.00 * dvx
	    vx[3,2] = vx[1,1] + 1.00 * dvx
	}
	vy[1,1] = vy[1,1] + 0.00 * dvy
	vy[1,2] = vy[1,1] + 0.20 * dvy
	vy[2,1] = vy[1,1] + 0.25 * dvy
	if (SF_NF(sf) > 1) {
	    vy[2,2] = vy[1,1] + 0.75 * dvy
	    vy[3,1] = vy[1,1] + 0.80 * dvy
	    vy[3,2] = vy[1,1] + 1.00 * dvy
	} else {
	    vy[2,2] = vy[1,1] + 1.00 * dvy
	    vy[3,1] = vy[1,1] + 1.00 * dvy
	    vy[3,2] = vy[1,1] + 1.00 * dvy
	}

	dvx = vx[2,1] - vx[2,2]
	dvy = vy[1,1] - vy[1,2]
	if (abs (dvx) > 0.01 && abs (dvy) > 0.01) {
	    # (X,R)
	    call gseti (gp, G_WCS, 2)
	    call gseti (gp, G_DRAWAXES, 3)
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gseti (gp, G_YLABELTICKS, YES)
	    call gseti (gp, G_XNMAJOR, 6)
	    call gseti (gp, G_XNMINOR, 4)
	    call gseti (gp, G_YNMAJOR, 4)
	    call gseti (gp, G_YNMINOR, 0)
	    call gsview (gp, vx[2,1], vx[2,2], vy[1,1], vy[1,2])
	    call gswind (gp, 0., 1., 0., 1.)
	    call gfill (gp, fa, fa[5], 4, GF_SOLID)

	    call gswind (gp, x1, x2, rmin, rmax)
	    call glabax (gp, "", "Column", "")

	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_N(sfs) == 0)
		    next
		x = SFD_X(SFS_SFD(sfs,1))
		y = SFS_W(sfs)
		if (key == 1) {
		    z = sqrt (SFS_M(sfs) / SF_M(sf))
		    z = max (0.005, 0.03 * z)
		    call gmark (gp, x, y, GM_MAG, z, z)
		}
		if (SFS_F(sfs) < SF_F(sf))
		    call gseti (gp, G_PLCOLOR, 2)
		else
		    call gseti (gp, G_PLCOLOR, 3)
		z = min (2., SFS_W(sfs) / rbest)
		z = 0.010 * (1 + (z - 1) * 5)
		call gmark (gp, x, y, GM_CIRCLE, z, z)
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, x1, SF_W(sf), x2, SF_W(sf))
	    call gseti (gp, G_PLTYPE, 1)
	}

	dvx = vx[1,1] - vx[1,2]
	dvy = vy[2,1] - vy[2,2]
	if (abs (dvx) > 0.01 && abs (dvy) > 0.01) {
	    # (R,Y)
	    call gseti (gp, G_WCS, 3)
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gseti (gp, G_YLABELTICKS, YES)
	    call gseti (gp, G_XNMAJOR, 4)
	    call gseti (gp, G_XNMINOR, 0)
	    call gseti (gp, G_YNMAJOR, 6)
	    call gseti (gp, G_YNMINOR, 4)
	    call gsview (gp, vx[1,1], vx[1,2], vy[2,1], vy[2,2])
	    call gswind (gp, 0., 1., 0., 1.)
	    call gfill (gp, fa, fa[5], 4, GF_SOLID)

	    call gswind (gp, rmin, rmax, y1, y2)
	    call glabax (gp, "", SF_WTYPE(sf), "Line")

	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_N(sfs) == 0)
		    next
		x = SFS_W(sfs)
		y = SFD_Y(SFS_SFD(sfs,1))
		if (key == 1) {
		    z = sqrt (SFS_M(sfs) / SF_M(sf))
		    z = max (0.005, 0.03 * z)
		    call gmark (gp, x, y, GM_MAG, z, z)
		}
		if (SFS_F(sfs) < SF_F(sf))
		    call gseti (gp, G_PLCOLOR, 2)
		else
		    call gseti (gp, G_PLCOLOR, 3)
		z = min (2., SFS_W(sfs) / rbest)
		z = 0.010 * (1 + (z - 1) * 5)
		call gmark (gp, x, y, GM_CIRCLE, z, z)
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, SF_W(sf), y1, SF_W(sf), y2)
	    call gseti (gp, G_PLTYPE, 1)
	}

	dvx = vx[2,1] - vx[2,2]
	dvy = vy[3,1] - vy[3,2]
	if (abs (dvx) > 0.01 && abs (dvy) > 0.01) {
	    # (X,F)
	    call gseti (gp, G_WCS, 4)
	    call gseti (gp, G_XLABELTICKS, NO)
	    call gseti (gp, G_YLABELTICKS, YES)
	    call gseti (gp, G_XNMAJOR, 6)
	    call gseti (gp, G_XNMINOR, 4)
	    call gseti (gp, G_YNMAJOR, 4)
	    call gseti (gp, G_YNMINOR, 0)
	    call gsview (gp, vx[2,1], vx[2,2], vy[3,1], vy[3,2])
	    call gswind (gp, 0., 1., 0., 1.)
	    call gfill (gp, fa, fa[5], 4, GF_SOLID)

	    call gswind (gp, x1, x2, fmin, fmax)
	    call glabax (gp, "", "", "Focus")

	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_N(sfs) == 0)
		    next
		x = SFD_X(SFS_SFD(sfs,1))
		y = SFS_F(sfs)
		if (key == 1) {
		    z = sqrt (SFS_M(sfs) / SF_M(sf))
		    z = max (0.005, 0.03 * z)
		    call gmark (gp, x, y, GM_MAG, z, z)
		}
		if (SFS_F(sfs) < SF_F(sf))
		    call gseti (gp, G_PLCOLOR, 2)
		else
		    call gseti (gp, G_PLCOLOR, 3)
		z = min (2., SFS_W(sfs) / rbest)
		z = 0.010 * (1 + (z - 1) * 5)
		call gmark (gp, x, y, GM_CIRCLE, z, z)
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, x1, SF_F(sf), x2, SF_F(sf))
	    call gseti (gp, G_PLTYPE, 1)
	}

	dvx = vx[3,1] - vx[3,2]
	dvy = vy[2,1] - vy[2,2]
	if (abs (dvx) > 0.01 && abs (dvy) > 0.01) {
	    # (F,Y)
	    call gseti (gp, G_WCS, 5)
	    call gseti (gp, G_XLABELTICKS, YES)
	    call gseti (gp, G_YLABELTICKS, NO)
	    call gseti (gp, G_XNMAJOR, 4)
	    call gseti (gp, G_XNMINOR, 0)
	    call gseti (gp, G_YNMAJOR, 6)
	    call gseti (gp, G_YNMINOR, 4)
	    call gsview (gp, vx[3,1], vx[3,2], vy[2,1], vy[2,2])
	    call gswind (gp, 0., 1., 0., 1.)
	    call gfill (gp, fa, fa[5], 4, GF_SOLID)

	    call gswind (gp, fmin, fmax, y1, y2)
	    call glabax (gp, "", "Focus", "")

	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_N(sfs) == 0)
		    next
		x = SFS_F(sfs)
		y = SFD_Y(SFS_SFD(sfs,1))
		if (key == 1) {
		    z = sqrt (SFS_M(sfs) / SF_M(sf))
		    z = max (0.005, 0.03 * z)
		    call gmark (gp, x, y, GM_MAG, z, z)
		}
		if (SFS_F(sfs) < SF_F(sf))
		    call gseti (gp, G_PLCOLOR, 2)
		else
		    call gseti (gp, G_PLCOLOR, 3)
		z = min (2., SFS_W(sfs) / rbest)
		z = 0.010 * (1 + (z - 1) * 5)
		call gmark (gp, x, y, GM_CIRCLE, z, z)
		call gseti (gp, G_PLCOLOR, 1)
	    }

	    call gseti (gp, G_PLTYPE, 2)
	    call gline (gp, SF_F(sf), y1, SF_F(sf), y2)
	    call gseti (gp, G_PLTYPE, 1)
	}

	# Label window.
	call gseti (gp, G_WCS, 1)
	call gseti (gp, G_DRAWAXES, 0)
	call gsview (gp, vx[1,1], vx[3,2], vy[1,1], vy[3,2])
	call glabax (gp, title, "", "")

	dvx = vx[2,1] - vx[2,2]
	dvy = vy[2,1] - vy[2,2]
	if (abs (dvx) > 0.01 && abs (dvy) > 0.01) {
	    # (X,Y)
	    call gseti (gp, G_DRAWAXES, 3)
	    call gseti (gp, G_LABELTICKS, NO)
	    call gseti (gp, G_XNMAJOR, 6)
	    call gseti (gp, G_XNMINOR, 4)
	    call gseti (gp, G_YNMAJOR, 6)
	    call gseti (gp, G_YNMINOR, 4)
	    call gsview (gp, vx[2,1], vx[2,2], vy[2,1], vy[2,2])
	    call gswind (gp, 0., 1., 0., 1.)
	    call gfill (gp, fa, fa[5], 4, GF_SOLID)

	    call gswind (gp, x1, x2, y1, y2)
	    call glabax (gp, "", "", "")

	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_N(sfs) == 0)
		    next
		sfd = SFS_SFD(sfs,1)
		x = SFD_X(sfd)
		y = SFD_Y(sfd)
		if (key == 1) {
		    z = sqrt (SFS_M(sfs) / SF_M(sf))
		    z = max (0.005, 0.03 * z)
		    call gmark (gp, x, y, GM_MAG, z, z)
		}
		if (SFS_F(sfs) < SF_F(sf))
		    call gseti (gp, G_PLCOLOR, 2)
		else
		    call gseti (gp, G_PLCOLOR, 3)
		z = min (2., SFS_W(sfs) / rbest)
		z = 0.010 * (1 + (z - 1) * 5)
		call gmark (gp, x, y, GM_CIRCLE, z, z)
		call gseti (gp, G_PLCOLOR, 1)
	    }
	}
end
