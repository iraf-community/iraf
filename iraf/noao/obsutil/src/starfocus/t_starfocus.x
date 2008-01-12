include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<mach.h>
include	"starfocus.h"

define	HELP		"nmisc$src/starfocus.key"
define	PROMPT		"Options"


# T_STARFOCUS -- Stellar focusing task.

procedure t_starfocus ()

begin
	call starfocus (STARFOCUS)
end


# T_PSFMEASURE -- PSF measuring task.

procedure t_psfmeasure ()

begin
	call starfocus (PSFMEASURE)
end


# STARFOCUS -- Stellar focusing and PSF measuring main routine.

procedure starfocus (type)

int	type		#I Task type

int	list		# List of images
pointer	fvals		# Focus values
pointer	fstep		# Focus step
pointer	nexposures	# Number of exposures
pointer	step		# step in pixels
int	direction	# Step direction
int	gap		# Double step gap
int	coords		# Type of image data
bool	display		# Display images?
int	frame		# Display frame
int	logfd		# Log file descriptor
bool	ignore_sat	# Ignore saturation?

real	wx, wy, f, df, xstep, ystep
int	i, i1, i2, i3, j, k, l, ip, wcs, key, id, ncols, nlines
int	nexp, nsfd, nimages, nstars, ngraph, nmark
pointer	sp, sf, image, system, cmd, rg, mark, im, mw, ct
pointer	sfds, sfd

bool	clgetb(), streq()
real	clgetr(), imgetr(), stf_r2i()
int	clgeti(), clgwrd(), clgcur(), imtopenp(), imtgetim(), imgeti()
int	nowhite(), open(), rng_index(), strdic(), ctoi(), ctor()
pointer	rng_open(), immap(), mw_openim(), mw_sctran()
errchk	immap, open, imgetr, imgeti, mw_openim, mw_sctran
errchk	stf_find, stf_bkgd, stf_profile, stf_widths, stf_fwhms, stf_radius
errchk	stf_organize, stf_graph, stf_display

begin
	call smark (sp)
	call salloc (sf, SF, TY_STRUCT)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fvals, SZ_LINE, TY_CHAR)
	call salloc (fstep, SZ_LINE, TY_CHAR)
	call salloc (nexposures, SZ_LINE, TY_CHAR)
	call salloc (step, SZ_LINE, TY_CHAR)
	call salloc (system, SZ_LINE, TY_CHAR)
	call salloc (cmd, SZ_LINE, TY_CHAR)

	call aclri (Memi[sf], SF)
	SF_TASK(sf) = type

	# Set task parameters.
	switch (type) {
	case STARFOCUS:
	    call clgstr ("focus", Memc[fvals], SZ_LINE)
	    call clgstr ("fstep", Memc[fstep], SZ_LINE)
	    call clgstr ("nexposures", Memc[nexposures], SZ_LINE)
	    call clgstr ("step", Memc[step], SZ_LINE)

	    direction = clgwrd ("direction", Memc[cmd], SZ_LINE,
		"|-line|+line+|-column|+column|")
	    gap = clgwrd ("gap", Memc[cmd], SZ_LINE, "|none|beginning|end|") 

	    if (nowhite (Memc[fvals], Memc[fvals], SZ_LINE) != 0) {
		iferr (rg = rng_open (Memc[fvals], -MAX_REAL, MAX_REAL, 1.))
		    rg = NULL
	    } else
		rg = NULL
	case PSFMEASURE:
	    Memc[fvals] = EOS
	    rg = NULL
	    nexp = 1
	}

	list = imtopenp ("images")
	display = clgetb ("display")
	frame = clgeti ("frame")
	coords = clgwrd ("coords", Memc[cmd], SZ_LINE, SF_TYPES)
	call clgstr ("wcs", Memc[system], SZ_LINE)

	SF_XF(sf) = clgetr ("xcenter")
	SF_YF(sf) = clgetr ("ycenter")
	SF_LEVEL(sf) = clgetr ("level")
	SF_WCODE(sf) = clgwrd ("size", SF_WTYPE(sf), SF_SZWTYPE, SF_WTYPES)
	SF_BETA(sf) = clgetr ("beta")
	SF_SCALE(sf) = clgetr ("scale")
	SF_RADIUS(sf) = max (3., clgetr ("radius"))
	SF_NIT(sf) = clgeti ("iterations")
	SF_SBUF(sf) = clgetr ("sbuffer")
	SF_SWIDTH(sf) = clgetr ("swidth")
	SF_SAT(sf) = clgetr ("saturation")
	ignore_sat = clgetb ("ignore_sat")
	SF_OVRPLT(sf) = NO

	if (SF_LEVEL(sf) > 1.)
	    SF_LEVEL(sf) = SF_LEVEL(sf) / 100.
	SF_LEVEL(sf) = max (0.05, min (0.95, SF_LEVEL(sf)))

	# Accumulate the psf/focus data.
	key = 'm'
	mark = NULL
	nstars = 0
	nmark = 0
	ngraph = 0
	nimages = 0
	nsfd = 0
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    im = immap (Memc[image], READ_ONLY, 0)
	    call imseti (im, IM_TYBNDRY, TYBNDRY)
	    call imseti (im, IM_NBNDRYPIX, NBNDRYPIX)
	    if (streq (Memc[system], "logical")) {
		mw = NULL
		ct = NULL
	    } else {
		mw = mw_openim (im)
		ct = mw_sctran (mw, Memc[system], "logical", 03)
	    }
	    ncols = IM_LEN(im,1)
	    nlines = IM_LEN(im,2)
	    nimages = nimages + 1
	    if (nimages == 1) {
		SF_NCOLS(sf) = ncols
		SF_NLINES(sf) = nlines
		if (IS_INDEF(SF_XF(sf)))
		    SF_XF(sf) = (SF_NCOLS(sf) + 1) / 2.
		if (IS_INDEF(SF_YF(sf)))
		    SF_YF(sf) = (SF_NLINES(sf) + 1) / 2.
	    } else if (ncols!=SF_NCOLS(sf)||nlines!=SF_NLINES(sf))
		call eprintf ("WARNING: Images have different sizes\n")

	    # Display the image if needed.
	    if (display) {
		switch (coords) {
		case SF_MARK1:
		    if (nimages == 1)
			call stf_display (Memc[image], frame)
		case SF_MARKALL:
		    call stf_display (Memc[image], frame)
		}
		if (nimages == 1) {
		    call printf (
		 "** Select stars to measure with 'm' and finish with 'q'.\n")
		    call printf (
		 "** Additional options are '?', 'g', and :show.\n")
		call flush (STDOUT)
		}
	    }

	    # Accumulate objects.
	    repeat {
		switch (coords) {
		case SF_CENTER:
		    if (nstars == nimages)
			break
		    if (rg == NULL && Memc[fvals] == EOS)
			id = nstars
		    else
			id = 0
		    wx = 1 + (ncols - 1) / 2.
		    wy = 1 + (nlines - 1) / 2.
		    key = 0
		case SF_MARK1:
		    if (nimages == 1) {
			if (clgcur ("imagecur", wx, wy, wcs, key,
			    Memc[cmd], SZ_LINE) == EOF)
			    break
			switch (key) {
			case '?':
			    call pagefile (HELP, PROMPT)
			    next
			case ':':
			    if (strdic (Memc[cmd], Memc[cmd], SZ_LINE,
				"|show|") == 1) {
				if (nsfd > 0) {
				    call stf_organize (sf, sfds, nsfd)
				    call mktemp ("tmp$iraf", Memc[cmd], SZ_LINE)
				    logfd = open (Memc[cmd], APPEND, TEXT_FILE)
				    call stf_log (sf, logfd)
				    call close (logfd)
				    call pagefile (Memc[cmd], "starfocus")
				    call delete (Memc[cmd])
				}
			    }
			    next
			case 'q':
			    break
			}
			id = nstars
			if (mark == NULL)
			    call malloc (mark, 3*10, TY_REAL)
			else if (mod (nmark, 10) == 0)
			    call realloc (mark, 3*(nmark+10), TY_REAL)
			Memr[mark+3*nmark] = id
			Memr[mark+3*nmark+1] = wx
			Memr[mark+3*nmark+2] = wy
			nmark = nmark+1
		    } else {
			if (nmark == 0)
			    break
			if (nstars / nmark == nimages)
			    break
			i = mod (nstars, nmark)
			id = Memr[mark+3*i]
			wx = Memr[mark+3*i+1]
			wy = Memr[mark+3*i+2]
			key = 0
		    }
		    if (ct != NULL)
			call mw_c2tranr (ct, wx, wy, wx, wy)
		case SF_MARKALL:
		    if (clgcur ("imagecur", wx, wy, wcs, key,
			Memc[cmd], SZ_LINE) == EOF)
			break
		    switch (key) {
		    case '?':
			call pagefile (HELP, PROMPT)
			next
		    case ':':
			if (strdic(Memc[cmd],Memc[cmd],SZ_LINE,"|show|")==1) {
			    if (nsfd > 0) {
				call stf_organize (sf, sfds, nsfd)
				call mktemp ("tmp$iraf", Memc[cmd], SZ_LINE)
				logfd = open (Memc[cmd], APPEND, TEXT_FILE)
				call stf_log (sf, logfd)
				call close (logfd)
				call pagefile (Memc[cmd], "starfocus")
				call delete (Memc[cmd])
			    }
			}
			next
		    case 'q':
			break
		    }
		    id = nstars
		    if (ct != NULL)
			call mw_c2tranr (ct, wx, wy, wx, wy)
		}

		if (type == STARFOCUS) {
		    ip = 1
		    if (ctoi (Memc[nexposures], ip, nexp) == 0)
			nexp = imgeti (im, Memc[nexposures])
		    ip = 1
		    if (ctor (Memc[step], ip, f) == 0)
			f = imgetr (im, Memc[step])

		    xstep = 0.
		    ystep = 0.
		    switch (direction) {
		    case 1:
			ystep = -f
		    case 2:
			ystep = f
		    case 3:
			xstep = -f
		    case 4:
			xstep = f
		    }

		    # Set the steps and order of evaluation.
		    # This depends on which star in the sequence is marked.
		    # Below we assume the minimum x or maximum y is marked.

		    i1 = 1; i2 = nexp; i3 = 1
#		    if (xstep < 0.) {
#			i1 = nexp; i2 = 1; i3 = -1; xstep = -xstep
#		    }
#		    if (ystep > 0.) {
#			i1 = nexp; i2 = 1; i3 = -1; ystep = -ystep
#		    }
		} else {
		    i1 = 1; i2 = 1; i3 = 1
		}

		k = nsfd
		do i = i1, i2, i3 {
		    if (i > 1) {
			wx = wx + xstep
			wy = wy + ystep
			switch (gap) {
			case 2:
			    if ((i==2 && i3==1) || (i==1 && i3==-1)) {
				wx = wx + xstep
				wy = wy + ystep
			    }
			case 3:
			    if ((i==nexp && i3==1) || (i==nexp-1 && i3==-1)) {
				wx = wx + xstep
				wy = wy + ystep
			    }
			}
		    }
				
		    if (wx < SF_RADIUS(sf)-NBNDRYPIX ||
			wx > IM_LEN(im,1)-SF_RADIUS(sf)+NBNDRYPIX ||
		        wy < SF_RADIUS(sf)-NBNDRYPIX ||
			wy > IM_LEN(im,2)-SF_RADIUS(sf)+NBNDRYPIX)
			next
		    if (nexp == 1)
			j = nimages
		    else
			j = i
		    if (nsfd == 0)
			call malloc (sfds, 10, TY_POINTER)
		    else if (mod (nsfd, 10) == 0)
			call realloc (sfds, nsfd+10, TY_POINTER)
		    call malloc (sfd, SFD, TY_STRUCT)
		    call strcpy (Memc[image], SFD_IMAGE(sfd), SF_SZFNAME)
		    SFD_ID(sfd) = id
		    SFD_X(sfd) = wx
		    SFD_Y(sfd) = wy
		    if (Memc[fvals] == EOS)
			f = INDEF
		    else if (Memc[fstep] != EOS) {
			ip = 1
			if (ctor (Memc[fvals], ip, f) == 0)
			    f = imgetr (im, Memc[fvals])
			ip = 1
			if (ctor (Memc[fstep], ip, df) == 0)
			    df = imgetr (im, Memc[fstep])
			f = f + (i - 1) * df
		    } else if (rg != NULL) {
			if (rng_index (rg, j, f) == EOF)
			    call error (1, "Focus list ended prematurely")
		    } else
			f = imgetr (im, Memc[fvals])
		    SFD_F(sfd) = f
		    SFD_STATUS(sfd) = 0
		    SFD_SFS(sfd) = NULL
		    SFD_SFF(sfd) = NULL
		    SFD_SFI(sfd) = NULL

		    iferr {
			do l = 1, SF_NIT(sf) {
			    if (l == 1)
				SFD_RADIUS(sfd) = max (3., SF_RADIUS(sf))
			    else
				SFD_RADIUS(sfd) = max (3., 3. * SFD_DFWHM(sfd))
			    SFD_NPMAX(sfd) = stf_r2i (SFD_RADIUS(sfd)) + 1
			    SFD_NP(sfd) = SFD_NPMAX(sfd)
			    call stf_find (sf, sfd, im)
			    call stf_bkgd (sf, sfd)
			    if (SFD_NSAT(sfd) > 0 && l == 1) {
				if (ignore_sat)
				    call error (0,
				    "Saturated pixels found - ignoring object")
				else
				    call eprintf (
					"WARNING: Saturated pixels found.\n")
			    }
			    call stf_profile (sf, sfd)
			    call stf_widths (sf, sfd)
			    call stf_fwhms (sf, sfd)
			}
			Memi[sfds+nsfd] = sfd
			nsfd = nsfd + 1
			wx = SFD_X(sfd)
			wy = SFD_Y(sfd)
		    } then {
			call erract (EA_WARN)
			call mfree (sfd, TY_STRUCT)
		    }
		}
		if (nsfd > k) {
		    nstars = nstars + 1
		    if (key == 'g') {
			if (nsfd - k > 0) {
			    call stf_organize (sf, sfds+k, nsfd-k)
			    call stf_graph (sf)
			    ngraph = ngraph + 1
			}
		    }
		}
	    }
	    if (mw != NULL)
		call mw_close (mw)
	    call imunmap (im)
	}

	if (nsfd == 0)
	    call error (1, "No input data")

	# Organize the objects, graph the data, and log the results.
	if (nstars > 1 || ngraph != nstars) {
	    call stf_organize (sf, sfds, nsfd)
	    call stf_graph (sf)
	}
	call stf_log (sf, STDOUT)
	call clgstr ("logfile", Memc[image], SZ_FNAME)
	ifnoerr (logfd = open (Memc[image], APPEND, TEXT_FILE)) {
	    call stf_log (sf, logfd)
	    call close (logfd)
	}

	# Finish up
	call rng_close (rg)
	call imtclose (list)
	call stf_free (sf)
	do i = 1, SF_NSFD(sf) {
	    sfd = SF_SFD(sf,i)
	    call asifree (SFD_ASI1(sfd))
	    call asifree (SFD_ASI2(sfd))
	    call mfree (sfd, TY_STRUCT)
	}
	call mfree (SF_SFDS(sf), TY_POINTER)
	call mfree (mark, TY_REAL)
	call sfree (sp)
end


# STF_FREE -- Free the starfocus data structures.

procedure stf_free (sf)

pointer	sf			#I Starfocus structure
int	i

begin
	do i = 1, SF_NSTARS(sf)
	    call mfree (SF_SFS(sf,i), TY_STRUCT)
	do i = 1, SF_NFOCUS(sf)
	    call mfree (SF_SFF(sf,i), TY_STRUCT)
	do i = 1, SF_NIMAGES(sf)
	    call mfree (SF_SFI(sf,i), TY_STRUCT)
	call mfree (SF_STARS(sf), TY_POINTER)
	call mfree (SF_FOCUS(sf), TY_POINTER)
	call mfree (SF_IMAGES(sf), TY_POINTER)
	SF_NSTARS(sf) = 0
	SF_NFOCUS(sf) = 0
	SF_NIMAGES(sf) = 0
end


# STF_ORGANIZE -- Organize the individual object structures by star, focus,
# and image.  Compute focus, radius, and magnitude by group and over all
# data.

procedure stf_organize (sf, sfds, nsfd)

pointer	sf			#I Starfocus structure
pointer	sfds			#I Pointer to array of object structures
int	nsfd			#I Number of object structures

int	i, j, k, nstars, nfocus, nimages, key
real	f
pointer	stars, focus, images, sfd, sfs, sff, sfi
pointer	sp, image
bool	streq()
errchk	malloc

int	stf_focsort(), stf_magsort()
extern	stf_focsort, stf_magsort

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Free previous structures.
	call stf_free (sf)

	# Organize sfds by star.
	nstars = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    key = SFD_ID(Memi[sfds+i])
	    for (j = 0; SFD_ID(Memi[sfds+j]) != key; j = j + 1)
		;
	    if (j == i)
		nstars = nstars + 1
	}
	call malloc (stars, nstars, TY_POINTER)

	nstars = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    key = SFD_ID(Memi[sfds+i])
	    for (j = 0; j < nstars; j = j + 1)
		if (SFS_ID(Memi[stars+j]) == key)
		    break
	    if (j == nstars) {
		k = 0
		for (j = i; j < nsfd; j = j + 1)
		    if (SFD_ID(Memi[sfds+j]) == key)
			k = k + 1
		call malloc (sfs, SFS(k), TY_STRUCT)
		SFS_ID(sfs) = key
		SFS_NSFD(sfs) = k
		k = 0
		for (j = i; j < nsfd; j = j + 1) {
		    sfd = Memi[sfds+j]
		    if (SFD_ID(sfd) == key) {
			k = k + 1
			SFD_SFS(sfd) = sfs
			SFS_SFD(sfs,k) = sfd
		    }
		}
		Memi[stars+nstars] = sfs
		nstars = nstars + 1
	    }
	}

	# Organize sfds by focus values.  Sort by magnitude.
	nfocus = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    f = SFD_F(Memi[sfds+i])
	    for (j = 0; SFD_F(Memi[sfds+j]) != f; j = j + 1)
		;
	    if (j == i)
		nfocus = nfocus + 1
	}
	call malloc (focus, nfocus, TY_POINTER)

	nfocus = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    f = SFD_F(Memi[sfds+i])
	    for (j = 0; j < nfocus; j = j + 1)
		if (SFF_F(Memi[focus+j]) == f)
		    break
	    if (j == nfocus) {
		k = 0
		for (j = i; j < nsfd; j = j + 1)
		    if (SFD_F(Memi[sfds+j]) == f)
			k = k + 1
		call malloc (sff, SFF(k), TY_STRUCT)
		SFF_F(sff) = f
		SFF_NSFD(sff) = k
		k = 0
		for (j = i; j < nsfd; j = j + 1) {
		    sfd = Memi[sfds+j]
		    if (SFD_F(sfd) == f) {
			k = k + 1
			SFD_SFF(sfd) = sff
			SFF_SFD(sff,k) = sfd
		    }
		}
		Memi[focus+nfocus] = sff
		nfocus = nfocus + 1
	    }
	}

	# Organize sfds by image.
	nimages = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    call strcpy (SFD_IMAGE(Memi[sfds+i]), Memc[image], SZ_FNAME)
	    for (j = 0; !streq (SFD_IMAGE(Memi[sfds+j]), Memc[image]); j = j+1)
		;
	    if (j == i)
		nimages = nimages + 1
	}
	call malloc (images, nimages, TY_POINTER)

	nimages = 0
	for (i = 0; i < nsfd; i = i + 1) {
	    call strcpy (SFD_IMAGE(Memi[sfds+i]), Memc[image], SZ_FNAME)
	    for (j = 0; j < nimages; j = j + 1)
		if (streq (SFI_IMAGE(Memi[images+j]), Memc[image]))
		    break
	    if (j == nimages) {
		k = 0
		for (j = i; j < nsfd; j = j + 1)
		    if (streq (SFD_IMAGE(Memi[sfds+j]), Memc[image]))
			k = k + 1
		call malloc (sfi, SFI(k), TY_STRUCT)
		call strcpy (Memc[image], SFI_IMAGE(sfi), SF_SZFNAME)
		SFI_NSFD(sfi) = k
		k = 0
		for (j = i; j < nsfd; j = j + 1) {
		    sfd = Memi[sfds+j]
		    if (streq (SFD_IMAGE(sfd), Memc[image])) {
			k = k + 1
			SFD_SFI(sfd) = sfi
			SFI_SFD(sfi,k) = sfd
		    }
		}
		Memi[images+nimages] = sfi
		nimages = nimages + 1
	    }
	}
		    
	SF_NSFD(sf) = nsfd
	SF_SFDS(sf) = sfds
	SF_NSTARS(sf) = nstars
	SF_STARS(sf) = stars
	SF_NFOCUS(sf) = nfocus
	SF_FOCUS(sf) = focus
	SF_NIMAGES(sf) = nimages
	SF_IMAGES(sf) = images

	# Find the average and best focus values.  Sort the focus groups
	# by magnitude and the star groups by focus.

	call stf_fitfocus (sf)
	do i = 1, SF_NFOCUS(sf) {
	    sff = SF_SFF(sf,i)
	    call qsort (SFF_SFD(sff,1), SFF_NSFD(sff), stf_magsort)
	}
	do i = 1, SF_NSTARS(sf) {
	    sfs = SF_SFS(sf,i)
	    call qsort (SFS_SFD(sfs,1), SFS_NSFD(sfs), stf_focsort)
	}

	call sfree (sp)
end


# STF_LOG -- Print log of results

procedure stf_log (sf, fd)

pointer	sf			#I Main data structure
int	fd			#I File descriptor

int	i, j, n
pointer	sp, str, sfd, sfs, sff, sfi

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print banner and overall result.
	call sysid (Memc[str], SZ_LINE)
	call fprintf (fd, "%s\n\n")
	    call pargstr (Memc[str])

	# Print each individual object organized by image.
	call fprintf (fd, "%15.15s %7s %7s %7s")
	    call pargstr ("Image")
	    call pargstr ("Column")
	    call pargstr ("Line")
	    call pargstr ("Mag")
	if (IS_INDEF(SF_F(sf))) {
	    call fprintf (fd, " %7s")
		call pargstr (SF_WTYPE(sf))
	} else {
	    call fprintf (fd, " %7s %7s")
		call pargstr ("Focus")
		call pargstr (SF_WTYPE(sf))
	}
	if (SF_WCODE(sf) == 4) {
	    call fprintf (fd, " %4s")
		call pargstr ("Beta")
	}
	call fprintf (fd, " %7s %7s %3s\n")
	    call pargstr ("Ellip")
	    call pargstr ("PA")
	    call pargstr ("SAT")

	do i = 1, SF_NIMAGES(sf) {
	    sfi = SF_SFI(sf,i)
	    n = 0
	    do j = 1, SFI_NSFD(sfi) {
		sfd = SFI_SFD(sfi,j)
		if (SFD_STATUS(sfd) != 0)
		    next
		if (n == 0) {
		    call fprintf (fd, "%15.15s")
			call pargstr (SFD_IMAGE(sfd))
		} else
		    call fprintf (fd, "%15w")
		call fprintf (fd, " %7.2f %7.2f %7.2f")
		    call pargr (SFD_X(sfd))
		    call pargr (SFD_Y(sfd))
		    call pargr (-2.5*log10 (SFD_M(sfd) / SF_M(sf)))
		if (IS_INDEF(SFD_F(sfd))) {
		    call fprintf (fd,
			" %7.3f")
			call pargr (SFD_W(sfd))
		} else {
		    call fprintf (fd, " %7.6g %7.3f")
			call pargr (SFD_F(sfd))
			call pargr (SFD_W(sfd))
		}
		if (SF_WCODE(sf) == 4) {
		    call fprintf (fd, " %4.1f")
			call pargr (SFD_BETA(sfd))
		}
		call fprintf (fd, " %7.2f %7d")
		    call pargr (SFD_E(sfd))
		    call pargr (SFD_PA(sfd))
		if (SFD_NSAT(sfd) == 0)
		    call fprintf (fd, "\n")
		else
		    call fprintf (fd, "  *\n")
		n = n + 1
	    }
	}
	if (n > 0)
	    call fprintf (fd, "\n")

	# Print best values by star.
	if (SF_NS(sf) > 1) {
	    n = 0
	    do i = 1, SF_NSTARS(sf) {
		sfs = SF_SFS(sf,i)
		if (SFS_NF(sfs) > 1 || SFS_N(sfs) > 1) {
		    call stf_title (sf, NULL, sfs, NULL, Memc[str], SZ_LINE)
		    call fprintf (fd, "  %s\n")
			call pargstr (Memc[str])
		    n = n + 1
		}
	    }
	    if (n > 0)
		call fprintf (fd, "\n")
	}

	# Print averages at each focus.
	if (SF_NF(sf) > 1) {
	    n = 0
	    do i = 1, SF_NFOCUS(sf) {
		sff = SF_SFF(sf,i)
		if (SFF_N(sff) > 1) {
		    call stf_title (sf, NULL, NULL, sff, Memc[str], SZ_LINE)
		    call fprintf (fd, "  %s\n")
			call pargstr (Memc[str])
		    n = n + 1
		}
	    }
	    if (n > 0)
		call fprintf (fd, "\n")
	}

	call stf_title (sf, NULL, NULL, NULL, Memc[str], SZ_LINE)
	call fprintf (fd, "%s\n")
	    call pargstr (Memc[str])
end


# STF_TITLE -- Return result title string.
# The title is dependent on whether an overall title, a title for a star
# group, for a focus group, or for an indivdual object is desired.
# The title also is adjusted for the select size type and the number
# of objects in a group.

procedure stf_title (sf, sfd, sfs, sff, title, sz_title)

pointer	sf			#I Starfocus pointer
pointer	sfd			#I Data pointer
pointer	sfs			#I Star pointer
pointer	sff			#I Focus pointer
char	title[sz_title]		#O Title string 
int	sz_title		#I Size of title string 

pointer	ptr
int	i, fd, stropen()
errchk	stropen

begin
	fd = stropen (title, sz_title, WRITE_ONLY)

	if (sfd != NULL) {
	    call fprintf (fd, "%s @ (%.2f, %.2f):")
		call pargstr (SFD_IMAGE(sfd))
		call pargr (SFD_X(sfd))
		call pargr (SFD_Y(sfd))
	    switch (SF_WCODE(sf)) {
	    case 4:
		call fprintf (fd, " %s=%.2f (%3.1f), e=%.2f, pa=%d")
		    call pargstr (SF_WTYPE(sf))
		    call pargr (SFD_W(sfd))
		    call pargr (SFD_BETA(sfd))
		    call pargr (SFD_E(sfd))
		    call pargr (SFD_PA(sfd))
	    default:
		call fprintf (fd, " %s=%.2f, e=%.2f, pa=%d")
		    call pargstr (SF_WTYPE(sf))
		    call pargr (SFD_W(sfd))
		    call pargr (SFD_E(sfd))
		    call pargr (SFD_PA(sfd))
	    }
	    if (SFD_SFS(sfd) != NULL) {
		if (SFS_M(SFD_SFS(sfd)) != SF_M(sf)) {
		    call fprintf (fd, " , m=%.2f")
			call pargr (-2.5*log10 (SFS_M(SFD_SFS(sfd)) / SF_M(sf)))
		}
	    }
	    if (!IS_INDEF(SFD_F(sfd))) {
		call fprintf (fd, ", f=%.4g")
		    call pargr (SFD_F(sfd))
	    }
	} else if (sfs != NULL) {
	    ptr = SFS_SFD(sfs,1)
	    call fprintf (fd, "%s")
	    if (SFS_NF(sfs) > 1)
		call pargstr ("Best focus estimate")
	    else if (SFS_N(sfs) > 1)
		call pargstr ("Average star")
	    else {
		for (i=1; SFD_STATUS(SFS_SFD(sfs,i))!=0; i=i+1)
		    ;
		call pargstr (SFD_IMAGE(SFS_SFD(sfs,i)))
	    }
	    call fprintf (fd, " @ (%.2f, %.2f): %s=%.2f")
		    call pargr (SFD_X(ptr))
		    call pargr (SFD_Y(ptr))
		    call pargstr (SF_WTYPE(sf))
		    call pargr (SFS_W(sfs))
	    #if (SFS_M(sfs) != SF_M(sf)) {
		call fprintf (fd, ", m=%.2f")
		    call pargr (-2.5 * log10 (SFS_M(sfs) / SF_M(sf)))
	    #}
	    if (!IS_INDEF(SFS_F(sfs))) {
		call fprintf (fd, ", f=%.4g")
		    call pargr (SFS_F(sfs))
	    }
	} else if (sff != NULL) {
	    if (SFF_NI(sff) == 1) {
		for (i=1; SFD_STATUS(SFF_SFD(sff,i))!=0; i=i+1)
		    ;
		call fprintf (fd, "%s")
		    call pargstr (SFD_IMAGE(SFF_SFD(sff,i)))
		if (!IS_INDEF(SFF_F(sff))) {
		    call fprintf (fd, " at focus %.4g")
			call pargr (SFF_F(sff))
		}
		call fprintf (fd, " with average")
	    } else {
		if (IS_INDEF(SFF_F(sff)))
		    call fprintf (fd, "Average")
		else {
		    call fprintf (fd, "Focus %.4g with average")
			call pargr (SFF_F(sff))
		}
	    }
	    call fprintf (fd, " %s of %.2f")
		call pargstr (SF_WTYPE(sf))
		call pargr (SFF_W(sff))
	} else {
	    if (IS_INDEF(SF_F(sf))) {
		if (SF_WCODE(sf) == 1) {
		    call fprintf (fd, "  %s%d%% enclosed flux radius of ")
			if (SF_N(sf) > 1)
			    call pargstr ("Average ")
			else
			    call pargstr ("")
			call pargr (100 * SF_LEVEL(sf))
		} else {
		    if (SF_N(sf) > 1)
			call fprintf (fd,
			    "  Average full width at half maximum (%s) of ")
		    else
			call fprintf (fd,
			    "  Full width at half maximum (%s) of ")
		    call pargstr (SF_WTYPE(sf))
		}
		call fprintf (fd, "%.4f")
		    call pargr (SF_W(sf))
	    } else {
		call fprintf (fd, "  %s of %.6g with ")
		    if (SF_NS(sf) > 1) {
			if (SF_NF(sf) > 1)
			    call pargstr ("Average best focus")
			else
			    call pargstr ("Average focus")
		    } else {
			if (SF_NF(sf) > 1)
			    call pargstr ("Best focus")
			else
			    call pargstr ("Focus")
		    }
		    call pargr (SF_F(sf))
		if (SF_WCODE(sf) == 1) {
		    call fprintf (fd, "%d%% enclosed flux radius of ")
			call pargr (100 * SF_LEVEL(sf))
		} else {
		    call fprintf (fd, "%s of ")
			call pargstr (SF_WTYPE(sf))
		}
		call fprintf (fd, "%.2f")
		    call pargr (SF_W(sf))
	    }
	}

	call strclose (fd)
end


# STF_FITFOCUS -- Find the best focus.

procedure stf_fitfocus (sf)

pointer	sf			#I Starfocus pointer

int	i, j, k, n, jmin
pointer	x, y, sfd, sfs, sff, sfi
real	f, r, m, wr, wf
bool	fp_equalr()

begin
	# Set number of valid points, stars, focuses, images.
	SF_N(sf) = 0
	SF_YP1(sf) = 0
	SF_YP2(sf) = 0
	do i = 1, SF_NSFD(sf) {
	    sfd = SF_SFD(sf,i)
	    if (SFD_STATUS(sfd) == 0) {
		SF_N(sf) = SF_N(sf) + 1
		SF_YP1(sf) = min (SF_YP1(sf), SFD_YP1(sfd))
		SF_YP2(sf) = max (SF_YP2(sf), SFD_YP2(sfd))
	    }
	}
	SF_NS(sf) = 0
	do i = 1, SF_NSTARS(sf) {
	    sfs = SF_SFS(sf,i)
	    SFS_N(sfs) = 0
	    SFS_M(sfs) = 0.
	    SFS_NF(sfs) = 0
	    do j = 1, SFS_NSFD(sfs) {
		sfd = SFS_SFD(sfs,j)
		if (SFD_STATUS(SFS_SFD(sfs,j)) != 0)
		    next
		SFS_N(sfs) = SFS_N(sfs) + 1
		SFS_M(sfs) = SFS_M(sfs) + SFD_M(sfd)
		sff = SFD_SFF(sfd)
		for (k = 1; SFD_SFF(SFS_SFD(sfs,k)) != sff; k = k + 1)
		    ;
		if (k == j)
		    SFS_NF(sfs) = SFS_NF(sfs) + 1
	    }
	    if (SFS_N(sfs) > 0) {
		SFS_M(sfs) = SFS_M(sfs) / SFS_N(sfs)
		SF_NS(sf) = SF_NS(sf) + 1
	    }
	}
	SF_NF(sf) = 0
	do i = 1, SF_NFOCUS(sf) {
	    sff = SF_SFF(sf,i)
	    SFF_W(sff) = 0.
	    SFF_N(sff) = 0
	    SFF_NI(sff) = 0
	    wr = 0
	    do j = 1, SFF_NSFD(sff) {
		sfd = SFF_SFD(sff,j)
		if (SFD_STATUS(sfd) != 0)
		    next
		m = SFS_M(SFD_SFS(sfd))
		wr = wr + m
		SFF_W(sff) = SFF_W(sff) + m * SFD_W(sfd)
		SFF_N(sff) = SFF_N(sff) + 1
		sfi = SFD_SFI(sfd)
		for (k = 1; SFD_SFI(SFF_SFD(sff,k)) != sfi; k = k + 1)
		    ;
		if (k == j)
		    SFF_NI(sff) = SFF_NI(sff) + 1
	    }
	    if (SFF_N(sff) > 0) {
		SFF_W(sff) = SFF_W(sff) / wr
		SF_NF(sf) = SF_NF(sf) + 1
	    }
	}
	SF_NI(sf) = 0
	do i = 1, SF_NIMAGES(sf) {
	    sfi = SF_SFI(sf,i)
	    SFI_N(sfi) = 0
	    do j = 1, SFI_NSFD(sfi)
		if (SFD_STATUS(SFI_SFD(sfi,j)) == 0)
		    SFI_N(sfi) = SFI_N(sfi) + 1
	    if (SFI_N(sfi) > 0)
		SF_NI(sf) = SF_NI(sf) + 1
	}

	# Find the average magnitude, best focus, and radius for each star.
	# Find the brightest magnitude and average best focus and radius
	# over all stars.

	SF_BEST(sf) = SF_SFD(sf,1)
	SF_F(sf) = 0.
	SF_W(sf) = 0.
	SF_M(sf) = 0.
	SF_NS(sf) = 0
	wr = 0.
	wf = 0.
	do i = 1, SF_NSTARS(sf) {
	    sfs = SF_SFS(sf,i)
	    call malloc (x, SFS_NSFD(sfs), TY_REAL)
	    call malloc (y, SFS_NSFD(sfs), TY_REAL)
	    k = 0
	    n = 0
	    do j = 1, SFS_NSFD(sfs) {
		sfd = SFS_SFD(sfs,j)
		if (SFD_STATUS(sfd) != 0)
		    next
		r = SFD_W(sfd)
		f = SFD_F(sfd)
		if (!IS_INDEF(f))
		    k = k + 1
		Memr[x+n] = f
		Memr[y+n] = r
		n = n + 1
		if (r < SFD_W(SF_BEST(sf)))
		    SF_BEST(sf) = sfd
	    }

	    # Find the best focus and radius.
	    if (n == 0) {
		SFS_F(sfs) = INDEF
		SFS_W(sfs) = INDEF
		SFS_M(sfs) = INDEF
		SFS_N(sfs) = 0
	    } else if (k == 0) {
		call alimr (Memr[y], n, f, r)
		f = INDEF
		m = SFS_M(sfs)
		wr = wr + m
		SFS_F(sfs) = f
		SFS_W(sfs) = r
		SFS_M(sfs) = m
		SFS_N(sfs) = n
		SF_W(sf) = SF_W(sf) + m * r
		SF_M(sf) = max (SF_M(sf), m)
		SF_NS(sf) = SF_NS(sf) + 1
	    } else {
		SFS_N(sfs) = n
		if (k < n) {
		    k = 0
		    do j = 0, n-1 {
			if (!IS_INDEF(Memr[x+j])) {
			    Memr[x+k] = Memr[x+j]
			    Memr[y+k] = Memr[y+j]
			    k = k + 1
			}
		    }
		}
		call xt_sort2 (Memr[x], Memr[y], k)
		n = 0
		do j = 1, k-1 {
		    if (fp_equalr (Memr[x+j], Memr[x+n])) {
			if (Memr[y+j] < Memr[y+n])
			    Memr[y+n] = Memr[y+j]
		    } else {
			n = n + 1
			Memr[x+n] = Memr[x+j]
			Memr[y+n] = Memr[y+j]
		    }
		}
		n = n + 1

		# Find the minimum radius
		jmin = 0
		do j = 0, n-1
		    if (Memr[y+j] < Memr[y+jmin])
			jmin = j

		# Use parabolic interpolation to find the best focus
		if (jmin == 0 || jmin == n-1) {
		    f = Memr[x+jmin]
		    r = Memr[y+jmin]
		} else
		    call stf_parab (Memr[x+jmin-1], Memr[y+jmin-1], f, r)

		m = SFS_M(sfs)
		wr = wr + m
		wf = wf + m
		SFS_F(sfs) = f
		SFS_W(sfs) = r
		SFS_M(sfs) = m
		SF_F(sf) = SF_F(sf) + m * f
		SF_W(sf) = SF_W(sf) + m * r
		SF_M(sf) = max (SF_M(sf), m)
		SF_NS(sf) = SF_NS(sf) + 1
	    }
	    call mfree (x, TY_REAL)
	    call mfree (y, TY_REAL)
	}

	if (wr > 0.)
	    SF_W(sf) = SF_W(sf) / wr
	else {
	    SF_W(sf) = INDEF
	    SF_M(sf) = INDEF
	}
	if (wf > 0.)
	    SF_F(sf) = SF_F(sf) / wf
	else
	    SF_F(sf) = INDEF
end


# STF_PARAB -- Find the minimum of a parabolic fit to three points.

procedure stf_parab (x, y, xmin, ymin)

real	x[3]
real	y[3]
real	xmin
real	ymin

double	x12, x13, x23, x213, x223, y13, y23, a, b, c

begin
	x12 = x[1] - x[2]
	x13 = x[1] - x[3]
	x23 = x[2] - x[3]
	x213 = x13 * x13
	x223 = x23 * x23
	y13 = y[1] - y[3]
	y23 = y[2] - y[3]
	c = (y13 - y23 * x13 / x23) / (x213 - x223 * x13 / x23)
	b = (y23 - c * x223) / x23
	a = y[3]
	xmin = -b / (2 * c)
	ymin = a + b * xmin + c * xmin * xmin
	xmin = xmin + x[3]
end


# STF_MAGSORT -- Compare two star structures by average magnitude.

int procedure stf_magsort (sfd1, sfd2)

pointer	sfd1, sfd2		# Structures to compare
pointer	sfs1, sfs2		# Star structures for magnitudes

begin
	sfs1 = SFD_SFS(sfd1)
	sfs2 = SFD_SFS(sfd2)
	if (SFS_M(sfs1) > SFS_M(sfs2))
	    return (-1)
	else if (SFS_M(sfs1) < SFS_M(sfs2))
	    return (1)
	else
	    return (0)
end


# STF_FOCSORT -- Compare two star structures by focus.

int procedure stf_focsort (sfd1, sfd2)

pointer	sfd1, sfd2		# Structures to compare

begin
	if (SFD_F(sfd1) < SFD_F(sfd2))
	    return (-1)
	else if (SFD_F(sfd1) > SFD_F(sfd2))
	    return (1)
	else
	    return (0)
end


# STF_DISPLAY -- Display image if necessary.
# The user is required to display the first image separately.

procedure stf_display (image, frame)

char	image[ARB]		#I Image to display
int	frame			#I Display frame to use

int	i, status
pointer	sp, dname, ds, iw, imd_mapframe(), iw_open()
bool	xt_imnameeq()
errchk	clcmdw

begin
	call smark (sp)
	call salloc (dname, SZ_LINE, TY_CHAR)

	ds = imd_mapframe (1, READ_WRITE, NO)
	do i = 1, MAX_FRAMES {
	    iferr (iw = iw_open (ds, i, Memc[dname], SZ_LINE, status))
		next
	    call iw_close (iw)
	    if (xt_imnameeq (image, Memc[dname]))
		break
	}
	call imunmap (ds)

	if (!xt_imnameeq (image, Memc[dname])) {
	    call sprintf (Memc[dname], SZ_LINE, "display %s frame=%d fill=yes")
		call pargstr (image)
		call pargi (frame)
	    call clcmdw (Memc[dname])
	}

	call sfree (sp)
end


# STFCUR -- Debugging routine.
# Replace calls to clgcur with stfcur so that an text file containing the
# cursor coordinates may be specified when running standalone (such as
# under a debugger).

int procedure stfcur (cur, wx, wy, wcs, key, cmd, sz_cmd)

char	cur[ARB]		# Cursor name
real	wx, wy			# Cursor coordinate
int	wcs			# WCS
int	key			# Key
char	cmd[sz_cmd]		# Command
int	sz_cmd			# Size of command

int	fd, stat, open(), fscan()
pointer	fname
errchk	open

begin
	if (fd == NULL) {
	    call malloc (fname, SZ_FNAME, TY_CHAR) 
	    call clgstr (cur, Memc[fname], SZ_FNAME)
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    call mfree (fname, TY_CHAR)
	}

	stat = fscan (fd)
	if (stat == EOF) {
	    call close (fd)
	    return (stat)
	}

	call gargr (wx)
	call gargr (wy)
	call gargi (wcs)
	call gargi (key)

	return (stat)
end
