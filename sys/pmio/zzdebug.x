# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<ctype.h>
include	<fset.h>
include	<pmset.h>
include	<plio.h>

task	pmtest	= t_pmtest,
	mkmask	= t_mkmask,
	pmcopy	= t_pmcopy,
	mio	= t_mio


# MKMASK -- Make a mask for the given image.

procedure t_mkmask()

char	image[SZ_FNAME]
char	mask[SZ_FNAME]
char	cmdfile[SZ_FNAME]
char	cmd[SZ_FNAME]

pointer	im, pm
int	x, y, r, x1, y1, x2, y2, fd
pointer	immap(), pm_newmask()
int	open(), nscan(), fscan()
bool	streq()

begin
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("mask", mask, SZ_FNAME)

	# Open the image and an empty mask.
	im = immap (image, READ_ONLY, 0)
	pm = pm_newmask (im, 1)

	# Get the list of commands to be processed.
	call clgstr ("cmdfile", cmdfile, SZ_FNAME)
	fd = open (cmdfile, READ_ONLY, TEXT_FILE)

	# Process the commands and draw the mask.
	while (fscan (fd) != EOF) {
	    call gargwrd (cmd, SZ_FNAME)
	    if (nscan() < 1)
		break

	    if (streq (cmd, "point")) {
		# Command: point x y
		call gargi (x)
		call gargi (y)
		if (nscan() < 3) {
		    call eprintf ("point: bad arg list\n")
		    next
		}
		
		call eprintf ("point %d %d\n")
		    call pargi (x);  call pargi (y)
		call pm_point (pm, x, y, PIX_SET + PIX_VALUE(1))

	    } else if (streq (cmd, "circle")) {
		# Command: circle x y r
		call gargi (x)
		call gargi (y)
		call gargi (r)
		if (nscan() < 4) {
		    call eprintf ("circle: bad arg list\n")
		    next
		}

		call eprintf ("circle %d %d %d\n")
		    call pargi (x);  call pargi (y);  call pargi (r)
		call pm_circle (pm, x, y, r, PIX_SET + PIX_VALUE(1))

	    } else if (streq (cmd, "box")) {
		# Command: box x1 y1 x2 y2
		call gargi (x1);  call gargi (y1)
		call gargi (x2);  call gargi (y2)
		if (nscan() < 5) {
		    call eprintf ("box: bad arg list\n")
		    next
		}

		call eprintf ("box %d %d %d %d\n")
		    call pargi (x1);  call pargi (y1)
		    call pargi (x2);  call pargi (y2)
		call pm_box (pm, x1,y1, x2,y2, PIX_SET + PIX_VALUE(1))

	    } else {
		call eprintf ("bad command %s\n")
		    call pargstr (cmd)
	    }

	    # call pm_debug (pm, STDERR, 80, PD_INDEX)
	}

	# Save the mask in a file.
	call pm_savef (pm, mask, "mkmask", 0)

	call pm_close (pm)
	call imunmap (im)
end


# PMCOPY -- Copy an image mask.

procedure t_pmcopy

char	refim[SZ_FNAME]
char	mask[SZ_FNAME], newmask[SZ_FNAME], title[SZ_LINE]

pointer	im, old_pm, new_pm
long	vs[PM_MAXDIM], vn[PM_MAXDIM]
pointer	immap(), pm_open(), pm_newmask()
int	pm_stati()

begin
	call clgstr ("mask", mask, SZ_FNAME)
	call clgstr ("refim", refim, SZ_FNAME)
	call clgstr ("newmask", newmask, SZ_FNAME)

	# Open reference image.
	im = immap (refim, READ_ONLY, 0)

	# Open old mask.
	old_pm = pm_open (NULL)
	call pm_loadf (old_pm, mask, title, SZ_LINE)
	call pm_seti (old_pm, P_REFIM, im)

	# Create a new mask.
	new_pm = pm_newmask (im, pm_stati(old_pm,P_DEPTH))

	# Copy the mask.
	call amovkl (1, vs, PM_MAXDIM)
	call amovkl (IM_LEN(im,1), vn, PM_MAXDIM)
	call pm_rop (old_pm, vs, new_pm, vs, vn, PIX_SRC)

	# Save in a file.
	call pm_savef (new_pm, newmask, title, 0)

	call pm_close (new_pm)
	call pm_close (old_pm)
	call imunmap (im)
end


# MIO -- Test MIO.

procedure t_mio()

char	image[SZ_FNAME]
char	mask[SZ_FNAME]

real	rsum
pointer	im, mp, pm, bp
bool	debug, usefullimage
long	v[IM_MAXDIM], vs[2], ve[2]
int	mval, npix, totpix

real	asums()
bool	clgetb()
pointer	immap(), mio_open()
int	clgeti(), mio_glsegs(), mio_stati(), clscan(), nscan()

begin
	call clgstr ("image", image, SZ_FNAME)
	call clgstr ("mask", mask, SZ_FNAME)
	debug = clgetb ("debug")

	im = immap (image, READ_ONLY, 0)
	mp = mio_open (mask, clgeti("flags"), im)

	# The following assumes a 2D image.
	usefullimage = true
	if (clscan ("region") != EOF) {
	    call gargi(vs[1]);  call gargi (vs[2])
	    call gargi(ve[1]);  call gargi (ve[2])
	    usefullimage = (nscan() != 4)
	}
	if (usefullimage) {
	    call amovkl (1, vs, 2)
	    call amovl (IM_LEN(im,1), ve, 2)
	}
	call mio_setrange (mp, vs, ve, 2)

	if (debug) {
	    pm = mio_stati (mp, P_PMDES)
	    call pm_debug (pm, STDERR, 80, PD_LLOUT)
	}

	totpix = 0
	rsum = 0.0

	while (mio_glsegs (mp, bp, mval, v, npix) != EOF) {
	    if (debug) {
		call eprintf ("x=%3d, y=%3d, n=%3d, mval=%o\n")
		    call pargl (v[1])
		    call pargl (v[2])
		    call pargi (npix)
		    call pargi (mval)
	    }
	    totpix = totpix + npix
	    rsum = rsum + asums (Mems[bp], npix)
	}

	call eprintf ("totpix=%d, sum=%g, mean=%g\n")
	    call pargi (totpix)
	    call pargr (rsum)
	    if (totpix == 0)
		call pargr (INDEFR)
	    else
		call pargr (rsum / totpix)

	call mio_close (mp)
	call imunmap (im)
end
