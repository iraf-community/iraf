# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	<imset.h>

# MINMAX -- Update the minimum and maximum pixel values of an image.  This is
# done only if the values are absent or invalid, unless the force flag is set.
# The header values are not updated when computing the min/max of an image
# section unless the force flag is set.  The values are printed on the standard
# output as they are computed, if the verbose option is selected.

procedure t_minmax()

pointer	images			# image name template
bool	force			# force recomputation of values
bool	update			# update values in image header
bool	verbose			# print values as they are computed

int	list
bool	section
long	vmin[IM_MAXDIM], vmax[IM_MAXDIM]
real	minval, maxval
pointer	im, sp, pixmin, pixmax, imname, imsect

pointer	immap()
bool	clgetb()
long	clktime()
int	imtopen(), imtgetim()
define	tryagain_ 91

begin
	call smark (sp)
	call salloc (images, SZ_LINE, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (imsect, SZ_FNAME, TY_CHAR)
	call salloc (pixmin, SZ_FNAME, TY_CHAR)
	call salloc (pixmax, SZ_FNAME, TY_CHAR)

	# Get list of input images.

	call clgstr ("images", Memc[images], SZ_LINE)
	list = imtopen (Memc[images])

	# Get switches.

	force   = clgetb ("force")
	update  = clgetb ("update")
	verbose = clgetb ("verbose")

	# Process each image in the list.

	while (imtgetim (list, Memc[imname], SZ_FNAME) != EOF) {
	    call imgsection (Memc[imname], Memc[imsect], SZ_FNAME)
	    section = (Memc[imsect] != EOS)

	    call strcpy ("", Memc[pixmin], SZ_FNAME)
	    call strcpy ("", Memc[pixmax], SZ_FNAME)

	    if (update) {
		iferr (im = immap (Memc[imname], READ_WRITE, 0))
		    goto tryagain_

		if (force || (IM_LIMTIME(im) < IM_MTIME(im))) {
		    call im_vminmax (im, minval, maxval, vmin, vmax)
		    call mkoutstr (vmin, IM_NDIM(im), Memc[pixmin], SZ_FNAME)
		    call mkoutstr (vmax, IM_NDIM(im), Memc[pixmax], SZ_FNAME)
		    if (!section) {
			IM_MIN(im) = minval
			IM_MAX(im) = maxval
			IM_LIMTIME(im) = clktime (long(0))
			call imseti (im, IM_WHEADER, YES)
		    }
		} else {
		    minval = IM_MIN(im)
		    maxval = IM_MAX(im)
		}

		call imunmap (im)

	    } else {
tryagain_	iferr (im = immap (Memc[imname], READ_ONLY, 0)) {
		    call erract (EA_WARN)
		    next
		} else {
		    if (force || IM_LIMTIME(im) < IM_MTIME(im)) {
			call im_vminmax (im, minval, maxval, vmin, vmax)
			call mkoutstr (vmin, IM_NDIM(im), Memc[pixmin],
			    SZ_FNAME)
			call mkoutstr (vmax, IM_NDIM(im), Memc[pixmax],
			    SZ_FNAME)
		    } else {
			minval = IM_MIN(im)
			maxval = IM_MAX(im)
		    }
		    call imunmap (im)
		}
	    }

	    # make the section strings

	    if (verbose) {
		call printf ("    %s %s %g %s %g\n")
		    call pargstr (Memc[imname])
		    call pargstr (Memc[pixmin])
		    call pargr (minval)
		    call pargstr (Memc[pixmax])
		    call pargr (maxval)
		call flush (STDOUT)
	    }
	}

	# Return the computed values of the last image examined as CL
	# parameters.

	call clputr ("minval", minval)
	call clputr ("maxval", maxval)
	call clpstr ("minpix", Memc[pixmin], SZ_FNAME)
	call clpstr ("maxpix", Memc[pixmax], SZ_FNAME)

	call sfree (sp)
end

# MKOUTSTR --

procedure mkoutstr (v, ndim, outstr, maxch)

long	v[ARB]		# imio v vector
int	ndim		# number of dimensions
char	outstr[ARB]	# output string
int	maxch		# maximum length of string

int	i, ip, nchars

int	ltoc()

begin
	# encode opening brackett
	outstr[1] = '['

	# encode v vector values
	ip = 2
	do i = 1, ndim {
	    nchars = ltoc (v[i], outstr[ip], maxch)
	    ip = ip + nchars
	    outstr[ip] = ','
	    ip = ip + 1
	}

	# encode closing bracketts and EOS
	outstr[ip-1] = ']'
	outstr[ip] = EOS
end
