include <fset.h>
include <imhdr.h>
include <mwset.h>

define	HELPFILE	"imcoords$src/wcsedit.key"

define	WCSCMDS	 ",?,show,update,quit,"
define	WCS_HELP	1
define	WCS_SHOW	2
define	WCS_UPDATE	3
define	WCS_QUIT	4

define	WCSPARS	 ",CRVAL,CRPIX,CD,LTV,LTM,WTYPE,AXTYPE,UNITS,LABEL,FORMAT,"
define	WCS_CRVAL	1
define	WCS_CRPIX	2
define	WCS_CD		3
define	WCS_LTV		4
define	WCS_LTM		5
define	WCS_WTYPE	6
define	WCS_AXTYPE	7
define	WCS_UNITS	8
define	WCS_LABEL	9
define	WCS_FORMAT	10

procedure t_wcsedit ()

bool	interactive, verbose, update, install
int	wcsdim, parno, naxes1, naxes2, ndim
pointer	sp, imtemplate, image, parameter, ax1list, ax2list, axes1, axes2
pointer	value, wcs, system
pointer	imlist, im, mwim, r, w, cd, ltm, ltv, iltm, nr, ncd
bool	clgetb(), streq(), wcs_iedit()
int	clgeti(), fstati(), wcs_decode_parno(), wcs_decode_axlist(), imtgetim()
int	mw_stati()
pointer	imtopen(), immap(), mw_openim(), mw_open()
errchk	mw_newsystem()

begin
	if (fstati (STDOUT, F_REDIR) == NO)
	    call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate working space.
	call smark (sp)
	call salloc (imtemplate, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (parameter, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (ax1list, SZ_FNAME, TY_CHAR)
	call salloc (ax2list, SZ_FNAME, TY_CHAR)
	call salloc (axes1, IM_MAXDIM, TY_INT)
	call salloc (axes2, IM_MAXDIM, TY_INT)
	call salloc (wcs, SZ_FNAME, TY_CHAR)
	call salloc (system, SZ_FNAME, TY_CHAR)

	# Get the list of images, parameter to be edited, axes lists,
	# and new parameter value.
	call clgstr ("image", Memc[imtemplate], SZ_FNAME)
	interactive = clgetb ("interactive")

	if (! interactive) {

	    # Get and check the wcs parameter to be edited.
	    call clgstr ("parameter", Memc[parameter], SZ_FNAME)
	    parno = wcs_decode_parno (Memc[parameter], SZ_FNAME)
	    if (parno <= 0) {
	        call printf ("%s is not a legal WCS parameter\n") 
		    call pargstr (Memc[parameter])
		call sfree (sp)
	        return
	    }

	    # Get the new parameter value.
	    call clgstr ("value", Memc[value], SZ_FNAME)

	    # Get the axes for which the parameter is to be edited.
	    call clgstr ("axes1", Memc[ax1list], SZ_FNAME)
	    if (parno == WCS_CD || parno == WCS_LTM)
	        call clgstr ("axes2", Memc[ax2list], SZ_FNAME)
	    else
		Memc[ax2list] = EOS

	    # Print any axis decoding error messages.
	    if (wcs_decode_axlist (parno, Memc[ax1list], Memc[ax2list],
	        IM_MAXDIM, Memi[axes1], naxes1, Memi[axes2], naxes2) == ERR) {
		if (naxes1 <= 0) {
	    	    call printf ("Error decoding axes1 list\n") 
		} else if ((Memi[axes1] < 1) || (Memi[axes1+naxes1-1] >
		    IM_MAXDIM)) {
	    	    call printf ("The axes1 values must be >= 1 and <= %d\n") 
			call pargi (IM_MAXDIM)
		} else if (naxes2 == 0) {
	    	    call printf ("Error decoding axes2 list\n") 
		} else if ((Memi[axes2] < 1) || (Memi[axes2+naxes2-1] >
		    IM_MAXDIM)) {
	    	    call printf ("The axes2 values must be >= 1 and <= %d\n") 
	        	call pargi (IM_MAXDIM)
		}
	        call sfree (sp)
	        return
	    }
	}

	# Get the remaining parameters.
	call clgstr ("wcs", Memc[wcs], SZ_FNAME)
	wcsdim = clgeti ("wcsdim")
	verbose = clgetb ("verbose")
	update = clgetb ("update")

	# Loop over the list of images
	imlist = imtopen (Memc[imtemplate])
	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {

	    # Remove any image section.
	    call imgimage (Memc[image], Memc[image], SZ_FNAME)

	    # Open the image and the wcs.
	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
	        im = immap (Memc[image], NEW_IMAGE, 0)
		IM_NDIM(im) = 0
		ndim = wcsdim
		mwim = mw_open (NULL, ndim)
		call mw_newsystem (mwim, Memc[wcs], ndim)
	    } else {
		mwim = mw_openim (im)
		iferr (call mw_ssystem (mwim, Memc[wcs])) {
		    call mw_close (mwim)
		    ndim = IM_NDIM(im)
		    mwim = mw_open (NULL, ndim)
		    call mw_newsystem (mwim, Memc[wcs], ndim)
		} else
		    ndim = mw_stati (mwim, MW_NPHYSDIM)
	    }
	    call mw_gsystem (mwim, Memc[system], SZ_FNAME)

	    # Allocate working memory.
	    call malloc (r, ndim * ndim, TY_DOUBLE)
	    call malloc (w, ndim * ndim, TY_DOUBLE)
	    call malloc (cd, ndim * ndim, TY_DOUBLE)
	    call malloc (ltm, ndim * ndim, TY_DOUBLE)
	    call malloc (ltv, ndim, TY_DOUBLE)
	    call malloc (iltm, ndim * ndim, TY_DOUBLE)
	    call malloc (nr, ndim * ndim, TY_DOUBLE)
	    call malloc (ncd, ndim * ndim, TY_DOUBLE)

	    # Compute the original world to logical transformation.
	    call mw_gwtermd (mwim, Memd[r], Memd[w], Memd[cd], ndim)
            call mw_gltermd (mwim, Memd[ltm], Memd[ltv], ndim)
            call mwvmuld (Memd[ltm], Memd[r], Memd[nr], ndim)
            call aaddd (Memd[nr], Memd[ltv], Memd[nr], ndim)
            call mwinvertd (Memd[ltm], Memd[iltm], ndim)
            call mwmmuld (Memd[cd], Memd[iltm], Memd[ncd], ndim)

	    # Edit the wcs.
	    if (interactive) {

		install = wcs_iedit (mwim, Memc[image], Memc[system],
		    Memd[ltv], Memd[ltm], Memd[w], Memd[nr], Memd[ncd],
		    ndim, verbose)

	    } else if (streq (Memc[wcs], "physical") || streq (Memc[wcs],
	        "world") || streq (Memc[wcs], Memc[system])) {

		install = false
	        if (Memi[axes1+naxes1-1] > ndim) {
		    call printf ("For image %s axes1 values must be <= %d\n")
		        call pargstr (Memc[image])
		        call pargi (ndim)
		} else if (Memi[axes2+max(1,naxes2)-1] > ndim) {
		    call printf (
		        "For image %s axes1,2 values must be <= %d\n")
		        call pargstr (Memc[image])
		        call pargi (ndim)
	        } else {

	            call wcs_edit (mwim, parno, Memi[axes1], naxes1,
		        Memi[axes2], naxes2, Memc[value], Memd[ltv],
			Memd[ltm], Memd[w], Memd[nr], Memd[ncd], ndim)

		    if (verbose)
	                call wcs_show (mwim, Memc[image], Memc[system],
			    Memd[ltv], Memd[ltm], Memd[w], Memd[nr],
			    Memd[ncd], ndim)

		    if (update)
		        install = true
		}

	    } else {
		call printf ("Cannot find wcs %s for image %s\n")
		    call pargstr (Memc[wcs])
		    call pargstr (Memc[image])
	    }


	    # Recompute and store the new wcs if update is enabled.
	    if (install) {
		call mw_sltermd (mwim, Memd[ltm], Memd[ltv], ndim)
		call mwmmuld (Memd[ncd], Memd[ltm], Memd[cd], ndim)
		call mwinvertd (Memd[ltm], Memd[iltm], ndim)
		call asubd (Memd[nr], Memd[ltv], Memd[r], ndim)
		call mwvmuld (Memd[iltm], Memd[r], Memd[nr], ndim)
		call mw_swtermd (mwim, Memd[nr], Memd[w], Memd[cd], ndim)
		call mw_saveim (mwim, im)
	    }

	    # Free the memory.
	    call mfree (r, TY_DOUBLE)
            call mfree (w, TY_DOUBLE)
            call mfree (cd, TY_DOUBLE)
            call mfree (ncd, TY_DOUBLE)
            call mfree (nr, TY_DOUBLE)
            call mfree (ltm, TY_DOUBLE)
            call mfree (ltv, TY_DOUBLE)
            call mfree (iltm, TY_DOUBLE)

	    call mw_close (mwim)
	    call imunmap (im)
	}

	call imtclose (imlist)
	call sfree (sp)
end


# WCS_IEDIT -- Interactively edit the wcs.

bool procedure wcs_iedit (mwim, image, system, ltv, ltm, w, r, cd, ndim,
	verbose)

pointer	mwim			# pointer to the current wcs
char	image[ARB]		# input image name
char	system[ARB]		# wcs system name
double	ltv[ARB]		# the lterm offsets
double	ltm[ndim,ARB]		# the lterm rotation matrix
double	w[ARB]			# the fits crval parameters
double	r[ARB]			# the fits crpix parameters
double	cd[ndim,ARB]		# the fits rotation matrix
int	ndim			# the dimension of the wcs
bool	verbose			# verbose mode

bool	update
int	cmd, parno, naxes1, naxes2
pointer	sp, parameter, value, ax1list, ax2list, axes1, axes2
int	clscan(), strdic(), nscan(), wcs_decode_parno(), wcs_decode_axlist()

begin
	# Allocate working memory.
	call smark (sp)
	call salloc (parameter, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (ax1list, SZ_FNAME, TY_CHAR)
	call salloc (ax2list, SZ_FNAME, TY_CHAR)
	call salloc (axes1, ndim, TY_INT)
	call salloc (axes2, ndim, TY_INT)

	# Print the starting wcs.
	if (verbose)
            call wcs_show (mwim, image, system, ltv, ltm, w, r, cd, ndim)

	# Loop over the command stream.
	update = false
	while (clscan ("commands") != EOF) {

	    # Get the command/parameter.
	    call gargwrd (Memc[parameter], SZ_FNAME)
	    if (nscan() < 1)
		next
	    cmd = strdic (Memc[parameter], Memc[parameter], SZ_FNAME, WCSCMDS)

	    switch (cmd) {
	    case WCS_HELP:
		call pagefile (HELPFILE, "")
	    case WCS_SHOW:
		call wcs_show (mwim, image, system, ltv, ltm, w, r, cd, ndim)
	    case WCS_UPDATE:
		update = true
		break
	    case WCS_QUIT:
		update = false
		break
	    default:
	        call gargwrd (Memc[value], SZ_FNAME)
	        call gargwrd (Memc[ax1list], SZ_FNAME)
	        call gargwrd (Memc[ax2list], SZ_FNAME)
		parno = wcs_decode_parno (Memc[parameter], SZ_FNAME)
		if (parno <= 0) {
	    	    call printf ("%s is not a legal WCS parameter\n") 
			call pargstr (Memc[parameter])
		} else if (nscan() < 2) {
		    call wcs_pshow (mwim, parno, image, system, ltv, ltm, w,
		        r, cd, ndim)
		} else if (wcs_decode_axlist (parno, Memc[ax1list],
		    Memc[ax2list], IM_MAXDIM, Memi[axes1], naxes1, Memi[axes2],
		    naxes2) == OK) {
		    call wcs_edit (mwim, parno, Memi[axes1], naxes1,
		        Memi[axes2], naxes2, Memc[value], ltv, ltm, w, r, cd,
			ndim)
		    if (verbose)
		        call wcs_pshow (mwim, parno, image, system, ltv, ltm,
			    w, r, cd, ndim)
		} else if (naxes1 <= 0) {
		    call printf ("Error decoding axes1 list\n")
		} else if ((Memi[axes1] < 1) || (Memi[axes1+naxes1-1] > ndim)) {
	    	    call printf ("The axes1 values must be >= 1 and <= %d\n") 
			call pargi (ndim)
		} else if (naxes2 <= 0) {
		    call printf ("Error decoding axes2 list\n")
		} else if ((Memi[axes2] < 1) || (Memi[axes2+naxes2-1] > ndim)) {
	    	    call printf ("The axes1 values must be >= 1 and <= %d\n") 
			call pargi (ndim)
		}
	    }
	}

	call sfree (sp)

	return (update)
end


# WCS_EDIT -- Edit the wcs.

procedure wcs_edit (mwim, parameter, axis1, naxis1, axis2, naxis2, value, ltv,
	ltm, w, r, cd, ndim)

pointer	mwim			# pointer to the current wcs
int	parameter		# parameter to be changed
int	axis1[ARB]		# list of axes1 for which to change value
int	naxis1			# number of axis for to change value
int	axis2[ARB]		# list of cross-term axes
int	naxis2			# number of cross-term axes
char	value[ARB]		# new wcs parameter value
double	ltv[ARB]		# the lterm offsets
double	ltm[ndim,ARB]		# the lterm rotation matrix
double	w[ARB]			# the fits crval parameters
double	r[ARB]			# the fits crpix parameters
double	cd[ndim,ARB]		# the fits rotation matrix
int	ndim			# the dimension of the wcs

double	dval
int	i, j, ip
int	ctod()

begin
	ip = 1
	switch (parameter) {
	case WCS_CRVAL:
	    if (ctod (value, ip, dval) > 0) {
		do i = 1, naxis1
	            w[axis1[i]] = dval
	    }
	case WCS_CRPIX:
	    if (ctod (value, ip, dval) > 0) {
		do i = 1, naxis1
	            r[axis1[i]] = dval
	    }
	case WCS_CD:
	    if (ctod (value, ip, dval) > 0) {
		if (naxis2 == 0) {
		    do i = 1, naxis1
	                cd[axis1[i],axis1[i]] = dval
		} else {
		    do i = 1, naxis1
			do j = 1, naxis2
	                    cd[axis2[j],axis1[i]] = dval
		}
	    }
	case WCS_LTV:
	    if (ctod (value, ip, dval) > 0) {
		do i = 1, naxis1
	            ltv[axis1[i]] = dval
	    }
	case WCS_LTM:
	    if (ctod (value, ip, dval) > 0) {
		if (naxis2 == 0) {
		    do i = 1, naxis1
	                ltm[axis1[i],axis1[i]] = dval
		} else {
		    do i = 1, naxis1
			do j = 1, naxis2
	                    ltm[axis1[i],axis2[j]] = dval
		}
	    }
	case WCS_WTYPE:
	    do i = 1, naxis1 {
		call mw_swtype (mwim, axis1[i], 1, value, "")
	        call mw_swattrs (mwim, axis1[i], "wtype", value)
	    }
	case WCS_AXTYPE:
	    do i = 1, naxis1
	        call mw_swattrs (mwim, axis1[i], "axtype", value)
	case WCS_UNITS:
	    do i = 1, naxis1
	        call mw_swattrs (mwim, axis1[i], "units", value)
	case WCS_LABEL:
	    do i = 1, naxis1
	        call mw_swattrs (mwim, axis1[i], "label", value)
	case WCS_FORMAT:
	    do i = 1, naxis1
	        call mw_swattrs (mwim, axis1[i], "format", value)
	default:
	    ;
	}
end


# WCS_SHOW -- Print a quick summary of the current wcs.

procedure wcs_show (mwim, image, system, ltv, ltm, w, r, cd, ndim)

pointer	mwim			# pointer to the current wcs
char	image[ARB]		# name of the imput image
char	system[ARB]		# name of the input wcs
double	ltv[ARB]		# the lterm offsets
double	ltm[ndim,ARB]		# the lterm rotation matrix
double	w[ARB]			# the fits crval parameters
double	r[ARB]			# the fits crpix parameters
double	cd[ndim,ARB]		# the fits rotation matrix
int	ndim			# the dimension of the wcs

int	i,j
pointer	sp, str
errchk	mw_gwattrs()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the image name and current wcs.
	call printf ("\nIMAGE: %s  CURRENT WCS: %s\n")
	    call pargstr (image)
	    call pargstr (system)

	# Print the axis banner.
	call printf ("   AXIS   ")
	do i = 1, ndim {
	    call printf ("%8d  ")
		call pargi (i)
	}
	call printf ("\n")

	# Print the crval parameters.
	call printf ("   CRVAL  ")
	do i = 1, ndim {
	    call printf ("%8g  ")
		call pargd (w[i])
	}
	call printf ("\n")

	# Print the crpix parameters.
	call printf ("   CRPIX  ")
	do i = 1, ndim {
	    call printf ("%8g  ")
		call pargd (r[i])
	}
	call printf ("\n")

	# Print the cd matrix.
	do i = 1, ndim {
	    call printf ("   CD %d   ")
		call pargi (i)
	    do j = 1, ndim {
	        call printf ("%8g  ")
		    call pargd (cd[j,i])
	    }
	    call printf ("\n")
	}

	# Print the ltv parameters.
	call printf ("   LTV    ")
	do i = 1, ndim {
	    call printf ("%8g  ")
		call pargd (ltv[i])
	}
	call printf ("\n")

	# Print the ltm matrix.
	do i = 1, ndim {
	    call printf ("   LTM %d  ")
		call pargi (i)
	    do j = 1, ndim {
	        call printf ("%8g  ")
		    call pargd (ltm[i,j])
	    }
	    call printf ("\n")
	}

	# Print the transformation type.
	call printf ("   WTYPE  ")
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mwim, i, "wtype", Memc[str], SZ_LINE))
		Memc[str] = EOS
	    call printf ("%8s  ")
		call pargstr (Memc[str])
	}
	call printf ("\n")

	# Print the axis type.
	call printf ("   AXTYPE ")
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mwim, i, "axtype", Memc[str], SZ_LINE))
		Memc[str] = EOS
	    call printf ("%8s  ")
		call pargstr (Memc[str])
	}
	call printf ("\n")

	# Print the units.
	call printf ("   UNITS  ")
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mwim, i, "units", Memc[str], SZ_LINE))
		Memc[str] = EOS
	    call printf ("%8s  ")
		call pargstr (Memc[str])
	}
	call printf ("\n")

	# Print the label.
	call printf ("   LABEL  ")
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mwim, i, "label", Memc[str], SZ_LINE))
		Memc[str] = EOS
	    call printf ("%8s  ")
		call pargstr (Memc[str])
	}
	call printf ("\n")

	# Print the format.
	call printf ("   FORMAT ")
	do i = 1, ndim {
	    iferr (call mw_gwattrs (mwim, i, "format", Memc[str], SZ_LINE))
		Memc[str] = EOS
	    call printf ("%8s  ")
		call pargstr (Memc[str])
	}
	call printf ("\n")

	call printf ("\n")

	call sfree (sp)
end


# WCS_PSHOW -- Print the current values of a specific parameter.

procedure wcs_pshow (mwim, parno, image, system, ltv, ltm, w, r, cd, ndim)

pointer	mwim			# pointer to the current wcs
int	parno			# print the parameter number
char	image[ARB]		# name of the imput image
char	system[ARB]		# name of the input wcs
double	ltv[ARB]		# the lterm offsets
double	ltm[ndim,ARB]		# the lterm rotation matrix
double	w[ARB]			# the fits crval parameters
double	r[ARB]			# the fits crpix parameters
double	cd[ndim,ARB]		# the fits rotation matrix
int	ndim			# the dimension of the wcs

int	i,j
pointer	sp, str
errchk	mw_gwattrs()

begin
	# Allocate working space.
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Print the image name and current wcs.
	call printf ("\nIMAGE: %s  CURRENT WCS: %s\n")
	    call pargstr (image)
	    call pargstr (system)

	# Print the axis banner.
	call printf ("   AXIS   ")
	do i = 1, ndim {
	    call printf ("%8d  ")
		call pargi (i)
	}
	call printf ("\n")

	switch (parno) {
	# Print the crval parameters.
	case WCS_CRVAL:
	    call printf ("   CRVAL  ")
	    do i = 1, ndim {
	        call printf ("%8g  ")
		    call pargd (w[i])
	    }
	    call printf ("\n")

	# Print the crpix parameters.
	case WCS_CRPIX:
	    call printf ("   CRPIX  ")
	    do i = 1, ndim {
	        call printf ("%8g  ")
		    call pargd (r[i])
	    }
	    call printf ("\n")

	# Print the cd matrix.
	case WCS_CD:
	    do i = 1, ndim {
	        call printf ("   CD %d   ")
		    call pargi (i)
	        do j = 1, ndim {
	            call printf ("%8g  ")
		        call pargd (cd[j,i])
	        }
	        call printf ("\n")
	    }

	# Print the ltv parameters.
	case WCS_LTV:
	    call printf ("   LTV    ")
	    do i = 1, ndim {
	        call printf ("%8g  ")
		    call pargd (ltv[i])
	    }
	    call printf ("\n")

	# Print the ltm matrix.
	case WCS_LTM:
	    do i = 1, ndim {
	        call printf ("   LTM %d  ")
		    call pargi (i)
	        do j = 1, ndim {
	            call printf ("%8g  ")
		        call pargd (ltm[i,j])
	        }
	        call printf ("\n")
	    }

	# Print the transformation type.
	case WCS_WTYPE:
	    call printf ("   WTYPE  ")
	    do i = 1, ndim {
	        iferr (call mw_gwattrs (mwim, i, "wtype", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	        call printf ("%8s  ")
		    call pargstr (Memc[str])
	    }
	    call printf ("\n")

	# Print the axis type.
	case WCS_AXTYPE:
	    call printf ("   AXTYPE ")
	    do i = 1, ndim {
	        iferr (call mw_gwattrs (mwim, i, "axtype", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	        call printf ("%8s  ")
		    call pargstr (Memc[str])
	    }
	    call printf ("\n")

	# Print the units.
	case WCS_UNITS:
	    call printf ("   UNITS  ")
	    do i = 1, ndim {
	        iferr (call mw_gwattrs (mwim, i, "units", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	        call printf ("%8s  ")
		    call pargstr (Memc[str])
	    }
	    call printf ("\n")

	# Print the label.
	case WCS_LABEL:
	    call printf ("   LABEL  ")
	    do i = 1, ndim {
	        iferr (call mw_gwattrs (mwim, i, "label", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	        call printf ("%8s  ")
		    call pargstr (Memc[str])
	    }
	    call printf ("\n")

	# Print the format.
	case WCS_FORMAT:
	    call printf ("   FORMAT ")
	    do i = 1, ndim {
	        iferr (call mw_gwattrs (mwim, i, "format", Memc[str], SZ_LINE))
		    Memc[str] = EOS
	        call printf ("%8s  ")
		    call pargstr (Memc[str])
	    }
	    call printf ("\n")
	default:
	    call printf ("Unknown WCS parameter\n")
	}
	call printf ("\n")

	call sfree (sp)
end


# WCS_DECODE_PARNO -- Decode the WCS parameter

int procedure wcs_decode_parno (parameter, maxch)

char	parameter[ARB]		# parameter name
int	maxch			# maximum length of parameter name

int	parno
int	strdic()

begin
	# Get and check the wcs parameter to be edited.
	call strupr (parameter)
	parno = strdic (parameter, parameter, maxch, WCSPARS)
	if (parno <= 0)
	    return (ERR)
	else
	    return (parno)
end


# WCS_DECODE_AXES -- Decode the axes lists.

int procedure wcs_decode_axlist (parno, ax1list, ax2list, max_naxes, axes1,
	naxes1, axes2, naxes2)

int	parno			# parameter to be edited
char	ax1list[ARB]		# principal axes list
char	ax2list[ARB]		# secondary axes list
int	max_naxes		# maximum number of axes to decode
int	axes1[ARB]		# list of principal axes to be edited
int	naxes1			# number of principal axes to be edited
int	axes2[ARB]		# list of secondary axes to be edited
int	naxes2			# number of secondary axes to be edited

int	wcs_getaxes()

begin
	naxes1 = wcs_getaxes (ax1list, axes1, max_naxes)
	if (naxes1 <= 0 || naxes1 > max_naxes)
	    return (ERR)
	else if ((axes1[1] < 1) || (axes1[naxes1] > max_naxes))
	    return (ERR)

	# Get the second list of axes.
	if ((parno == WCS_CD) || (parno == WCS_LTM)) {
	    naxes2 = wcs_getaxes (ax2list, axes2, max_naxes)
	    if (ax2list[1] == EOS)
		return (OK)
	    else if (naxes2 == 0)
	        return (ERR)
	    else if ((axes2[1] < 0) || (axes2[naxes2] > max_naxes))
	        return (ERR)
	} else {
	    naxes2 = naxes1
	    call amovi (axes1, axes2, naxes1)
	}

	return (OK)
end


define	MAX_NRANGES	10

# WCS_GETAXES -- Decode the input axis list.

int procedure wcs_getaxes (axlist, axes, max_naxes)

char	axlist[ARB]		# the axis list to be decoded
int	axes[ARB]		# the output decode axes
int	max_naxes		# the maximum number of output axes

int	naxes, axis, ranges[3,MAX_NRANGES+1]
int	decode_ranges(), get_next_number()

begin
	# Clear the axes array.
	call aclri (axes, max_naxes)

	# Check for a blank string.
	if (axlist[1] == EOS)
	    return (0)

	# Check for an illegal axis list string.
	if (decode_ranges (axlist, ranges, MAX_NRANGES, naxes) == ERR)
	    return (0)

	naxes = 0
	axis = 0
	while ((naxes < max_naxes) && (get_next_number (ranges, axis) != EOF)) {
	    naxes = naxes + 1
	    axes[naxes] = axis
	}

	return (naxes)
end
