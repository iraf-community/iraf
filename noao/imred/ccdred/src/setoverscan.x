include	<imhdr.h>
include	<imset.h>
include <pkg/gtools.h>
include	<pkg/xtanswer.h>
include	"ccdred.h"


# SET_OVERSCAN -- Set the overscan vector.
#
#   1.  Return immediately if the overscan correction is not requested or
#	if the image has been previously corrected.
#   2.	Determine the overscan columns or lines.  This may be specifed
#	directly or indirectly through the image header or symbol table.
#   3.  Determine the type of overscan.
#   4.	If fitting the overscan average the overscan columns or lines and
#     	fit a function with the ICFIT routines to smooth the overscan vector.
#   5.  Set the processing flag.
#   6.  Log the operation (to user, logfile, and output image header).

procedure set_overscan (ccd)

pointer	ccd			# CCD structure pointer

int	i, first, last, navg, npts, type
int	nc, nl, c1, c2, l1, l2
pointer	sp, str, errstr, func, buf, x, overscan

int	clgwrd()
real	asumr()
bool	clgetb(), ccdflag()
pointer	imgl2r(), imgs2r()
errchk	imgl2r, imgs2r, fit_overscan

begin
	# Check if the user wants this operation or if it has been done.
	if (!clgetb ("overscan") || ccdflag (IN_IM(ccd), "overscan"))
	    return

	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (errstr, SZ_LINE, TY_CHAR)
	call salloc (func, SZ_LINE, TY_CHAR)
	call imstats (IN_IM(ccd), IM_IMAGENAME, Memc[str], SZ_LINE)

	# Check bias section.
	nc = IM_LEN(IN_IM(ccd),1)
	nl = IM_LEN(IN_IM(ccd),2)
	c1 = BIAS_C1(ccd)
	c2 = BIAS_C2(ccd)
	l1 = BIAS_L1(ccd)
	l2 = BIAS_L2(ccd)
	if ((c1 < 1) || (c2 > nc) || (l1 < 1) || (l2 > nl)) {
	    call sprintf (Memc[errstr], SZ_LINE,
		"Error in bias section: image=%s[%d,%d], biassec=[%d:%d,%d:%d]")
		call pargstr (Memc[str])
		call pargi (nc)
		call pargi (nl)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call error (0, Memc[errstr])
	}
	if ((c1 == 1) && (c2 == nc) && (l1 == 1) && (l2 == nl)) {
	    call error (0, "Bias section not specified or given as full image")
	}

	# If no processing is desired then print overscan strip and return.
	if (clgetb ("noproc")) {
	    call eprintf ("  [TO BE DONE] Overscan section is [%d:%d,%d:%d].\n")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
		call sfree (sp)
		return
	}

	# Determine the overscan section parameters. The readout axis
	# determines the type of overscan.  The step sizes are ignored.
	# The limits in the long dimension are replaced by the trim limits.

	type = clgwrd ("function", Memc[func], SZ_LINE, OVERSCAN_TYPES)
	if (type < OVERSCAN_FIT) {
	    overscan = NULL
	    if (READAXIS(ccd) == 2)
		call error (1,
		    "Overscan function type not allowed with readaxis of 2")
	} else {
	    if (READAXIS(ccd) == 1) {
		first = c1
		last = c2
		navg = last - first + 1
		npts = nl
		call salloc (buf, npts, TY_REAL)
		do i = 1, npts
		    Memr[buf+i-1] = asumr (Memr[imgs2r (IN_IM(ccd), first, last,
			i, i)], navg)
		if (navg > 1)
		    call adivkr (Memr[buf], real (navg), Memr[buf], npts)

		# Trim the overscan vector and set the pixel coordinate.
		npts = CCD_L2(ccd) - CCD_L1(ccd) + 1
		call malloc (overscan, npts, TY_REAL)
		call salloc (x, npts, TY_REAL)
		call trim_overscan (Memr[buf], npts, IN_L1(ccd), Memr[x],
		    Memr[overscan])

		call fit_overscan (Memc[str], c1, c2, l1, l2, Memr[x],
		    Memr[overscan], npts)

	    } else {
		first = l1
		last = l2
		navg = last - first + 1
		npts = nc
		call salloc (buf, npts, TY_REAL)
		call aclrr (Memr[buf], npts)
		do i = first, last
		    call aaddr (Memr[imgl2r(IN_IM(ccd),i)], Memr[buf],
			Memr[buf], npts)
		if (navg > 1)
		    call adivkr (Memr[buf], real (navg), Memr[buf], npts)

		# Trim the overscan vector and set the pixel coordinate.
		npts = CCD_C2(ccd) - CCD_C1(ccd) + 1
		call malloc (overscan, npts, TY_REAL)
		call salloc (x, npts, TY_REAL)
		call trim_overscan (Memr[buf], npts, IN_C1(ccd), Memr[x],
		    Memr[overscan])

		call fit_overscan (Memc[str], c1, c2, l1, l2, Memr[x],
		    Memr[overscan], npts)
	    }
	}
	
	# Set the CCD structure overscan parameters.
	CORS(ccd, OVERSCAN) = O
	COR(ccd) = YES
	OVERSCAN_TYPE(ccd) = type
	OVERSCAN_VEC(ccd) = overscan

	# Log the operation.
	if (type < OVERSCAN_FIT) {
	    call sprintf (Memc[str], SZ_LINE,
		"Overscan section is [%d:%d,%d:%d] with function=%s")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
		call pargstr (Memc[func])
	} else {
	    call sprintf (Memc[str], SZ_LINE,
		"Overscan section is [%d:%d,%d:%d] with mean=%g")
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
		call pargr (asumr (Memr[overscan], npts) / npts)
	}
	call timelog (Memc[str], SZ_LINE)
	call ccdlog (IN_IM(ccd), Memc[str])
	call hdmpstr (OUT_IM(ccd), "overscan", Memc[str])

	call sfree (sp)
end


# FIT_OVERSCAN -- Fit a function to smooth the overscan vector.
#   The fitting uses the ICFIT procedures which may be interactive.
#   Changes to these parameters are "learned".  The user is queried with a four
#   valued logical query (XT_ANSWER routine) which may be turned off when
#   multiple images are processed.

procedure fit_overscan (image, c1, c2, l1, l2, x, overscan, npts)

char	image[ARB]		# Image name for query and title
int	c1, c2, l1, l2		# Overscan strip
real	x[npts]			# Pixel coordinates of overscan
real	overscan[npts]		# Input overscan and output fitted overscan
int	npts			# Number of data points

int	interactive, fd
pointer	sp, str, w, ic, cv, gp, gt

int	clgeti(), ic_geti(), open()
real	clgetr(), ic_getr()
pointer	gopen(), gt_init()
errchk	gopen, open

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (w, npts, TY_REAL)
	call amovkr (1., Memr[w], npts)

	# Open the ICFIT procedures, get the fitting parameters, and
	# set the fitting limits.

	call ic_open (ic)
	call clgstr ("function", Memc[str], SZ_LINE)
	call ic_pstr (ic, "function", Memc[str])
	call ic_puti (ic, "order", clgeti ("order"))
	call clgstr ("sample", Memc[str], SZ_LINE)
	call ic_pstr (ic, "sample", Memc[str])
	call ic_puti (ic, "naverage", clgeti ("naverage"))
	call ic_puti (ic, "niterate", clgeti ("niterate"))
	call ic_putr (ic, "low", clgetr ("low_reject"))
	call ic_putr (ic, "high", clgetr ("high_reject"))
	call ic_putr (ic, "grow", clgetr ("grow"))
	call ic_putr (ic, "xmin", min (x[1], x[npts]))
	call ic_putr (ic, "xmax", max (x[1], x[npts]))
	call ic_pstr (ic, "xlabel", "Pixel")
	call ic_pstr (ic, "ylabel", "Overscan")

	# If the fitting is done interactively set the GTOOLS and GIO
	# pointers.  Also "learn" the fitting parameters since they may
	# be changed when fitting interactively.

	call sprintf (Memc[str], SZ_LINE,
	    "Fit overscan vector for %s interactively")
	    call pargstr (image)
	call set_interactive (Memc[str], interactive)
	if ((interactive == YES) || (interactive == ALWAYSYES)) {
	    gt = gt_init ()
	    call sprintf (Memc[str], SZ_LINE,
	        "Overscan vector for %s from section [%d:%d,%d:%d]\n")
	        call pargstr (image)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call gt_sets (gt, GTTITLE, Memc[str])
	    call gt_sets (gt, GTTYPE, "line")
	    call gt_setr (gt, GTXMIN, x[1])
	    call gt_setr (gt, GTXMAX, x[npts])
	    call clgstr ("graphics", Memc[str], SZ_FNAME)
	    gp = gopen (Memc[str], NEW_FILE, STDGRAPH)

	    call icg_fit (ic, gp, "cursor", gt, cv, x, overscan, Memr[w], npts)

	    call ic_gstr (ic, "function", Memc[str], SZ_LINE)
	    call clpstr ("function", Memc[str])
	    call clputi ("order", ic_geti (ic, "order"))
	    call ic_gstr (ic, "sample", Memc[str], SZ_LINE)
	    call clpstr ("sample", Memc[str])
	    call clputi ("naverage", ic_geti (ic, "naverage"))
	    call clputi ("niterate", ic_geti (ic, "niterate"))
	    call clputr ("low_reject", ic_getr (ic, "low"))
	    call clputr ("high_reject", ic_getr (ic, "high"))
	    call clputr ("grow", ic_getr (ic, "grow"))

	    call gclose (gp)
	    call gt_free (gt)
	} else
	    call ic_fit (ic, cv, x, overscan, Memr[w], npts, YES, YES, YES, YES)

	# Make a log of the fit in the plot file if given.
	call clgstr ("plotfile", Memc[str], SZ_LINE)
	call xt_stripwhite (Memc[str])
	if (Memc[str] != EOS) {
	    fd = open (Memc[str], APPEND, BINARY_FILE)
	    gp = gopen ("stdvdm", NEW_FILE, fd)
	    gt = gt_init ()
	    call sprintf (Memc[str], SZ_LINE,
	        "Overscan vector for %s from section [%d:%d,%d:%d]\n")
	        call pargstr (image)
		call pargi (c1)
		call pargi (c2)
		call pargi (l1)
		call pargi (l2)
	    call gt_sets (gt, GTTITLE, Memc[str])
	    call gt_sets (gt, GTTYPE, "line")
	    call gt_setr (gt, GTXMIN, 1.)
	    call gt_setr (gt, GTXMAX, real (npts))
	    call icg_graphr (ic, gp, gt, cv, x, overscan, Memr[w], npts)
	    call gclose (gp)
	    call close (fd)
	    call gt_free (gt)
	}

	# Replace the raw overscan vector with the smooth fit.
	call cvvector (cv, x, overscan, npts)

	# Finish up.
	call ic_closer (ic)
	call cvfree (cv)
	call sfree (sp)
end


# TRIM_OVERSCAN -- Trim the overscan vector.

procedure trim_overscan (data, npts, start, x, overscan)

real	data[ARB]		# Full overscan vector
int	npts			# Length of trimmed vector
int	start			# Trim start
real	x[npts]			# Trimmed pixel coordinates (returned)
real	overscan[npts]		# Trimmed overscan vector (returned)

int	i, j

begin
	do i = 1, npts {
	    j = start + i - 1
	    x[i] = j
	    overscan[i] = data[j]
	}
end
