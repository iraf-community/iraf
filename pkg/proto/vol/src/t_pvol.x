include <ctype.h>
include <imhdr.h>
include <time.h>
include "pvol.h"

define	iwrapup_	91
define	mwrapup_	92


# PVOL -- Project Volume.  Given an input datacube, produce a series of
# frames representing projections at stepped rotations around the cube,
# using voxel intensity and/or opacity information.  This is a form of
# volume rendering.

procedure t_pvol

pointer	input, output, sp, tmpstr, vp, timestr, im1, im2
long	clock1, clock2, elapclock, cpu1, cpu2, elapcpu
bool	need_lims, use_both
real	tmpmin, tmpmax

pointer immap()
int	clgeti(), clktime(), cputime()
bool	clgetb()
real	clgetr()

begin
	call smark (sp)
	call salloc (tmpstr, SZ_LINE, TY_CHAR)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (timestr, SZ_FNAME, TY_CHAR)

	# Allocate storage for volume projection descriptor.
	call malloc (vp, LEN_VP, TY_STRUCT)

	# Input parameters.
	if (clgetb ("verbose"))
	    VERBOSE(vp) = YES
	else
	    VERBOSE(vp) = NO
	call clgstr ("input", Memc[input], SZ_FNAME)
	call clgstr ("output", Memc[output], SZ_FNAME)

	# Geometric projection parameters:
	DEGREES(vp) = clgetr ("degrees")
	INIT_THETA(vp) = clgetr ("theta0")
	NFRAMES(vp) = clgeti ("nframes")
	if (IS_INDEFI(NFRAMES(vp)) && !IS_INDEFR(DEGREES(vp)))
	    NFRAMES(vp) = int (360.0 / DEGREES(vp))
	else if (IS_INDEFR(DEGREES(vp)) && !IS_INDEFI(NFRAMES(vp)))
	    DEGREES(vp) = 360.0 / NFRAMES(vp)
	else if (IS_INDEFR(DEGREES(vp)) && IS_INDEFI(NFRAMES(vp))) {
	    NFRAMES(vp) = 36
	    DEGREES(vp) = 10.0
	}
	PTYPE(vp) = clgeti ("ptype")
	VIMIN(vp) = clgetr ("imin")
	VIMAX(vp) = clgetr ("imax")
	IZERO(vp) = clgetr ("izero")
	OSCALE(vp) = clgetr ("oscale")
	OMIN(vp) = clgetr ("omin")
	OMAX(vp) = clgetr ("omax")
	AMIN(vp) = clgetr ("amin")
	AMAX(vp) = clgetr ("amax")
	DISCUTOFF(vp) = NO
	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN) {
	    DISPOWER(vp) = clgetr ("dispower")
	    if (clgetb ("discutoff"))
		DISCUTOFF(vp) = YES
	}
	if (PTYPE(vp) == P_MODN)
	    MODN(vp) = clgeti ("modn")
	VECX(vp) = clgetr ("vecx")
	VECY(vp) = clgetr ("vecy")
	VECZ(vp) = clgetr ("vecz")

	MAX_WS(vp) = clgeti ("maxws")

	# In prototype, the incremental algorithm is only implemented for
	# rotations about the X axis, counterclockwise when viewed from +X
	# looking back toward the origin.

	if (!(VECX(vp) == +1.0 && VECY(vp) == 0.0 && VECZ(vp) == 0.0)) {
	    call eprintf ("ERROR:  Only +X axis rotations supported with")
	    call eprintf (" incremental alg. at present\n")
	    call error (0, "Unsupported feature")
	}

	# Open images.
	im1 = immap (Memc[input], READ_ONLY, 0)
	im2 = immap (Memc[output], NEW_IMAGE, 0)
	call clgstr ("title", IM_TITLE(im1), SZ_IMTITLE)

	# If input image is 4d, with 2 elements in 4th dimension, one of them
	# must be opacity and the other intensity.  If someone wants to merge
	# two or more sets of intensity data, they can make independent runs
	# of PVOL and merge the outputs using RGB displays.

	use_both = false
	OPACELEM(vp) = INDEFI
	if (IM_NDIM(im1) == 4 && PTYPE(vp) != P_ATTENUATE) {
	    if (IM_LEN(im1,4) > 2)
		call error (0, "Don't know how to handle 4d image w/ >2 elems")
	    else if (IM_LEN(im1,4) == 2) {
		OPACELEM(vp) = clgeti ("opacelem")
		if (PTYPE(vp) == P_LASTONLY) {
		    call eprintf ("Warning: cannot use ptype LASTONLY with ")
		    call eprintf ("combined opacity/intensity data.\n")
		    PTYPE(vp) = P_SUM
		    call eprintf ("         resetting ptype = %d (SUM)\n")
			call pargi (PTYPE(vp))
		}
		use_both = true
		if (VERBOSE(vp) == YES)
		    call eprintf ("4D image, using both opacity & intensity.\n")
	    } else
		OPACELEM(vp) = INDEFI
	} else if (IM_NDIM(im1) > 4)
	    call error (0, "Don't know how to handle > 4d image")

	# Determine voxel intensity minimum & maximum for all intensity
	# transformations.  Both a specified intensity min & max and an
	# image min & max are required in the intensity transformation step
	# function:  if image min & max are up to date in the image header,
	# they will be used for image min & max; and if task parameters
	# imin, imax are NOT supplied, they will be set equal to image min
	# & max.  Likewise, if image min & max are not present, but
	# task params imin,imax are, the image min & max will be set to
	# imin,imax for duration of PVOL execution.  If neither are supplied,
	# the image min & max will be calculated but not updated (might not
	# have write access, user might not want them updated); however, if
	# verbose is on, the user will be warned to run MINMAX on the image
	# in the future to save time.
	
	if (PTYPE(vp) == P_ATTENUATE || use_both) {
	    # Get opacity transformation function parameters.
	    if (IS_INDEFR(OMIN(vp)))
		OMIN(vp) = IIMIN(vp)
	    if (IS_INDEFR(OMAX(vp)))
		OMAX(vp) = IIMAX(vp)
	    if (OMAX(vp) - OMIN(vp) <= 0.0) {
		call eprintf ("Error:  Invalid omin / omax (%g : %g)\n")
		    call pargr (OMIN(vp))
		    call pargr (OMAX(vp))
		goto iwrapup_
	    }
	}

	if (PTYPE(vp) != P_ATTENUATE || use_both) {
	    # Get intensity transformation function parameters & image minmax.
	    need_lims = false
	    if (IM_LIMTIME(im1) < IM_MTIME(im1))
		need_lims = true
	    else {
		tmpmin = IM_MIN(im1)
		tmpmax = IM_MAX(im1)
	    }
	    if (IS_INDEFR(VIMIN(vp))) {
		if (need_lims) {
		    call imminmax (im1, tmpmin, tmpmax)
		    need_lims = false
		    if (VERBOSE(vp) == YES) {
			call eprintf ("Must compute input image min & max...\n")
			call eprintf ("NOTE:  run MINMAX with force+ & update+")
			call eprintf (" on input image in the future.\n")
		    }
		}
		IIMIN(vp) = tmpmin
		VIMIN(vp) = IIMIN(vp)
	    } else {
		if (need_lims) {
		    IIMIN(vp) = VIMIN(vp)
		    if (VERBOSE(vp) == YES)
			call eprintf ("Image MIN not present; using IMIN\n")
		} else 
		    IIMIN(vp) = tmpmin
	    }

	    if (IS_INDEFR(VIMAX(vp))) {
		if (need_lims) {
		    call imminmax (im1, tmpmin, tmpmax)
		    if (VERBOSE(vp) == YES) {
			call eprintf ("Must compute input image min & max...\n")
			call eprintf ("NOTE:  run MINMAX with force+ & update+")
			call eprintf (" on input image in the future.\n")
		    }
		}
		IIMAX(vp) = tmpmax
		VIMAX(vp) = IIMAX(vp)
	    } else {
		if (need_lims) {
		    IIMAX(vp) = VIMAX(vp)
		    if (VERBOSE(vp) == YES)
			call eprintf ("Image MAX not present; using IMAX\n")
		} else
		    IIMAX(vp) = tmpmax
	    }

	    if (VIMAX(vp) - VIMIN(vp) <= 0.0 && PTYPE(vp) != P_ATTENUATE) {
		call eprintf ("Error:  Invalid imin / imax (%g : %g)\n")
		    call pargr (VIMIN(vp))
		    call pargr (VIMAX(vp))
		goto iwrapup_
	    }

	}

	# Load the relevant output header parameters.
	IM_PIXTYPE(im2) = TY_REAL
	IM_NDIM(im2) = 3
	IM_LEN(im2,COL) = IM_LEN(im1,COL)

	# Store run parameters in output image header.
	call imastr (im2, "V_OIMAGE", Memc[input])
	call imastr (im2, "V_OTITLE", IM_TITLE(im1))
	call imaddr (im2, "V_OLDMIN", IIMIN(vp))
	call imaddr (im2, "V_OLDMAX", IIMAX(vp))
	call imaddr (im2, "V_DEGREES", DEGREES(vp))
	call imaddr (im2, "V_THETA0", INIT_THETA(vp))
	call sprintf (Memc[tmpstr], SZ_LINE, "x=%5.2f, y=%5.2f, z=%5.2f")
	    call pargr (VECX(vp)); call pargr (VECY(vp)); call pargr (VECZ(vp)) 
	call imastr (im2, "V_ROTVECT", Memc[tmpstr])
	call imaddi (im2, "V_PTYPE", PTYPE(vp))
	call sprintf (Memc[tmpstr], SZ_LINE, "%g : %g")
	    call pargr (VIMIN(vp)); call pargr (VIMAX(vp))
	call imastr (im2, "V_IMINMX", Memc[tmpstr])
	call imaddr (im2, "V_IZERO", IZERO(vp))
	call sprintf (Memc[tmpstr], SZ_LINE, "%g : %g")
	    call pargr (OMIN(vp)); call pargr (OMAX(vp))
	call imastr (im2, "V_OMINMX", Memc[tmpstr])
	call imaddr (im2, "V_OSCALE", OSCALE(vp))
	call sprintf (Memc[tmpstr], SZ_LINE, "%g : %g")
	    call pargr (AMIN(vp)); call pargr (AMAX(vp))
	call imastr (im2, "V_ATTEN", Memc[tmpstr])
	if (PTYPE(vp) == P_INVDISPOW || PTYPE(vp) == P_MODN)
	    call imaddr (im2, "V_DISPOW", DISPOWER(vp))
	call imaddb (im2, "V_DISCUT", (DISCUTOFF(vp) == YES))
	if (PTYPE(vp) == P_MODN)
	    call imaddi (im2, "V_MODN", MODN(vp))
	if (use_both) {
	    call imastr (im2, "V_BOTH", "4D: Both opacity and intensity used")
	    call imaddi (im2, "V_OPELEM", OPACELEM(vp))
	}

	# Initialize timers.
	clock1 = clktime (long (0))
	call cnvtime (clock1, Memc[timestr], SZ_TIME)
	cpu1 = cputime (long (0))

	# Do all the work.
	call vproject (im1, im2, vp, use_both)

	call sysid (Memc[tmpstr], SZ_LINE)
	call imastr (im2, "P_SYSTEM", Memc[tmpstr])

	clock2 = clktime (long (0))
	elapclock = (clock2 - clock1)
	cpu2 = cputime (long (0))
	elapcpu = (cpu2 - cpu1) / 1000

	call imastr (im2, "P_STIME", Memc[timestr])
	clock1 = clktime (long (0))
	call cnvtime (clock1, Memc[timestr], SZ_TIME)
	call imastr (im2, "P_ETIME", Memc[timestr])
	call sprintf (Memc[tmpstr], SZ_LINE,
	    "Elapsed cpu = %02d %02s:%02s:%02s, clock = %02d %02s:%02s:%02s")
	    call pargi (elapcpu/86400)
	    call pargi (mod (elapcpu, 86400) / 3600)
	    call pargi (mod (elapcpu, 3600) / 60)
	    call pargi (mod (elapcpu, 60))
	    call pargi (elapclock/86400)
	    call pargi (mod (elapclock, 86400) / 3600)
	    call pargi (mod (elapclock, 3600) / 60)
	    call pargi (mod (elapclock, 60))
	call imastr (im2, "P_ELAPSED", Memc[tmpstr])

iwrapup_
	call imunmap (im1)
	call imunmap (im2)
mwrapup_	
	call mfree (vp, TY_STRUCT)
	call sfree (sp)
end
