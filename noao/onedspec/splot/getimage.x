include	<error.h>
include	<imhdr.h>
include	<pkg/gtools.h>
include <smw.h>

# GETIMAGE -- Read new image pixels.

procedure getimage (image, nline, nband, nap, wave_scl, w0, wpc, units,
	im, mw, sh, gt)

char	image[ARB]
int	nline, nband, nap
bool	wave_scl
double	w0, wpc
real	a, b
char	units[ARB]
pointer	sp, imsect, im, mw, sh, gt

int	da, n, sec[3,3], clgeti()
real	gt_getr()
double	shdr_lw()
pointer	immap(), smw_openim()
errchk	immap, shdr_open, shdr_system, un_changer

begin
	call smark (sp)
	call salloc (imsect, SZ_FNAME, TY_CHAR)

	# Map the image if necessary.  Don't allow image sections but
	# determine requested spectrum from any explicit specification.

	da = 0
	if (im == NULL) {
	    call imgsection (image, Memc[imsect], SZ_FNAME)
	    call imgimage (image, image, SZ_FNAME)
	    im = immap (image, READ_ONLY, 0)
	    mw = smw_openim (im)
	    n = IM_NDIM(im)
	    if (Memc[imsect] != EOS) {
		call amovki (1, sec[1,1], n)
		call amovi (IM_LEN(im,1), sec[1,2], n)
		call amovki (1, sec[1,3], n)
		call id_section (Memc[imsect], sec[1,1], sec[1,2], sec[1,3], n)
		switch (SMW_FORMAT(mw)) {
		case SMW_ND:
		    if (n == 1)
			da = 1
		    if (n == 2) {
			if (abs (sec[1,2]-sec[1,1]) == 0) {
			    nline = sec[1,1]
			    da = 2
			} else if (abs (sec[2,2]-sec[2,1]) == 0) {
			    nline = sec[2,1]
			    da = 1
			}
		    } else {
			if (abs (sec[1,2]-sec[1,1]) == 0) {
			    nline = sec[1,1]
			    if (abs (sec[2,2]-sec[2,1]) == 0) {
				nband = sec[2,1]
			        if (abs (sec[3,2]-sec[3,1]) > 0)
				    da = 3
			    } else if (abs (sec[3,2]-sec[3,1]) == 0) {
				nband = sec[3,1]
				da = 2
			    }
			} else if (abs (sec[2,2]-sec[2,1]) == 0) {
			    nline = sec[2,1]
			    if (abs (sec[3,2]-sec[3,1]) == 0) {
				nband = sec[3,1]
				da = 1
			    }
			}
		    }
		    if (da > 0) {
			call smw_daxis (mw, im, da, INDEFI, INDEFI)
			call smw_saxis (mw, NULL, im)
		    }
		default:
		    da = 1
		    if (n > 1 && abs (sec[2,2]-sec[2,1]) == 0)
			nline = sec[2,1]
		    if (n > 2 && abs (sec[3,2]-sec[3,1]) == 0)
			nband = sec[3,1]
		}
	    }
	}

	# Get header info.
	switch (SMW_FORMAT(mw)) {
	case SMW_ND:
	    nap = INDEFI
	    n = SMW_LLEN(mw,2)
	    if (n > 1) {
		if (nline == 0)
	            nline = max (1, min (n, clgeti ("line")))
	    } else
		nline = 0
	    n = SMW_LLEN(mw,3)
	    if (n > 1) {
		if (nband == 0)
	            nband = max (1, min (n, clgeti ("band")))
	    } else
		nband = 0
	default:
	    n = SMW_NSPEC(mw)
	    if (n > 1) {
		if (nline == 0) { 
	            nline = clgeti ("line")
		    nap = nline
		}
	    } else {
		nline = 0
		nap = INDEFI
	    }
	    n = SMW_NBANDS(mw)
	    if (n > 1) {
		if (nband == 0) 
		    nband = max (1, min (n, clgeti ("band")))
	    } else
		nband = 0
	}

	call shdr_open (im, mw, nline, nband, nap, SHDATA, sh)
	nap = AP(sh)
	nline = LINDEX(sh,1)

	if (DC(sh) == DCNO && !IS_INDEFD(w0))
	    call usercoord (sh, 'l', 1D0, w0, 2D0, w0+wpc)

	# Cancel wavelength coordinates if not desired or set units.
	if (!wave_scl)
	    call shdr_system (sh, "physical")
	else {
	    iferr (call shdr_units (sh, units))
		;
	}

	if (da > 0) {
	    a =  gt_getr (gt, GTXMIN)
	    b =  gt_getr (gt, GTXMAX)
	    if (IS_INDEF(a) && IS_INDEF(b)) {
		if (!wave_scl) {
		    call gt_setr (gt, GTXMIN, real(sec[da,1]))
		    call gt_setr (gt, GTXMAX, real(sec[da,2]))
		} else {
		    a = shdr_lw (sh, double(sec[da,1]))
		    b = shdr_lw (sh, double(sec[da,2]))
		    call gt_setr (gt, GTXMIN, a)
		    call gt_setr (gt, GTXMAX, b)
		}
	    }
	}

	# Make a title.
	call mktitle (sh, gt)

	call sfree (sp)
end
