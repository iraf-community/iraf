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
char	units[ARB]
pointer	sp, imsect, im, mw, sh, gt

int	i, n, sec[3,3], clgeti()
pointer	immap(), smw_openim()
errchk	immap, shdr_open, shdr_system, un_changer

begin
	call smark (sp)
	call salloc (imsect, SZ_FNAME, TY_CHAR)

	# Map the image if necessary.  Don't allow image sections but
	# determine requested spectrum from any explicit specification.

	if (im == NULL) {
	    call imgsection (image, Memc[imsect], SZ_FNAME)
	    call imgimage (image, image, SZ_FNAME)
	    im = immap (image, READ_ONLY, 0)
	    mw = smw_openim (im)
	    n = IM_NDIM(im)
	    if (Memc[imsect] != EOS && n > 1) {
		call amovki (1, sec[1,1], n)
		call amovki (IM_LEN(im,1), sec[1,2], n)
		call amovki (1, sec[1,3], n)
		call id_section (Memc[imsect], sec[1,1], sec[1,2], sec[1,3], n)
		switch (SMW_FORMAT(mw)) {
		case SMW_ND:
		    i = 0
		    if (n == 2) {
			if (abs (sec[1,2]-sec[1,1]) == 0) {
			    nline = sec[1,1]
			    i = 2
			} else if (abs (sec[2,2]-sec[2,1]) == 0) {
			    nline = sec[2,1]
			    i = 1
			}
		    } else {
			if (abs (sec[1,2]-sec[1,1]) == 0) {
			    nline = sec[1,1]
			    if (abs (sec[2,2]-sec[2,1]) == 0) {
				nband = sec[2,1]
			        if (abs (sec[3,2]-sec[3,1]) > 0)
				    i = 3
			    } else if (abs (sec[3,2]-sec[3,1]) == 0) {
				nband = sec[3,1]
				i = 2
			    }
			} else if (abs (sec[2,2]-sec[2,1]) == 0) {
			    nline = sec[2,1]
			    if (abs (sec[3,2]-sec[3,1]) == 0) {
				nband = sec[3,1]
				i = 1
			    }
			}
		    }
		    if (i > 0) {
			call smw_daxis (mw, im, i, INDEFI, INDEFI)
			call smw_saxis (mw, NULL, im)
		    }
		default:
		    if (abs (sec[2,2]-sec[2,1]) == 0)
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

	if (DC(sh) == DCNO && !IS_INDEFD(w0))
	    call usercoord (sh, 'l', 1D0, w0, 2D0, w0+wpc)

	# Cancel wavelength coordinates if not desired or set units.
	if (!wave_scl)
	    call shdr_system (sh, "physical")
	else {
	    iferr (call shdr_units (sh, units))
		;
	}

	# Make a title.
	call mktitle (sh, gt)

	call sfree (sp)
end
