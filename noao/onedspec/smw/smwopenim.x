include <imhdr.h>
include	<imio.h>
include	<mwset.h>

define	SYSTEMS	"|equispec|multispec|physical|image|world|linear|"


# SMW_OPENIM -- Open the spectral MWCS for various input formats.

pointer procedure smw_openim (im)

pointer	im		#I Image pointer
pointer	mw		#O MWCS pointer

pointer	sp, system, mw_openim()
bool	streq()
int	i, wcsdim, sys, strdic(), mw_stati()
errchk	mw_openim, smw_oldms, smw_linear

begin
	call smark (sp)
	call salloc (system, SZ_FNAME, TY_CHAR)

	# Workaround for truncation of header during image header copy.
	IM_HDRLEN(im) = IM_LENHDRMEM(im)

	# Force higher dimensions to length 1.
	do i = IM_NDIM(im) + 1, 3
	    IM_LEN(im,i) = 1

	mw = mw_openim (im)
	call mw_seti (mw, MW_USEAXMAP, NO)
	wcsdim = mw_stati (mw, MW_NDIM)
	call mw_gwattrs (mw, 0, "system", Memc[system], SZ_FNAME)
	sys = strdic (Memc[system], Memc[system], SZ_FNAME, SYSTEMS)

	# Set various input systems.
	switch (sys) {
	case 1:
	    call smw_equispec (im, mw)
	case 2:
	    call smw_multispec (im, mw)
	default:
	    if (sys == 0) {
	        call eprintf (
	    "WARNING: Unknown coordinate system `%s' - assuming `linear'.\n")
		    call pargstr (Memc[system])
	    } else if (sys == 3)
		call mw_newsystem (mw, "image", wcsdim)

	    # Old "multispec" format.
	    ifnoerr (call imgstr (im, "APFORMAT", Memc[system], SZ_FNAME)) {
		if (streq (Memc[system], "onedspec"))
		    call smw_onedspec (im, mw)
		else
		    call smw_oldms (im, mw)

	    # Old "onedspec" format or other 1D image.
	    } else if (wcsdim == 1) {
		call smw_onedspec (im, mw)

	    # N-dimensional image.
	    } else
		call smw_nd (im, mw)
	}

	call sfree (sp)
	return (mw)
end
