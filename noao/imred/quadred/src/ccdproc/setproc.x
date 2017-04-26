include	<imhdr.h>
include	"ccdred.h"

# SET_PROC -- Set the processing parameter structure pointer.

procedure set_proc (in, out, ccd)

pointer	in			# Input IMIO pointer
pointer	out			# Output IMIO pointer
pointer	ccd			# CCD structure (returned)

int	clgwrd(), clscan(), nscan()
real	clgetr()
pointer	sp, str

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Allocate the ccd structure.
	call calloc (ccd, LEN_CCD, TY_STRUCT)

	IN_IM(ccd) = in
	OUT_IM(ccd) = out
	COR(ccd) = NO
	CORS(ccd, FIXPIX) = NO
	CORS(ccd, OVERSCAN) = NO
	CORS(ccd, TRIM) = NO
	READAXIS(ccd) = clgwrd ("readaxis",Memc[str],SZ_LINE,"|line|columns|")
	MINREPLACE(ccd) = clgetr ("minreplace")

	CALCTYPE(ccd) = TY_REAL
	if (clscan ("pixeltype") != EOF) {
	    call gargwrd (Memc[str], SZ_LINE)
	    call gargwrd (Memc[str], SZ_LINE)
	    if (nscan() == 2) {
	        if (Memc[str] == 'r')
		    CALCTYPE(ccd) = TY_REAL
		else if (Memc[str] == 's')
		    CALCTYPE(ccd) = TY_SHORT
		else
		    call error (1, "Invalid calculation datatype")
	    }
	}

	call sfree (sp)
end


# FREE_PROC -- Free the processing structure pointer.

procedure free_proc (ccd)

pointer	ccd			# CCD structure

begin
	# Unmap calibration images.
	if (ZERO_IM(ccd) != NULL)
	    call ccd_unmap (ZERO_IM(ccd))
	if (DARK_IM(ccd) != NULL)
	    call ccd_unmap (DARK_IM(ccd))
	if (FLAT_IM(ccd) != NULL)
	    call ccd_unmap (FLAT_IM(ccd))
	if (ILLUM_IM(ccd) != NULL)
	    call ccd_unmap (ILLUM_IM(ccd))
	if (FRINGE_IM(ccd) != NULL)
	    call ccd_unmap (FRINGE_IM(ccd))

	# Free memory
	if (BADCOLS(ccd) != NULL)
	    call mfree (BADCOLS(ccd), TY_SHORT)
	if (BADLINES(ccd) != NULL)
	    call mfree (BADLINES(ccd), TY_SHORT)
	if (OVERSCAN_VEC(ccd) != NULL)
	    call mfree (OVERSCAN_VEC(ccd), TY_REAL)
	call mfree (IN_SEC(ccd), TY_INT)
	call mfree (OUT_SEC(ccd), TY_INT)
	call mfree (BIAS_SEC(ccd), TY_INT)
	call mfree (ccd, TY_STRUCT)
end
