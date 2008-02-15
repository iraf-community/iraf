include <imset.h>
include <plset.h>

define	RG_REGIONS	"|circle|ellipse|box|rectangle|polygon|vector|columns|\
lines|pie|cannulus|eannulus|rannulus|pannulus|point|"

define	RG_CIRCLE	1
define	RG_ELLIPSE	2
define	RG_BOX		3
define	RG_RECTANGLE	4
define	RG_POLYGON	5
define	RG_VECTOR	6
define	RG_COLUMNS	7
define	RG_LINES	8
define	RG_PIE		9
define	RG_CANNULUS	10
define	RG_EANNULUS	11
define	RG_RANNULUS	12
define	RG_PANNULUS	13
define	RG_POINT	14

define	MAX_NVERTICES	100

# RG_SETREG -- Set the pixel mask region to the appropriate number.

procedure me_setreg (region, pmim, pregno, pregval, verbose)

char	region[ARB]		#I the region description
pointer	pmim			#I the pixelmask image descriptor
int	pregno			#I the current region number
int	pregval			#I the current region value
bool	verbose			#I print status messages ?

real	xc, yc, a, b, ratio, theta
real	x1, y1, x2, y2, width
pointer	sp, function, ufunction, pl, xver, yver, rangestr
int	nfuncs, nver, nold
int	strdic(), imstati(), nscan()

begin
	# Allocate working space. 
	call smark (sp)
	call salloc (function, SZ_FNAME, TY_CHAR)
	call salloc (ufunction, SZ_FNAME, TY_CHAR)
	call salloc (xver, MAX_NVERTICES, TY_REAL)
	call salloc (yver, MAX_NVERTICES, TY_REAL)
	call salloc (rangestr, SZ_FNAME, TY_CHAR)

	# Determine the type of region.
	call sscan (region)
	call gargwrd (Memc[function], SZ_FNAME)
	nfuncs = strdic (Memc[function], Memc[ufunction], SZ_FNAME, RG_REGIONS)
	if (nfuncs <= 0) {
	    if (verbose) {
		call printf ("    Region %d cannot be decoded\n")
		    call pargi (pregno)
	    }
	    call sfree (sp)
	    return
	}

	pl = imstati (pmim, IM_PLDES)

	switch (nfuncs) {

	case RG_CIRCLE:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    if (nscan() < 4) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_circle (pl, xc, yc, a, PIX_SRC+PIX_VALUE(pregval))
	    }

	case RG_ELLIPSE:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (ratio)
	    call gargr (theta)
	    if (nscan() < 6) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_ellipse (pl, xc, yc, a, ratio, theta,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_BOX:
	    call gargr (x1)
	    call gargr (y1)
	    call gargr (x2)
	    call gargr (y2)
	    if (nscan() < 5) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_box (pl, x1, y1, x2, y2, PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_RECTANGLE:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (ratio)
	    call gargr (theta)
	    if (nscan() < 6) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_rectangle (pl, xc, yc, a, ratio, theta,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_POLYGON:
	    nver = 0
	    repeat {
	        nold = nscan()
		call gargr (Memr[xver+nver])
		call gargr (Memr[yver+nver])
		if ((nscan() - nold) == 2)
		    nver = nver + 1
		else
		    break
	    } until ((nscan() - nold) < 2)
	    if (nver <3 ) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_polygon (pl, Memr[xver], Memr[yver], nver,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_VECTOR:
	    call gargr (x1)
	    call gargr (y1)
	    call gargr (x2)
	    call gargr (y2)
	    call gargr (width)
	    if (nscan() < 6) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_vector (pl, x1, y1, x2, y2, width,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_COLUMNS:
	    call gargwrd (Memc[rangestr], SZ_FNAME)
	    if (nscan() < 2) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_cols (pl, Memc[rangestr],
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_LINES:
	    call gargwrd (Memc[rangestr], SZ_FNAME)
	    if (nscan() < 2) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_lines (pl, Memc[rangestr],
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_PIE:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (b)
	    if (nscan() < 5) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_pie (pl, xc, yc, a, b, PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_CANNULUS:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (b)
	    if (nscan() < 5) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_cannulus (pl, xc, yc, a, b,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_EANNULUS:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (b)
	    call gargr (ratio)
	    call gargr (theta)
	    if (nscan() < 7) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_eannulus (pl, xc, yc, a, b, ratio, theta,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_RANNULUS:
	    call gargr (xc)
	    call gargr (yc)
	    call gargr (a)
	    call gargr (b)
	    call gargr (ratio)
	    call gargr (theta)
	    if (nscan() < 7) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_rannulus (pl, xc, yc, a, b, ratio, theta,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_PANNULUS:
	    call gargr (b)
	    if (nscan () < 2) {
	        nver = 0
	    } else {
	        nver = 0
	        repeat {
	            nold = nscan()
		    call gargr (Memr[xver+nver])
		    call gargr (Memr[yver+nver])
		    if ((nscan() - nold) == 2)
		        nver = nver + 1
		    else
			break
	        } until ((nscan() - nold) < 2)
	    }
	    if (nver < 3) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_apolygon (pl, b, Memr[xver], Memr[yver], nver,
		    PIX_SRC + PIX_VALUE(pregval))
	    }

	case RG_POINT:
	    call gargr (xc)
	    call gargr (yc)
	    if (nscan() < 3) {
		if (verbose) {
		    call printf ("    Region %d cannot be decoded\n")
		        call pargi (pregno)
		}
	    } else {
		call pe_point (pl, xc, yc, PIX_SRC+PIX_VALUE(pregval))
	    }

	default:
	    ;
	}

	call sfree (sp)
end
