include <imhdr.h>
include <fset.h>
include <pkg/dttext.h>
include "iralign.h"


# T_IRMATCHD1 -- Align the individual subraster elements in the input image.
# In order to run this program the user should have created the output image
# and the database file with the IRMOSAIC task. In addition the user should
# supply a coordinate list consisting of pairs of coordinates of identical
# objects or features in two adjacent subrasters.

procedure t_irmatchd1 ()

int	cl, nxrsub, nyrsub, ncols, nrows, nxsub, nysub, nxoverlap, nyoverlap
int	corner, order, raster, nimlines, nimcols, interp, align, verbose
int	nshifts, nmatch
pointer	sp, inimage, outimage, database, coords, section, matchlist, ranges
pointer	str, im, outim, dt
pointer	nrshifts, ncshifts, xrshifts, yrshifts, xcshifts, ycshifts
real	xshift, yshift, rval, oval

bool	clgetb()
int	open(), clgeti(), clgwrd(), btoi(), ir_shifts(), decode_ranges()
pointer	immap(), dtmap()
real	clgetr()

begin
	# Set the standard output to flush on a new line.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary working space.
	call smark (sp)
	call salloc (inimage, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (matchlist, SZ_LINE, TY_CHAR)
	call salloc (ranges, 3 * MAX_NRANGES + 1, TY_INT)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the input and output images and the coordinate list.
	call clgstr ("input", Memc[inimage], SZ_FNAME)
	call clgstr ("output", Memc[outimage], SZ_FNAME)
	call clgstr ("database", Memc[database], SZ_FNAME)
	call clgstr ("refsection", Memc[section], SZ_FNAME)
	align = clgwrd ("alignment", Memc[str], SZ_LINE, ",coords,shifts,")
	call clgstr ("match", Memc[matchlist], SZ_LINE)

	# Open the images and files.
	im = immap (Memc[inimage], READ_ONLY, 0)
	outim = immap (Memc[outimage], NEW_COPY, im)
	dt = dtmap (Memc[database], READ_ONLY)

	# Get the data base parameters.
	call ir_dtrparams (dt, Memc[inimage], ncols, nrows, nxsub, nysub,
	    nxoverlap, nyoverlap, corner, order, raster, oval)
	nxrsub = clgeti ("nxrsub")
	if (IS_INDEFI(nxrsub) || nxrsub < 1 || nxrsub > nxsub)
	    nxrsub = (nxsub + 1) / 2
	nyrsub = clgeti ("nyrsub")
	if (IS_INDEFI(nyrsub) || nyrsub < 1 || nyrsub > nysub)
	    nyrsub = (nysub + 1) / 2
	nimcols = clgeti ("nimcols")
	if (! IS_INDEFI(nimcols) && nimcols > 0 && nimcols >= IM_LEN(im,1))
	    IM_LEN(outim,1) = nimcols
	nimlines = clgeti ("nimlines")
	if (! IS_INDEFI(nimlines) && nimlines > 0 && nimlines >= IM_LEN(im,2))
	    IM_LEN(outim,2) = nimlines
	interp = clgwrd ("interpolant", Memc[str], SZ_LINE,
	    ",nearest,linear,poly3,poly5,spline3,")
	rval = clgetr ("oval")
	if (! IS_INDEFR(rval))
	    oval = rval
	verbose = btoi (clgetb ("verbose"))

	# Decode the list of input images to be intensity matched.
	if (Memc[matchlist] == EOS) {
	    Memi[ranges] = NULL
	} else if (Memc[matchlist] == '*') {
	    Memi[ranges] = 1
	    Memi[ranges+1] = nxsub * nysub
	    Memi[ranges+2] = 1
	    Memi[ranges+3] = NULL
	} else if (decode_ranges (Memc[matchlist], Memi[ranges], MAX_NRANGES,
	    nmatch) == ERR) {
		call error (0,
		"Cannot decode list of rasters to be intensity matched.")

	}

	# Allocate temporary space.
	call salloc (xrshifts, nxsub * nysub, TY_REAL)
	call salloc (yrshifts, nxsub * nysub, TY_REAL)
	call salloc (xcshifts, nxsub * nysub, TY_REAL)
	call salloc (ycshifts, nxsub * nysub, TY_REAL)
	call salloc (nrshifts, nxsub * nysub, TY_INT)
	call salloc (ncshifts, nxsub * nysub, TY_INT)

	# Compute the shifts for each subraster.
	switch (align) {
	case IR_COORDS:
	    call clgstr ("coords", Memc[coords], SZ_FNAME)
	    cl = open (Memc[coords], READ_ONLY, TEXT_FILE)
	    nshifts = ir_shifts (cl, Memr[xrshifts], Memr[yrshifts],
	        Memr[xcshifts], Memr[ycshifts], Memi[nrshifts], Memi[ncshifts],
		ncols, nrows, nxrsub, nyrsub, nxsub, nysub, nxoverlap,
		nyoverlap, order)
	    call close (cl)
	case IR_SHIFTS:
	    xshift = clgetr ("xshift")
	    yshift = clgetr ("yshift")
	    nshifts = 1
	    call ir_cshifts (Memr[xrshifts], Memr[yrshifts], Memr[xcshifts],
	        Memr[ycshifts], nxrsub, nyrsub, nxsub, nysub, xshift, yshift)
	default:
	    call error (0, "T_IRALIGN: Undefined alignment algorithm")
	}

	# Shift all the subrasters.
	if (nshifts > 0) {
	    call ir_m1subalign (im, outim, Memc[section], dt, Memi[ranges],
	        Memr[xrshifts], Memr[yrshifts], Memr[xcshifts], Memr[ycshifts],
		nxsub, nysub, nxrsub, nyrsub, ncols, nrows, nxoverlap,
		nyoverlap, order, raster, oval, interp, verbose)
	} else {
	    call error (0, "There are no legal shifts in the coords file.")
	}

	# Close up files
	call imunmap (im)
	call imunmap (outim)
	call dtunmap (dt)
	call sfree (sp)
end

