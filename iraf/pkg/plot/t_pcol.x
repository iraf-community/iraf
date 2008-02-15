# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>
include	<mwset.h>

# T_PCOL --  Plot an image column.

procedure t_pcol ()

pointer	image, wcslab, fmt
pointer	im, mw, ct, sp, x_vec, y_vec
int	col, ncols, nlines
real	zmin, zmax
int	clgeti()
pointer	immap(), mw_openim(), mw_sctran()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (wcslab, SZ_LINE, TY_CHAR)
	call salloc (fmt, SZ_LINE, TY_CHAR)

	# Open image
	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)
        call clgstr ("wcs", Memc[wcslab], SZ_LINE)
        mw = mw_openim (im)
        call mw_seti (mw, MW_USEAXMAP, NO)
        ct = mw_sctran (mw, "logical", Memc[wcslab], 0)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	col = clgeti ("col")
	if (col < 1 || col > ncols) {
	    call imunmap (im)
	    call error (2, "column index references outside image")
	}

	# Now get the requested column.
	call malloc (x_vec, nlines, TY_REAL)
	call malloc (y_vec, nlines, TY_REAL)
	call plt_gcols (im, mw, ct, col, col, Memr[x_vec], Memr[y_vec],
	    zmin, zmax, Memc[wcslab], Memc[fmt], SZ_LINE) 
 
	# Draw the requested column to the screen.
	call pc_draw_vector (Memc[image], Memr[x_vec], Memr[y_vec], nlines,
	    zmin, zmax, col, col, Memc[wcslab], Memc[fmt], false)
       
        # Free resources.
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)

	call imunmap (im)
	call sfree (sp)
end
