# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gset.h>
include	<mach.h>
include	<imhdr.h>

# T_PCOL --  Plot an image column.

procedure t_pcol ()

real	zmin, zmax
pointer	image, section
pointer	im, sp, x_vec, y_vec
int	col, ncols, nlines, ndim
int	clgeti()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)

	# Open image and graphics stream.
	call clgstr ("image", Memc[image], SZ_FNAME)
	im = immap (Memc[image], READ_ONLY, 0)

	# Watch out for one dimensional images.
	ndim = IM_NDIM(im)
	if (ndim == 1)
	    call error (1, "One dimensional image - use prow")

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	call clputi ("col.p_maximum", ncols)
	col = clgeti ("col")
	if (col < 1 || col > ncols) {
	    call imunmap (im)
	    call error (2, "column index references outside image")
	}

	# Now get the requested column.
	call malloc (x_vec, nlines, TY_REAL)
	call malloc (y_vec, nlines, TY_REAL)
	call plt_gcols (im, col, col, Memr[x_vec], Memr[y_vec], zmin, zmax) 
 
	# Draw the requested column to the screen.
	call pc_draw_vector (Memc[image], Memr[x_vec], Memr[y_vec], nlines,
	    zmin, zmax, col, col, false)
       
        # Free resources.
	call mfree (x_vec, TY_REAL)
	call mfree (y_vec, TY_REAL)

	call imunmap (im)
	call sfree (sp)
end
