include <imhdr.h>
include <fset.h>
include "iralign.h"

# T_IRALIGN -- Align the individual subraster elements in the input image.
# In order to run this program the user should have created the output image
# and the database file with the IRMOSAIC task. In addition the user should
# supply a coordinate list consisting of pairs of coordinates of identical
# objects or features in two adjacent subrasters.

procedure t_iralign ()

int	cl, nimages, interp, align, verbose
pointer	ir, sp, inimage, outimage, database, coords, trimlimits, str
pointer	im, outim, dt

bool	clgetb()
int	open(), clgwrd(), btoi()
int	ir_links(), ir_clinks(), ir_flinks()
pointer	immap(), dtmap()
real	clgetr()

begin
	# Allocate sapce for the ir strucuture.
	call ir_init (ir)

	# Set the standard output to flush on a new line.
	call fseti (STDOUT, F_FLUSHNL, YES)

	# Allocate temporary working space.
	call smark (sp)
	call salloc (inimage, SZ_FNAME, TY_CHAR)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (coords, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (trimlimits, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Get the input and output images and the coordinate list.
	call clgstr ("input", Memc[inimage], SZ_FNAME)
	call clgstr ("output", Memc[outimage], SZ_FNAME)
	call clgstr ("database", Memc[database], SZ_FNAME)
	call clgstr ("coords", Memc[coords], SZ_FNAME)
	align = clgwrd ("alignment", Memc[str], SZ_LINE, ",coords,shifts,file,")
	call clgstr ("trimlimits", Memc[trimlimits], SZ_FNAME)

	# Open the images and files.
	im = immap (Memc[inimage], READ_ONLY, 0)
	outim = immap (Memc[outimage], NEW_COPY, im)
	dt = dtmap (Memc[database], READ_ONLY)

	# Get the data base parameters.
	call ir_dtrparams (dt, Memc[inimage], ir)

	# Get the rest of the parameters.
	call ir_params (ir, im, outim)
	interp = clgwrd ("interpolant", Memc[str], SZ_LINE,
	    ",nearest,linear,poly3,poly5,spline3,")
	verbose = btoi (clgetb ("verbose"))

	# Allocate array space.
	nimages = IR_NXSUB(ir) * IR_NYSUB(ir)
	call ir_arrays (ir, nimages)

	# Compute the shifts for each subraster.
	switch (align) {
	case IR_COORDS:
	    cl = open (Memc[coords], READ_ONLY, TEXT_FILE)
	    if (ir_links (cl, Memr[IR_XRSHIFTS(ir)], Memr[IR_YRSHIFTS(ir)],
	        Memr[IR_XCSHIFTS(ir)], Memr[IR_YCSHIFTS(ir)],
		Memi[IR_NRSHIFTS(ir)], Memi[IR_NCSHIFTS(ir)],
		IR_NCOLS(ir), IR_NROWS(ir), IR_NXRSUB(ir), IR_NYRSUB(ir),
		IR_NXSUB(ir), IR_NYSUB(ir), IR_NXOVERLAP(ir), IR_NYOVERLAP(ir),
		IR_ORDER(ir)) > 0) {
	        call ir_shifts (ir, im, outim, Memr[IR_XRSHIFTS(ir)],
		    Memr[IR_YRSHIFTS(ir)], Memr[IR_XCSHIFTS(ir)],
		    Memr[IR_YCSHIFTS(ir)], Memi[IR_IC1(ir)],
		    Memi[IR_IC2(ir)], Memi[IR_IL1(ir)], Memi[IR_IL2(ir)],
		    Memi[IR_OC1(ir)], Memi[IR_OC2(ir)], Memi[IR_OL1(ir)],
		    Memi[IR_OL2(ir)], Memr[IR_DELTAX(ir)], Memr[IR_DELTAY(ir)])
	    } else
	        call error (0, "There are no legal shifts in the coords file.")
	    call close (cl)

	case IR_SHIFTS:
	    if (ir_clinks (Memr[IR_XRSHIFTS(ir)], Memr[IR_YRSHIFTS(ir)],
	        Memr[IR_XCSHIFTS(ir)], Memr[IR_YCSHIFTS(Ir)], IR_NXRSUB(ir),
		IR_NYRSUB(ir), IR_NXSUB(ir), IR_NYSUB(ir), clgetr ("xshift"),
		clgetr ("yshift")) > 0) {
	        call ir_shifts (ir, im, outim, Memr[IR_XRSHIFTS(ir)],
		    Memr[IR_YRSHIFTS(ir)], Memr[IR_XCSHIFTS(ir)],
		    Memr[IR_YCSHIFTS(ir)], Memi[IR_IC1(ir)],
		    Memi[IR_IC2(ir)], Memi[IR_IL1(ir)], Memi[IR_IL2(ir)],
		    Memi[IR_OC1(ir)], Memi[IR_OC2(ir)], Memi[IR_OL1(ir)],
		    Memi[IR_OL2(ir)], Memr[IR_DELTAX(ir)], Memr[IR_DELTAY(ir)])
	    } else
	        call error (0, "There are no legal shifts in the coords file.")

	case IR_FILE:

	    cl = open (Memc[coords], READ_ONLY, TEXT_FILE)
	    if (ir_flinks (cl, Memr[IR_DELTAX(ir)], Memr[IR_DELTAY(ir)],
	        Memr[IR_DELTAI(ir)], nimages) >= nimages) {
	        call ir_fshifts (ir, im, outim, Memr[IR_DELTAX(ir)],
		    Memr[IR_DELTAY(ir)], Memi[IR_IC1(ir)], Memi[IR_IC2(ir)],
		    Memi[IR_IL1(ir)], Memi[IR_IL2(ir)], Memi[IR_OC1(ir)],
		    Memi[IR_OC2(ir)], Memi[IR_OL1(ir)], Memi[IR_OL2(ir)])
	    } else
	        call error (0, "There are fewer shifts than subraster.")
	    call close (cl)

	default:
	    call error (0, "T_IRALIGN: Undefined alignment algorithm")
	}

	# Fill the output image with the undefined value.
	call ir_imzero (outim, int (IM_LEN(outim,1)), int (IM_LEN(outim,2)),
	    IR_OVAL(ir))

	# Shift all the subrasters.
	call amovkr (0.0, Memr[IR_DELTAI(ir)], nimages)
	call ir_subalign (ir, im, outim, Memc[trimlimits], Memi[IR_IC1(ir)],
	    Memi[IR_IC2(ir)], Memi[IR_IL1(ir)], Memi[IR_IL2(ir)],
	    Memi[IR_OC1(ir)], Memi[IR_OC2(ir)], Memi[IR_OL1(ir)],
	    Memi[IR_OL2(ir)], Memr[IR_DELTAX(ir)], Memr[IR_DELTAY(ir)], 
	    Memr[IR_DELTAI(ir)], NO, interp, verbose)

	# Close up files
	call imunmap (im)
	call imunmap (outim)
	call dtunmap (dt)
	call sfree (sp)
	call ir_free (ir)
end
