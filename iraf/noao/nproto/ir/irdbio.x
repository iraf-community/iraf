include "iralign.h"

# IR_DTRPARAMS --  Procedure to read in the parameters from the database file.

procedure ir_dtrparams (dt, image, ir)

pointer	dt		# pointer to the database file
char	image[ARB]	# input image
pointer	ir		# pointer to the ir structure

int	recnum, nsubrasters
pointer	sp, str
int	dtlocate(), dtgeti(), strmatch()
real	dtgetr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	recnum = dtlocate (dt, image)

	IR_NCOLS(ir) = dtgeti (dt, recnum, "ncols")
	IR_NROWS(ir) = dtgeti (dt, recnum, "nrows")
	IR_NXSUB(ir) = dtgeti (dt, recnum, "nxsub")
	IR_NYSUB(ir) = dtgeti (dt, recnum, "nysub")
	IR_NXOVERLAP(ir) = dtgeti (dt, recnum, "nxoverlap")
	IR_NYOVERLAP(ir) = dtgeti (dt, recnum, "nyoverlap")

	call dtgstr (dt, recnum, "corner", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "ll") != 0)
	    IR_CORNER(ir) = IR_LL
	else if (strmatch (Memc[str], "lr") != 0)
	    IR_CORNER(ir) = IR_LR
	else if (strmatch (Memc[str], "ul") != 0)
	    IR_CORNER(ir) = IR_UL
	else if (strmatch (Memc[str], "ur") != 0)
	    IR_CORNER(ir) = IR_UR
	else
	    IR_CORNER(ir) = IR_LL

	call dtgstr (dt, recnum, "order", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "column") != 0)
	    IR_ORDER(ir) = IR_COLUMN
	else if (strmatch (Memc[str], "row") != 0)
	    IR_ORDER(ir) = IR_ROW
	else
	    IR_ORDER(ir) = IR_ROW

	call dtgstr (dt, recnum, "raster", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "yes") != 0)
	    IR_RASTER(ir) = YES
	else if (strmatch (Memc[str], "no") != 0)
	    IR_RASTER(ir) = NO
	else
	    IR_RASTER(ir) = NO

	IR_OVAL(ir) = dtgetr (dt, recnum, "oval")
	nsubrasters = dtgeti (dt, recnum, "nsubrasters")

	call sfree (sp)
end


# IR_DTWPARAMS -- Procedure to write out the parameters to the output file

procedure ir_dtwparams (dt, outimage, trimsection, medsection, ir)

pointer	dt		# pointer to the database file
char	outimage[ARB]	# name of the output image
char	trimsection[ARB]# input subraster section
char	medsection[ARB]	# section for computing the median
pointer	ir		# pointer to the ir structure

bool	itob()

begin
	call dtptime (dt)
	call dtput (dt, "begin\t%s\n")
	    call pargstr (outimage)
	call dtput (dt, "\ttrimsection\t%s\n")
	    call pargstr (trimsection)
	call dtput (dt, "\tmedsection\t\t%s\n")
	    call pargstr (medsection)
	call dtput (dt, "\tncols\t\t%d\n")
	    call pargi (IR_NCOLS(ir))
	call dtput (dt, "\tnrows\t\t%d\n")
	    call pargi (IR_NROWS(ir))
	call dtput (dt, "\tnxsub\t\t%d\n")
	    call pargi (IR_NXSUB(ir))
	call dtput (dt, "\tnysub\t\t%d\n")
	    call pargi (IR_NYSUB(ir))
	call dtput (dt, "\tnxoverlap\t%d\n")
	    call pargi (IR_NXOVERLAP(ir))
	call dtput (dt, "\tnyoverlap\t%d\n")
	    call pargi (IR_NYOVERLAP(ir))
	call dtput (dt, "\tcorner\t\t%s\n")
	switch (IR_CORNER(ir)) {
	case IR_LL:
	    call pargstr ("ll")
	case IR_LR:
	    call pargstr ("lr")
	case IR_UL:
	    call pargstr ("ul")
	case IR_UR:
	    call pargstr ("ur")
	}
	call dtput (dt, "\torder\t\t%s\n")
	switch (IR_ORDER(ir)) {
	case IR_ROW:
	    call pargstr ("row")
	case IR_COLUMN:
	    call pargstr ("column")
	}
	call dtput (dt, "\traster\t\t%b\n")
	    call pargb (itob (IR_RASTER(ir)))
	call dtput (dt, "\toval\t\t%g\n")
	    call pargr (IR_OVAL(ir))
end
