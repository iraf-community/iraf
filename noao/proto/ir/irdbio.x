include "iralign.h"

# IR_DTRPARAMS --  Procedure to read in the parameters from the database file.

procedure ir_dtrparams (dt, image, ncols, nrows, nxsub, nysub, nxoverlap,
	nyoverlap, corner, order, raster, oval)

pointer	dt		# pointer to the database file
char	image[ARB]	# input image
int	ncols		# number of columns in the subraster
int	nrows		# number of rows in the subraster
int	nxsub		# number of subrasters in the x direction
int	nysub		# number of subrasters in the y direction
int	nxoverlap	# number of columns of overlap
int	nyoverlap	# number of rows of overlap
int	corner		# starting corner of mosaic
int	order		# row or column order for mosaic
int	raster		# raster scan
real	oval		# value of undefined output image pixels

int	recnum, nsubrasters
pointer	sp, str
int	dtlocate(), dtgeti(), strmatch()
real	dtgetr()

begin
	call smark (sp)
	call salloc (str, SZ_FNAME + 1, TY_CHAR)
	recnum = dtlocate (dt, image)

	ncols = dtgeti (dt, recnum, "ncols")
	nrows = dtgeti (dt, recnum, "nrows")
	nxsub = dtgeti (dt, recnum, "nxsub")
	nysub = dtgeti (dt, recnum, "nysub")
	nxoverlap = dtgeti (dt, recnum, "nxoverlap")
	nyoverlap = dtgeti (dt, recnum, "nyoverlap")

	call dtgstr (dt, recnum, "corner", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "ll") != 0)
	    corner = IR_LL
	else if (strmatch (Memc[str], "lr") != 0)
	    corner = IR_LR
	else if (strmatch (Memc[str], "ul") != 0)
	    corner = IR_UL
	else if (strmatch (Memc[str], "ur") != 0)
	    corner = IR_UR
	else
	    corner = IR_LL

	call dtgstr (dt, recnum, "order", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "column") != 0)
	    order = IR_COLUMN
	else if (strmatch (Memc[str], "row") != 0)
	    order = IR_ROW
	else
	    order = IR_ROW

	call dtgstr (dt, recnum, "raster", Memc[str], SZ_FNAME)
	if (strmatch (Memc[str], "yes") != 0)
	    raster = YES
	else if (strmatch (Memc[str], "no") != 0)
	    raster = NO
	else
	    raster = NO

	oval = dtgetr (dt, recnum, "oval")
	nsubrasters = dtgeti (dt, recnum, "nsubrasters")

	call sfree (sp)
end


# IR_DTWPARAMS -- Procedure to write out the parameters to the output file

procedure ir_dtwparams (dt, outimage, section, ncols, nrows, nxsub, nysub,
    nxoverlap, nyoverlap, corner, order, raster, oval)

pointer	dt		# pointer to the database file
char	outimage[ARB]	# name of the output image
char	section[ARB]	# input subraster section
int	ncols		# number of columns per subraster
int	nrows		# number of rows per subraster
int	nxsub		# number of subrasters in the x direction
int	nysub		# number of subrasters in the y direction
int	nxoverlap	# number of columns of overlap
int	nyoverlap	# number of rows of overlap
int	corner		# starting corner of mosaic
int	order		# mosaic in column or row order
int	raster		# raster scan pattern
real	oval		# undefined pixel value

bool	itob()

begin
	call dtptime (dt)
	call dtput (dt, "begin\t%s\n")
	    call pargstr (outimage)
	call dtput (dt, "\tsection\t\t%s\n")
	    call pargstr (section)
	call dtput (dt, "\tncols\t\t%d\n")
	    call pargi (ncols)
	call dtput (dt, "\tnrows\t\t%d\n")
	    call pargi (nrows)
	call dtput (dt, "\tnxsub\t\t%d\n")
	    call pargi (nxsub)
	call dtput (dt, "\tnysub\t\t%d\n")
	    call pargi (nysub)
	call dtput (dt, "\tnxoverlap\t%d\n")
	    call pargi (nxoverlap)
	call dtput (dt, "\tnyoverlap\t%d\n")
	    call pargi (nyoverlap)
	call dtput (dt, "\tcorner\t\t%s\n")
	switch (corner) {
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
	switch (order) {
	case IR_ROW:
	    call pargstr ("row")
	case IR_COLUMN:
	    call pargstr ("column")
	}
	call dtput (dt, "\traster\t\t%b\n")
	    call pargb (itob (raster))
	call dtput (dt, "\toval\t\t%g\n")
	    call pargr (oval)
end

