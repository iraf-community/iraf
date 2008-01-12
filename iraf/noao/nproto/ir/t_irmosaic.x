include <imhdr.h>
include <fset.h>
include "iralign.h"


# T_IRMOSAIC -- Procedure to combine a list of subrasters into a single large
# image.

procedure t_irmosaic ()

int	nimages, nmissing, verbose, subtract
pointer	ir, sp, outimage, database, trimsection, medsection, nullinput, ranges
pointer	str, index, c1, c2, l1, l2, isnull, median, imlist, outim, dt

bool	clgetb()
char	clgetc()
int	btoi(), clgwrd(), imtlen(), clgeti(), decode_ranges(), ir_get_imtype()
pointer	imtopenp(), ir_setim(), dtmap()
real	clgetr()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)
	call malloc (ir, LEN_IRSTRUCT, TY_STRUCT)

	# Allocate temporary working space.
	call smark (sp)
	call salloc (outimage, SZ_FNAME, TY_CHAR)
	call salloc (database, SZ_FNAME, TY_CHAR)
	call salloc (trimsection, SZ_FNAME, TY_CHAR)
	call salloc (medsection, SZ_FNAME, TY_CHAR)
	call salloc (nullinput, SZ_FNAME, TY_CHAR)
	call salloc (ranges, 3 * MAX_NRANGES + 1, TY_INT)
	call salloc (str, SZ_FNAME, TY_CHAR)

	# Get the image list, output image name and database file name.
	imlist = imtopenp ("input")
	call clgstr ("output", Memc[outimage], SZ_FNAME)
	call clgstr ("database", Memc[database], SZ_FNAME)
	call clgstr ("trim_section", Memc[trimsection], SZ_FNAME)
	call clgstr ("null_input", Memc[nullinput], SZ_FNAME)
	call clgstr ("median_section", Memc[medsection], SZ_FNAME)
	if (Memc[medsection] == EOS)
	    subtract = NO
	else
	    subtract = btoi (clgetb ("subtract"))
	verbose = btoi (clgetb ("verbose"))

	# Get the mosaicing parameters.
	IR_NXSUB(ir) = clgeti ("nxsub")
	IR_NYSUB(ir) = clgeti ("nysub")
	IR_CORNER(ir) = clgwrd ("corner", Memc[str], SZ_FNAME, ",ll,lr,ul,ur,")
	IR_ORDER(ir) = clgwrd ("direction", Memc[str], SZ_FNAME, ",row,column,")
	IR_RASTER(ir) = btoi (clgetb ("raster"))
	IR_NXOVERLAP(ir) = clgeti ("nxoverlap")
	IR_NYOVERLAP(ir) = clgeti ("nyoverlap")
	IR_OVAL(ir) = clgetr ("oval")

	# Check that the number of observed and missing images matches
	# the number of specified subrasters.
	if (Memc[nullinput] == EOS) {
	    nmissing = 0
	    Memi[ranges] = 0
	    Memi[ranges+1] = 0
	    Memi[ranges+2] = 1
	    Memi[ranges+3] = NULL
	} else {
	    if (decode_ranges (Memc[nullinput], Memi[ranges], MAX_NRANGES,
	        nmissing) == ERR)
	        call error (0, "Error decoding list of unobserved rasters.")
	}
	nimages = imtlen (imlist) + nmissing
	if (nimages != (IR_NXSUB(ir) * IR_NYSUB(ir)))
	    call error (0,
	        "The number of input images is not equal to nxsub * nysub.")

	# Compute the output image characteristics and open the output image.
	outim = ir_setim (ir, imlist, Memc[trimsection], Memc[outimage],
	    clgeti ("nimcols"), clgeti ("nimrows"), ir_get_imtype (clgetc (
	    "opixtype")))

	# Open the database file.
	dt = dtmap (Memc[database], APPEND)

	# Allocate space for and setup the database.
	call salloc (index, nimages, TY_INT)
	call salloc (c1, nimages, TY_INT)
	call salloc (c2, nimages, TY_INT)
	call salloc (l1, nimages, TY_INT)
	call salloc (l2, nimages, TY_INT)
	call salloc (isnull, nimages, TY_INT)
	call salloc (median, nimages, TY_REAL)

	call ir_setup (ir, imlist, Memi[ranges], Memc[trimsection],
	    Memc[medsection], outim, Memi[index], Memi[c1], Memi[c2],
	    Memi[l1], Memi[l2], Memi[isnull], Memr[median])

	# Write the parameters to the database file.
	call ir_dtwparams (dt, Memc[outimage], Memc[trimsection],
	    Memc[medsection], ir)

	# Make the output image.
	call ir_mkmosaic (imlist, Memc[trimsection], outim, Memi[index],
	    Memi[c1], Memi[c2], Memi[l1], Memi[l2], Memi[isnull],
	    Memr[median], IR_NXSUB(ir), IR_NYSUB(ir), IR_OVAL(ir), subtract)

	# Write the database file.
	call ir_dtwinput (imlist, Memc[trimsection], Memc[outimage], dt,
	    Memi[index], Memi[c1], Memi[c2], Memi[l1], Memi[l2], Memi[isnull],
	    Memr[median], IR_NXSUB(ir) * IR_NYSUB(ir), subtract, verbose)

	# Close up files and free space.
	call dtunmap (dt)
	call imunmap (outim)
	call clpcls (imlist)
	call sfree (sp)
	call mfree (ir, TY_STRUCT)
end


define	NTYPES	7

# IR_GET_IMTYPE -- Procedure to get the image type.

int procedure ir_get_imtype (c)

char	c	# character denoting the image type

int	i, typecodes[NTYPES]
int	stridx()
string	types "usilrdx"
data	typecodes /TY_USHORT, TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE,
		   TY_COMPLEX/

begin
	i = stridx (c, types)
	if (i == 0)
	    return (ERR)
	else
	    return (typecodes[i])
end


# IR_SETUP -- Setup the data base parameters for the images.

procedure ir_setup (ir, imlist, ranges, trimsection, medsection, outim,
	index, c1, c2, l1, l2, isnull, median)

pointer	ir			# pointer to the ir structure
pointer	imlist			# pointer to the list of input images
int	ranges[ARB]		# list of missing subrasters
char	trimsection[ARB]	# input image section for output
char	medsection[ARB]		# input image section for median computation
pointer	outim			# pointer to the output image
int	index[ARB]		# index array
int	c1[ARB]			# array of beginning column limits
int	c2[ARB]			# array of ending column limits
int	l1[ARB]			# array of beginning line limits
int	l2[ARB]			# array of ending line limits
int	isnull[ARB]		# output input image order number
real	median[ARB]		# output median of input image

int	i, j, k, nimrows, nimcols, imcount, next_null
pointer	sp, imname, im, buf
int	get_next_number(), imtgetim()
pointer	immap(), imgs2r()
real	amedr()

begin
	nimcols = IM_LEN(outim,1)
	nimrows = IM_LEN(outim,2)

	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	imcount = 1
	next_null = 0
	if (get_next_number (ranges, next_null) == EOF)
	    next_null = IR_NXSUB(ir) * IR_NYSUB(ir) + 1

	# Loop over the input images.
	do i = 1, IR_NXSUB(ir) * IR_NYSUB(ir) {

	    # Set the indices array.
	    call ir_indices (i, j, k, IR_NXSUB(ir), IR_NYSUB(ir),
	        IR_CORNER(ir), IR_RASTER(ir), IR_ORDER(ir))
	    index[i] = i
	    c1[i] = max (1, min (1 + (j - 1) * (IR_NCOLS(ir) -
	        IR_NXOVERLAP(ir)), nimcols))
	    c2[i] = min (nimcols, max (1, c1[i] + IR_NCOLS(ir) - 1))
	    l1[i] = max (1, min (1 + (k - 1) * (IR_NROWS(ir) -
	        IR_NYOVERLAP(ir)), nimrows))
	    l2[i] = min (nimrows, max (1, l1[i] + IR_NROWS(ir) - 1))

	    # Set the index of each image in the image template
	    # and compute the median of the subraster.
	    if (i < next_null) {
		isnull[i] = imcount
		if (medsection[1] != EOS) {
		    if (imtgetim (imlist, Memc[imname], SZ_FNAME) == EOF)
			call error (0, "Error reading input image list.")
		    call strcat (medsection, Memc[imname], SZ_FNAME)
		    im = immap (Memc[imname], READ_ONLY, TY_CHAR)
		    buf = imgs2r (im, 1, int (IM_LEN(im,1)), 1, int (IM_LEN(im,
		        2)))
		    median[i] = amedr (Memr[buf], int (IM_LEN(im,1)) *
			    int (IM_LEN(im,2)))
		    call imunmap (im)
		} else
		    median[i] = INDEFR
		imcount = imcount + 1
	    } else {
		isnull[i] = 0
		if (medsection[1] == EOS)
		    median[i] = INDEFR
		else
		    median[i] = IR_OVAL(ir)
		if (get_next_number (ranges, next_null) == EOF)
	    	    next_null = IR_NXSUB(ir) * IR_NYSUB(ir) + 1
	    }

	}

	call imtrew (imlist)
	call sfree (sp)
end


# IR_SETIM -- Procedure to set up the output image characteristics.

pointer procedure ir_setim (ir, list, trimsection, outimage, nimcols, nimrows,
	opixtype)

pointer	ir		# pointer to the ir structure
pointer	list		# pointer to list of input images
char	trimsection[ARB]# input image section
char	outimage[ARB]	# name of the output image
int	nimcols		# number of output image columns
int	nimrows		# number of output image rows
int	opixtype	# output image pixel type

int	ijunk, nc, nr
pointer	sp, imname, im, outim
pointer	imtgetim(), immap()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# Get the size of the first subraster.
	if (imtgetim (list, Memc[imname], SZ_FNAME) != EOF) {
	    call strcat (trimsection, Memc[imname], SZ_FNAME)
	    im = immap (Memc[imname], READ_ONLY, 0)
	    IR_NCOLS(ir) = IM_LEN(im,1)
	    IR_NROWS(ir) = IM_LEN(im,2)
	    call imunmap (im)
	    call imtrew (list)
	} else
	    call error (0, "Error reading first input image.\n")

	# Compute the size of the output image.
	ijunk = IR_NXSUB(ir) * IR_NCOLS(ir) - (IR_NXSUB(ir) - 1) *
	    IR_NXOVERLAP(ir)
	if (IS_INDEFI(nimcols))
	    nc = ijunk
	else
	    nc = max (nimcols, ijunk)
	ijunk = IR_NYSUB(ir) * IR_NROWS(ir) - (IR_NYSUB(ir) - 1) *
	    IR_NYOVERLAP(ir)
	if (IS_INDEFI(ijunk))
	    nr = ijunk
	else
	    nr = max (nimrows, ijunk)

	# Set the output pixel type.
	if (opixtype == ERR)
	    opixtype = TY_REAL

	# Open output image and set the parameters.
	outim = immap (outimage, NEW_IMAGE, 0)
	IM_NDIM(outim) = 2
	IM_LEN(outim,1) = nc
	IM_LEN(outim,2) = nr
	IM_PIXTYPE(outim) = opixtype

	call sfree (sp)

	return (outim)
end


# IR_MKMOSAIC -- Procedure to make the mosaiced image.

procedure ir_mkmosaic (imlist, trimsection, outim, index, c1, c2, l1, l2,
	isnull, median, nxsub, nysub, oval, subtract)

pointer	imlist		# pointer to input image list
char	trimsection[ARB]# input image section
pointer	outim		# pointer to the output image
int	index[ARB]	# index array for sorting the images
int	c1[ARB]		# array of column beginnings
int	c2[ARB]		# array of column endings
int	l1[ARB]		# array of line beginnings
int	l2[ARB]		# array of line endings
int	isnull[ARB]	# index of input image in the template
real	median[ARB]	# array of input image median values
int	nxsub		# number of subrasters per output image column
int	nysub		# number of subrasters per output image row
real	oval		# pixel value of undefined output image regions
int	subtract	# subtract the median off each subraster

int	i, j, noutcols, noutlines, olineptr, ll1, ll2
pointer	sp, inimage, imptrs, buf
pointer	imtrgetim(), immap(), impl2r()

begin
	# Allocate temporary space.
	call smark (sp)
	call salloc (imptrs, nxsub, TY_POINTER)
	call salloc (inimage, SZ_FNAME, TY_CHAR)

	# Sort the subrasters on the yindex.
	call ir_qsorti (l1, index, index, nxsub * nysub)

	noutcols = IM_LEN(outim,1)
	noutlines = IM_LEN(outim,2)

	# Loop over the input images.
	olineptr = 1
	do i = 1, nxsub * nysub, nxsub {

	    # Compute the line and column limits.
	    ll1 = l1[index[i]]
	    ll2 = l2[index[i]]

	    # Open the nxsub input images.
	    do j = i, i + nxsub - 1 {
		if (isnull[index[j]] <= 0) {
		    Memc[inimage] = EOS
		    Memi[imptrs+j-i] = NULL
		} else {
		    if (imtrgetim (imlist, isnull[index[j]], Memc[inimage],
		        SZ_FNAME) == EOF)
			Memi[imptrs+j-i] = NULL
		    else {
			call strcat (trimsection, Memc[inimage], SZ_FNAME)
			Memi[imptrs+j-i] = immap (Memc[inimage], READ_ONLY, 0)
		    }
		}
	    }

	    # Write out the undefined lines.
	    while (olineptr < ll1) {
		buf = impl2r (outim, olineptr)
		call amovkr (oval, Memr[buf], noutcols)
		olineptr = olineptr + 1
	    }

	    # Write the output lines.
	    call ir_mklines (Memi[imptrs], outim, index, c1, c2, ll1, ll2,
	        median, i, nxsub, oval, subtract)
	    olineptr = ll2 + 1

	    # Close up the images.
	    # Open the nxsub input images.
	    do j = i, i + nxsub - 1 {
		if (Memi[imptrs+j-i] != NULL)
		    call imunmap (Memi[imptrs+j-i])
	    }

	}

	# Write out the remaining undefined lines.
	while (olineptr < noutlines) {
	    buf = impl2r (outim, olineptr)
	    call amovkr (oval, Memr[buf], noutcols)
	    olineptr = olineptr + 1
	}

	call sfree (sp)
end


# IR_MKLINES -- Construct and output image lines.

procedure ir_mklines (imptrs, outim, index, c1, c2, l1, l2, meds, init, nsub, 
	oval, subtract)

pointer	imptrs[ARB]		# array of input image pointers
pointer	outim			# output imnage pointer
int	index[ARB]		# array of indices
int	c1[ARB]			# array of beginning columns
int	c2[ARB]			# array of ending columns
int	l1			# beginning line
int	l2			# ending line
real	meds[ARB]		# array of median values
int	init			# first index
int	nsub			# number of subrasters
real	oval			# output value
int	subtract		# subtract the median value

int	i, j, jj, noutcols
pointer	obuf, ibuf
pointer	impl2r(), imgl2r()

begin
	noutcols = IM_LEN(outim, 1)
	do i = l1, l2 {
	    obuf = impl2r (outim, i)
	    call amovkr (oval, Memr[obuf],  noutcols)
	    do j = 1, nsub {
		jj = index[j+init-1]
		if (imptrs[j] != NULL) {
		    ibuf = imgl2r (imptrs[j], i - l1 + 1)
		    if (subtract == YES)
		        call asubkr (Memr[ibuf], meds[jj], Memr[obuf+c1[jj]-1],
			    c2[jj] - c1[jj] + 1)
		    else
		        call amovr (Memr[ibuf], Memr[obuf+c1[jj]-1], c2[jj] -
			    c1[jj] + 1)
		}
	    }
	}
end


# IR_DTWINPUT -- Procedure to  write the output database file.

procedure ir_dtwinput (imlist, trimsection, outimage, dt, index, c1, c2, l1,
	l2, isnull, median, nsub, subtract, verbose)

int	imlist		# input image list
char	trimsection[ARB]# trim section of input image
char	outimage[ARB]	# output image
pointer	dt		# pointer to the database file
int	index[ARB]	# array of sorted indices (not used at present)
int	c1[ARB]		# array of beginning column limits
int	c2[ARB]		# array of ending column limits
int	l1[ARB]		# array of beginning line limits
int	l2[ARB]		# array of ending line limits
int     isnull[ARB]	# image name index
real	median[ARB]	# array of medians
int	nsub		# number of subrasters
int	subtract	# subtract the median from the subraster
int	verbose		# print verbose messages

int	i
pointer	sp, imname
int	imtrgetim()

begin
	call smark (sp)
	call salloc (imname, SZ_FNAME, TY_CHAR)

	# Write out the number of subrasters.
	call dtput (dt, "\tnsubrasters\t%d\n")
	    call pargi (nsub)

	do i = 1, nsub {

	    if (isnull[i] <= 0)
		call strcpy ("nullimage", Memc[imname], SZ_FNAME)
	    else if (imtrgetim (imlist, isnull[i], Memc[imname],
	        SZ_FNAME) != EOF)
		call strcat (trimsection, Memc[imname], SZ_FNAME)
	    else
		Memc[imname] = EOS

	    call dtput (dt,"\t%s  %s[%d:%d,%d:%d]  %g  %g\n")
	        call pargstr (Memc[imname])
	        call pargstr (outimage)
	        call pargi (c1[i])
	        call pargi (c2[i])
	        call pargi (l1[i])
	        call pargi (l2[i])
	        call pargr (median[i])
	    if (subtract == YES)
	        call pargr (-median[i])
	     else
	        call pargr (0.0)

	    if (verbose == YES) {
	        call printf ("imcopy  %s  %s[%d:%d,%d:%d]  %g  %g\n") 
		    call pargstr (Memc[imname])
		    call pargstr (outimage)
		    call pargi (c1[i])
		    call pargi (c2[i])
		    call pargi (l1[i])
		    call pargi (l2[i])
		    call pargr (median[i])
	        if (subtract == YES)
		    call pargr (-median[i])
	        else
	            call pargr (0.0)
	    }
	}

	call sfree (sp)
end
