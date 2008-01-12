include <error.h>
include <fset.h>		# to check whether output is redirected
include <imhdr.h>
include <mach.h>		# for MAX_SHORT
include <tbset.h>
include "imtab.h"

define	NCOLS	(1 + IM_MAXDIM)	# max number of columns to write to the table

# imtab -- create a table from an image
# This task copies data from an image to a table.  Pixel values are
# read from the image line by line and written to a column in increasing
# row number.
# If the table already exists then columns will be added to it; note that
# the column names must not conflict with existing names.
#
# Phil Hodge, 10-Jan-1991  Task created.
# Phil Hodge, 17-Mar-1992  Include text as a valid table type; call pargstr
#			to pass cname in case column name already exists.
# Phil Hodge, 16-Apr-1993  Include short datatype.
# Phil Hodge, 28-Sep-1993  Include wcs option for pixel coordinates.
# Phil Hodge, 13-Dec-1993  Slight changes to itb_init because of optimizer
#			problems with SGI Fortran.
# Phil Hodge,  8-Jun-1999  Set output to STDOUT if redirected.
# Phil Hodge, 30-Mar-2000  Allow lists of names for input and output.

procedure imtab()

pointer input			# name of an input image
pointer outlist			# names of output tables
pointer outtable		# name of an output table
char	cname[SZ_COLNAME,NCOLS]	# column names
char	c_root[SZ_COLNAME]	# root for column names for position
pointer wcs			# wcs name if c_root != ""
pointer formats			# list of formats for pixel coords
pointer ttype			# type of output table (if new)
#--
pointer sp
pointer im			# pointer to image descriptors
pointer xps, xpr, xpd		# pointer to input data from image
pointer tp			# pointer to descriptor for output table
pointer cp[NCOLS]		# column descriptors
pointer mw, ct			# mwcs pointers

pointer imt, tnt		# pointers for filename templates
int	nin, nout		# numbers of names in lists
int	junk

long	v[IM_MAXDIM]		# for call to imgnld
int	lcoords[IM_MAXDIM]	# "logical" coordinates, copied from v
real	ipcoords[IM_MAXDIM]	# "logical" coordinates, copied from v
real	opcoords[IM_MAXDIM]	# "physical" coordinates from ipcoords
double	iwcoords[IM_MAXDIM]	# "logical" coordinates, copied from v
double	owcoords[IM_MAXDIM]	# "world" coordinates from iwcoords

int	wcs_type		# wcs name as an int
int	ax[IM_MAXDIM]		# ax[i] is physical axis for logical axis i
real	inr[IM_MAXDIM]		# copy of input coordinates used by itb_ctranr
double	ind[IM_MAXDIM]		# copy of input coordinates used by itb_ctrand
int	wcsdim			# dimension of physical image coord system
int	impixtype		# data type of image

int	ncols			# number of columns to create
int	dtype[NCOLS]		# data type of each table column
int	frow, lrow		# row number limits for tbcptd
int	row			# loop index
int	i, j, k			# loop indexes
bool	done			# loop-termination flag
int	clgwrd()
int	fstati()
int	imgnls(), imgnlr(), imgnld()
pointer imtopenp(), tbnopen()
int	imtlen(), imtgetim(), tbnlen(), tbnget()
bool	streq()

begin
	call smark (sp)
	call salloc (input, SZ_FNAME, TY_CHAR)
	call salloc (outlist, SZ_LINE, TY_CHAR)
	call salloc (outtable, SZ_FNAME, TY_CHAR)
	call salloc (wcs, SZ_FNAME, TY_CHAR)
	call salloc (formats, SZ_FNAME, TY_CHAR)
	call salloc (ttype, SZ_FNAME, TY_CHAR)

	imt = imtopenp ("input")
	nin = imtlen (imt)

	if (fstati (STDOUT, F_REDIR) == YES)
	    call strcpy ("STDOUT", Memc[outlist], SZ_LINE)
	else
	    call clgstr ("outtable", Memc[outlist], SZ_LINE)
	tnt = tbnopen (Memc[outlist])
	nout = tbnlen (tnt)

	# Compare the numbers of input and output names.
	call itb_names (nin, nout, Memc[outlist])

	# Get the column names.
	call clgstr ("colname", cname[1,1], SZ_COLNAME)	# name for data
	call clgstr ("pname", c_root, SZ_COLNAME)	# root name for position
	call xt_stripwhite (c_root)
	if (c_root[1] != EOS) {
	    wcs_type = clgwrd ("wcs", Memc[wcs], SZ_FNAME,
			"|logical|physical|world")
	    call clgstr ("formats", Memc[formats], SZ_FNAME)
	} else {
	    wcs_type = IMTAB_NO_WCS
	    Memc[formats] = EOS
	}

	# What table type should be created?
	if (streq (Memc[outlist], "STDOUT"))
	    call strcpy ("default", Memc[ttype], SZ_FNAME)
	else
	    call clgstr ("tbltype", Memc[ttype], SZ_FNAME)

	# Loop over the list of input images.
	while (imtgetim (imt, Memc[input], SZ_FNAME) != EOF) {

	    if (nout == 1)
		call tbnrew (tnt)
	    junk = tbnget (tnt, Memc[outtable], SZ_FNAME)

	    # Open the input image and the wcs and get the column data types.
	    call itb_init (Memc[input], wcs_type, im, dtype)
	    impixtype = IM_PIXTYPE(im)

	    # Initialize the wcs.
	    call itb_wcs_init (im, wcs_type, mw, ct, ax, wcsdim)
	    call amovkr (1., inr, IM_MAXDIM)
	    call amovkd (1.d0, ind, IM_MAXDIM)

	    # Initialize for reading the image.
	    do k = 1, IM_MAXDIM
		v[k] = 1

	    # Initialize for writing to the table.
	    row = 1
	    frow = 1
	    lrow = IM_LEN(im,1)
	    if (wcs_type == IMTAB_NO_WCS)
		ncols = 1
	    else if (wcs_type == IMTAB_LOGICAL)
		ncols = 1 + IM_NDIM(im)
	    else if (wcs_type == IMTAB_PHYSICAL || wcs_type == IMTAB_WORLD)
		ncols = 1 + wcsdim

	    # Open or create the output table.
	    call itb_table (im, Memc[outtable], wcs_type, Memc[ttype], ncols,
		cname, c_root, Memc[formats], dtype, tp, cp)

	    # Copy each line of the image into a column of the table.
	    done = false
	    while (!done) {

		# Assign pixel index for all but the first axis.
		if (wcs_type == IMTAB_LOGICAL)
		    do j = 2, ncols-1
			lcoords[j] = v[j]
		else if (wcs_type == IMTAB_PHYSICAL)
		    do j = 2, ncols-1
			ipcoords[j] = real (v[j])
		else if (wcs_type == IMTAB_WORLD)
		    do j = 2, ncols-1
			iwcoords[j] = double (v[j])

		if (impixtype == TY_SHORT || impixtype == TY_UBYTE)
		    done = (imgnls (im, xps, v) == EOF)
		else if (impixtype == TY_REAL || impixtype == TY_USHORT)
		    done = (imgnlr (im, xpr, v) == EOF)
		else
		    done = (imgnld (im, xpd, v) == EOF)

		if (!done) {

		    # Write the pixel coordinates.
		    if (wcs_type == IMTAB_LOGICAL) {
			do i = 1, IM_LEN(im,1) { # simply write pixel numbers
			    lcoords[1] = i
			    call tbrpti (tp, cp[2], lcoords, ncols-1, row)
			    row = row + 1
			}
		    } else if (wcs_type == IMTAB_PHYSICAL) {
			do i = 1, IM_LEN(im,1) {
			    ipcoords[1] = real (i)
			    call itb_ctranr (im, ct, ax, inr,
					ipcoords, opcoords, wcsdim)
			    call tbrptr (tp, cp[2], opcoords, ncols-1, row)
			    row = row + 1
			}
		    } else if (wcs_type == IMTAB_WORLD) {
			do i = 1, IM_LEN(im,1) {
			    iwcoords[1] = double (i)
			    call itb_ctrand (im, ct, ax, ind,
					iwcoords, owcoords, wcsdim)
			    call tbrptd (tp, cp[2], owcoords, ncols-1, row)
			    row = row + 1
			}
		    }

		    # Copy image line into a portion of a column of the table.
		    if (impixtype == TY_SHORT || impixtype == TY_UBYTE)
			call tbcpts (tp, cp[1], Mems[xps], frow, lrow)
		    else if (impixtype == TY_REAL || impixtype == TY_USHORT)
			call tbcptr (tp, cp[1], Memr[xpr], frow, lrow)
		    else
			call tbcptd (tp, cp[1], Memd[xpd], frow, lrow)
		    frow = frow + IM_LEN(im,1)
		    lrow = lrow + IM_LEN(im,1)
		}
	    }
	    if (mw != NULL)
		call mw_close (mw)		# close mwcs
	    call imunmap (im)		# close image
	    call tbtclo (tp)		# close table
	}

	call imtclose (imt)
	call tbnclose (tnt)

	call sfree (sp)
end

# This routine checks the number of input and output file names.
# The number of names in the input and output lists must be the same,
# unless all the input will be written to the standard output.

procedure itb_names (nin, nout, outlist)

int	nin		# i: number of input image names
int	nout		# i: number of output table names
char	outlist[ARB]	# i: output names (to be compared with "STDOUT")
#--
bool	strne()

begin
	if (nin == 0)
	    call error (1, "no input image specified")

	if (nout == 0)
	    call error (1, "no output table specified")

	if (nin != nout && strne (outlist, "STDOUT")) {

	    if (nin == 1) {
		call eprintf ("There is one input image")
	    } else {
		call eprintf ("There are %d input images")
		    call pargi (nin)
	    }
	    if (nout == 1) {
		call eprintf (" and one output table;\n")
	    } else {
		call eprintf (" and %d output tables;\n")
		    call pargi (nout)
	    }
	    call error (1, "the lists must have the same length")
	}
end

# itb_init -- get data types and column info
# This routine opens the input image and gets the data type for each column.

procedure itb_init (input, wcs_type, im, dtype)

char	input[ARB]	# i: name of image
int	wcs_type	# i: type of wcs for pixel coordinates
pointer im		# o: imhdr pointer
int	dtype[NCOLS]	# o: data type of each table column
#--
int	i		# loop index
int	fill_extra	# dummy
pointer immap()

begin
	# Open input image.
	im = immap (input, READ_ONLY, NULL)

	# Fewer data types are allowed for tables than for images.
	switch (IM_PIXTYPE(im)) {
	case TY_UBYTE, TY_SHORT:
	    dtype[1] = TY_SHORT
	case TY_USHORT, TY_INT, TY_LONG:
	    dtype[1] = TY_INT
	case TY_REAL:
	    dtype[1] = TY_REAL
	case TY_DOUBLE:
	    dtype[1] = TY_DOUBLE
	default:
	    call error (1, "image data type not supported for tables")
	}

	# Set the data types of columns for pixel coordinates.
	fill_extra = IM_NDIM(im) + 2
	if (wcs_type == IMTAB_NO_WCS) {
	    do i = 2, NCOLS
		dtype[i] = TY_SHORT		# ignored

	} else if (wcs_type == IMTAB_LOGICAL) {
	    # Check the image size to see if we can use TY_SHORT.
	    do i = 1, IM_NDIM(im) {
		if (IM_LEN(im,i) > MAX_SHORT)
		    dtype[i+1] = TY_INT
		else
		    dtype[i+1] = TY_SHORT
	    }
	    do i = fill_extra, NCOLS
		dtype[i] = TY_SHORT		# ignored

	} else if (wcs_type == IMTAB_PHYSICAL) {
	    do i = 2, NCOLS
		dtype[i] = TY_REAL

	} else if (wcs_type == IMTAB_WORLD) {
	    do i = 2, NCOLS
		dtype[i] = TY_DOUBLE
	}
end

# itb_table -- initialization for output table
# This routine opens the output table (or creates it if it doesn't already
# exist) and creates the columns for the data and the pixel coordinates.

procedure itb_table (im, outtable, wcs_type, ttype, ncols,
		cname, c_root, formats, dtype, tp, cp)

pointer im			# i: imhdr pointer for input image
char	outtable[ARB]		# i: name of output table
int	wcs_type		# i: type of wcs for pixel coordinates
char	ttype[ARB]		# i: table type (e.g. "row")
int	ncols			# i: total number of columns to write
char	cname[SZ_COLNAME,NCOLS]	# io: column names
char	c_root[ARB]		# i: root for column name for pixels
char	formats[ARB]		# i: user-specified formats for pixels
int	dtype[NCOLS]		# i: data types of table columns
pointer tp			# o: pointer to table descriptor
pointer cp[NCOLS]		# o: column descriptors
#--
char	colunits[SZ_COLUNITS,NCOLS]	# column units
char	colfmt[SZ_COLFMT,NCOLS]	# column format
char	history[SZ_FNAME]	# for history records
int	lendat[NCOLS]		# one
int	nrows
int	i
bool	new_table		# true if the table does not already exist
bool	column_conflict		# true if column already exists
int	ip, ctowrd()
pointer tbtopn()
int	tbtacc()

begin
	colunits[1,1] = EOS
	colfmt[1,1] = EOS

	# Assign column names.
	do i = 2, NCOLS {
	    call sprintf (cname[1,i], SZ_COLNAME, "%s%d")
		call pargstr (c_root)
		call pargi (i-1)
	}

	# Replace commas with blanks in the user-specified format string.
	ip = 1
	while (formats[ip] != EOS) {
	    if (formats[ip] == ',')
		formats[ip] = ' '
	    ip = ip + 1
	}

	# Assign print format and units.
	ip = 1
	do i = 2, NCOLS {

	    if (wcs_type == IMTAB_LOGICAL) {

		# If the user specified a format, use it; otherwise,
		# assign a default.
		if (ctowrd (formats, ip, colfmt[1,i], SZ_COLFMT) < 1) {
		    if (dtype[i] == TY_INT)
			call strcpy ("%11d", colfmt[1,i], SZ_COLFMT)
		    else if (dtype[i] == TY_SHORT)
			call strcpy ("%5d", colfmt[1,i], SZ_COLFMT)
		}
		call strcpy ("pixels", colunits[1,i], SZ_COLUNITS)

	    } else if (wcs_type == IMTAB_PHYSICAL) {

		if (ctowrd (formats, ip, colfmt[1,i], SZ_COLFMT) < 1)
		    call strcpy ("%9.3f", colfmt[1,i], SZ_COLFMT)
		call strcpy ("pixels", colunits[1,i], SZ_COLUNITS)

	    } else if (wcs_type == IMTAB_WORLD) {

		if (ctowrd (formats, ip, colfmt[1,i], SZ_COLFMT) < 1)
		    colfmt[1,i] = EOS		# take the default
		colunits[1,i] = EOS		# we don't know the units

	    } else {
		colfmt[1,i] = EOS
		colunits[1,i] = EOS
	    }
	}

	do i = 1, NCOLS {
	    lendat[i] = 1
	    cp[i] = NULL
	}

	nrows = 1
	do i = 1, IM_NDIM(im)
	    nrows = nrows * IM_LEN(im,i)

	# Does the table already exist?
	new_table = (tbtacc (outtable) == NO)

	if (new_table) {
	    tp = tbtopn (outtable, NEW_FILE, NULL)

	    if (ttype[1] == 'r') {
		call tbpset (tp, TBL_WHTYPE, TBL_TYPE_S_ROW)
	    } else if (ttype[1] == 'c') {
		call tbpset (tp, TBL_WHTYPE, TBL_TYPE_S_COL)
		call tbpset (tp, TBL_ALLROWS, nrows)
	    } else if (ttype[1] == 't') {
		call tbpset (tp, TBL_WHTYPE, TBL_TYPE_TEXT)
	    }
	} else {
	    tp = tbtopn (outtable, READ_WRITE, NULL)
	}

	# Make sure the columns don't already exist.
	column_conflict = false
	if ( ! new_table ) {
	    call tbcfnd (tp, cname, cp, ncols)
	    do i = 1, ncols {
		if (cp[i] != NULL) {
		    call eprintf ("Column %s already exists.\n")
			call pargstr (cname[1,i])
		    column_conflict = true
		}
	    }
	    if (column_conflict) {
		call imunmap (im)
		call tbtclo (tp)
		call error (1,
		    "new columns in existing table must be unique")
	    }
	}

	# Define the columns.
	call tbcdef (tp, cp, cname, colunits, colfmt, dtype, lendat, ncols)

	if (new_table)
	    call tbtcre (tp)		# open the file

	# Write history info.
	call strcpy ("Column ", history, SZ_FNAME)
	call strcat (cname[1,1], history, SZ_FNAME)	# column name for data
	call strcat (" from ", history, SZ_FNAME)
	call strcat (IM_HDRFILE(im), history, SZ_FNAME)	# name of input image
	call tbhadt (tp, "history", history)
	if (ncols > 1) {
	    call strcpy ("Column ", history, SZ_FNAME)
	    call strcat (cname[1,1], history, SZ_FNAME)
	    if (ncols > 2)
		call strcat (", pixel columns ", history, SZ_FNAME)
	    else
		call strcat (", pixel column ", history, SZ_FNAME)
	    do i = 2, ncols-1 {
		call strcat (cname[1,i], history, SZ_FNAME)
		call strcat (", ", history, SZ_FNAME)
	    }
	    call strcat (cname[1,ncols], history, SZ_FNAME)
	    call tbhadt (tp, "history", history)
	}
end
