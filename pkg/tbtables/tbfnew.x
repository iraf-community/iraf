# This file contains tbfnew and tbfroot.

include <tbset.h>
include "tbtables.h"
include "tblfits.h"	# defines SZ_FTTYPE, FITS_INDEFI, FITS_ORIGIN, etc

# tbfnew -- create a new FITS table
# If the FITS file doesn't exist it will be created.  If it does exist
# a new BINTABLE extension will be created at the end of the file (or
# one will be replaced, if overwrite=yes).
#
# Note that the TABLE extension (ASCII) is not supported.
#
# The unit number used by the FITSIO interface is gotten and assigned to
# what would be the fd file number for ordinary iraf I/O.
#
# If an EXTNAME was included in the file name (i.e. root.fits[extname]),
# that name will be used for the value of EXTNAME in the new extension.
# added to the header of the new extension.
# If an extension number was explicitly given in the file name, then
# the number must match the actual number of the extension to be created.
# It is not necessary to specify either a name or a number.
#
# If overwrite = YES was specified, an existing extension will be searched
# for and deleted, then the new table will be written in its place.
#
# On input to this routine, TB_HDU is either an explicit number or a flag
# (as is the case for tbfopn).  On output, the number of the newly created
# extension will be assigned to TB_HDU.
#
# If we're creating a new FITS file, an NEXTEND keyword with a value of 1
# will be added to the primary header.  If we're appending a new extension
# to an existing FITS file, NEXTEND will be added or updated in the primary
# header if the primary data unit is null (i.e. NAXIS = 0).  If overwrite=yes,
# then NEXTEND will not be modified, and it will not be added if it is not
# already present.
#
# Note that in calls to fsmahd, the HDU number is given as a number plus one.
# This is to emphasize the different numbering convention between FITSIO and
# the stsdas tables package.
#
# Phil Hodge,  6-Jul-1995  Subroutine created
# Phil Hodge,  2-Feb-1996  Use tbffnd to find table in file; allow overwrite.
# Phil Hodge, 10-Apr-1997  Write FILENAME to PHU if new file (call tbfroot).
# Phil Hodge, 29-Jul-1997  Set nrows to zero instead of one.
# Phil Hodge, 14-Aug-1997  Set EXTEND = T in PHU if appending a new extension.
# Phil Hodge,  5-Mar-1998  fnroot and fnextn are functions, not subroutines.
# Phil Hodge,  5-Mar-1999  Move 'TB_FILE(tp) = fd' to after fsopen or fsinit.
# Phil Hodge, 12-Mar-1999  When 'TB_FILE(tp) = fd' was moved, it was to a
#	point that was too far down; move it to the points immediately after
#	the calls to fsopen and fsinit.
# Phil Hodge, 22-Mar-1999  Use TB_OS_FILENAME(tp) instead of TB_NAME(tp) as
#	the name of the file to open.
#	Change macro names SZ_FITS_TTYPE, SZ_FITS_TFORM and SZ_FITS_TUNIT
#	to SZ_FTTYPE, SZ_FTFORM and SZ_FTUNIT respectively.
# Phil Hodge,  1-Jun-1999  Use both TB_FILE and TB_FILE2;
#	fd is a two-element array, eight-byte aligned.
# Phil Hodge,  7-Jun-1999  Set TB_SUBTYPE instead of TB_HDUTYPE.
# Phil Hodge,  8-Sep-1999  Update NEXTEND in the primary header.
# Phil Hodge, 23-Jun-2000  Use COL_TDTYPE instead of COL_DTYPE, and add
#	COL_TSCAL & COL_TZERO to the header if COL_TDTYPE != COL_DTYPE.

procedure tbfnew (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer sp
pointer errmess
pointer ttype, tform, tunit	# for arrays to be passed to fsibin
pointer extname		# for extension name, if any
pointer tdisp		# for print format
pointer keyword		# for keyword name (e.g. TDISPn)
pointer filename	# name without directory prefix
pointer comment		# returned by fsgkyj and ignored
pointer cp		# pointer to a column descriptor
int	blocksize
int	bitpix, naxis, naxes[2]
bool	simple, extend
int	status		# zero is OK
int	hdu		# HDU number (zero is primary header)
int	fd[2]		# unit number for FITS file; cfitsio pointer
double	dfd		# to force alignment of fd
#equivalence (fd, dfd)	# to force alignment of fd
int	hdutype		# type of current HDU
int	extver		# value of EXTVER from existing header, or -1
int	ncols		# number of columns, but min of 1 (for allocating space)
int	nrows		# dummy number of rows
int	nfields		# number of columns to define
int	vsize		# size of area for variable-length data (zero)
int	i
int	ival		# undefined value for int, short, bool
int	ttype0, tform0, tunit0	# offsets into 2-D char arrays
int	tdtype		# "true" data type, i.e. not scaled by tscal, tzero
char	dtype_c		# data type char:  'D', 'E', 'J', 'I', 'L', 'A'
int	nelem		# array length
bool	append		# append new hdu at end of file?
bool	done
pointer tbcnum()
int	access()
int	tbpsta(), tbcigi()
int	tbffnd()
errchk	tbffnd, tbfptf, tbferr

begin
	status = 0

	append = true			# reset if overwrite = yes
	nfields = tbpsta (tp, TBL_NCOLS)
	ncols = max (nfields, 1)

	call smark (sp)
	call salloc (ttype, (SZ_FTTYPE+1) * ncols, TY_CHAR)
	call salloc (tform, (SZ_FTFORM+1) * ncols, TY_CHAR)
	call salloc (tunit, (SZ_FTUNIT+1) * ncols, TY_CHAR)
	call salloc (extname, SZ_LINE, TY_CHAR)
	call salloc (keyword, SZ_FNAME, TY_CHAR)
	call salloc (tdisp, SZ_FNAME, TY_CHAR)

	# Get a unit number.
	# This call does nothing if linked with CFITSIO.  In that case,
	# fd is output from fsopen or fsinit, and fd is actually a C pointer.
	fd[2] = 0		# not needed for four-byte C pointers
	call fsgiou (fd, status)
	if (status != 0)
	    call tbferr (status)

	# See if the FITS file already exists.
	if (access (TB_NAME(tp), 0, 0) == YES) {

	    # Open the file read/write.
	    call fsopen (fd, TB_OS_FILENAME(tp), 1, blocksize, status)
	    if (status != 0)
		call tbferr (status)

	    TB_FILE(tp) = fd[1]
	    TB_FILE2(tp) = fd[2]

	    # If overwrite=yes, find the specified extension and delete it.
	    # Then move to the previous hdu and set a flag (append=false)
	    # indicating that the hdu to be created should be inserted
	    # following that hdu.
	    if (TB_OVERWRITE(tp) == YES) {

		hdu = tbffnd (tp, Memc[extname], SZ_LINE, extver, hdutype)
		if (hdu == EOF) {
		    call salloc (errmess, SZ_LINE, TY_CHAR)
		    call sprintf (Memc[errmess], SZ_LINE, 
			"table not found in FITS file `%s'")
			call pargstr (TB_NAME(tp))
		    call error (status, Memc[errmess])
		} else {
		    call fsdhdu (fd, hdutype, status)
		    if (status != 0)
			call tbferr (status)
		    # move to previous hdu, fitsio number (hdu-1) + 1
		    call fsmahd (fd, hdu, hdutype, status)
		    if (status != 0)
			call tbferr (status)
		    append = false
		}

	    } else {

		# Go to the primary header and make sure EXTEND = T.
		call fsmahd (fd, 1, hdutype, status)		# phdu
		extend = true
		call fsukyl (fd, "EXTEND",
			extend, "There may be standard extensions", status)
		if (status != 0)
		    call tbferr (status)

		# Find out how many extensions there currently are in the file.
		done = false
		hdu = 0				# incremented in the loop
		while (!done) {
		    # Move forward one HDU.
		    hdu = hdu + 1
		    call fsmahd (fd, hdu+1, hdutype, status)
		    if (status != 0) {		# we've reached EOF
			status = 0
			call ftcmsg()
			done = true
		    }
		}
		# Return to the primary header, and update NEXTEND to the
		# value it should be after we add a new extension.
		call fsmahd (fd, 1, hdutype, status)
		# check that the primary header is _just_ a header
		call malloc (comment, SZ_FNAME, TY_CHAR)
		call fsgkyj (fd, "NAXIS", naxis, Memc[comment], status)
		call mfree (comment, TY_CHAR)
		if (naxis == 0) {
		    call fsukyj (fd, "NEXTEND",
			hdu, "number of extensions in file", status)
		    if (status != 0)
			call tbferr (status)
		}

		# Move back to the last existing HDU in the file;
		# hdu is the number of the extension that we'll add later.
		call fsmahd (fd, (hdu-1)+1, hdutype, status)
		if (status != 0)
		    call tbferr (status)
	    }

	    # Zero or -1 mean append at end of file.
	    if (TB_HDU(tp) <= 0)
		TB_HDU(tp) = hdu		# user-interface convention

	    # Note that hdu is the last existing extension and is one indexed,
	    # while TB_HDU is the number of the new extension and is currently
	    # zero indexed.

	    # If an HDU number was specified, it ought to agree with
	    # what we've found.
	    if (TB_HDU(tp) != hdu) {
		call salloc (errmess, SZ_LINE, TY_CHAR)
		call sprintf (Memc[errmess], SZ_LINE,
	"extension %d was specified, but %s currently has %d extensions")
		    call pargi (TB_HDU(tp))
		    call pargstr (TB_NAME(tp))
		    call pargi (hdu-1)
		call error (status, Memc[errmess])
	    }

	} else {

	    # Create a new FITS file.
	    blocksize = 2880

	    if (TB_HDU(tp) <= 1) {
		TB_HDU(tp) = 1			# user interface numbering
	    } else {
		call salloc (errmess, SZ_LINE, TY_CHAR)
		call sprintf (Memc[errmess], SZ_LINE,
	"extension number in new FITS file (%s) can't be greater than one")
		    call pargstr (TB_NAME(tp))
		call error (1, Memc[errmess])
	    }

	    call fsinit (fd, TB_OS_FILENAME(tp), blocksize, status)
	    if (status != 0)
		call tbferr (status)

	    TB_FILE(tp) = fd[1]
	    TB_FILE2(tp) = fd[2]

	    # Create the primary header unit (with no data).
	    simple = true
	    bitpix = 16
	    naxis = 0
	    naxes[1] = 0
	    extend = true
	    call fsphpr (fd, simple, bitpix, naxis, naxes,
			0, 1, extend, status)
	    if (status != 0)
		call tbferr (status)

	    # Add the ORIGIN keyword to the primary header.
	    call fspkys (fd, "ORIGIN", FITS_ORIGIN, FITS_ORIGIN_CMT, status)
	    if (status != 0)
		call tbferr (status)

	    # Add the FILENAME keyword to the primary header.
	    call salloc (filename, SZ_FNAME, TY_CHAR)
	    call tbfroot (TB_NAME(tp), Memc[filename], SZ_FNAME)
	    call fspkys (fd, "FILENAME", Memc[filename], "name of file", status)
	    if (status != 0)
		call tbferr (status)

	    # Since this is a new file, set NEXTEND to one.
	    call fspkyj (fd, "NEXTEND",
			1, "number of extensions in file", status)
	    if (status != 0)
		call tbferr (status)
	}

	# Create a new empty HDU following the last extension that we
	# have accessed.  We skip this for now if overwrite = yes.
	if (append) {
	    call fscrhd (fd, status)
	    if (status != 0)
		call tbferr (status)
	}

	TB_SUBTYPE(tp) = TBL_SUBTYPE_BINTABLE

	# Create a BINTABLE extension, not ASCII table, and write
	# the required header keywords for this extension.

	# First fill the arrays of column names, etc.
	ttype0 = 0
	tform0 = 0
	tunit0 = 0
	do i = 1, nfields {
	    cp = tbcnum (tp, i)
	    tdtype = COL_TDTYPE(cp)		# "true" data type
	    nelem = tbcigi (cp, TBL_COL_LENDATA)
	    switch (tdtype) {			# get TFORM code
	    case TY_DOUBLE:
		dtype_c = 'D'
	    case TY_REAL:
		dtype_c = 'E'
	    case TY_INT:
		dtype_c = 'J'
	    case TY_SHORT:
		dtype_c = 'I'
	    case TY_BOOL:
		dtype_c = 'L'
	    default:
		dtype_c = 'A'
	    }
	    call tbcigt (cp, TBL_COL_NAME, Memc[ttype+ttype0], SZ_FTTYPE)
	    call tbcigt (cp, TBL_COL_UNITS, Memc[tunit+tunit0], SZ_FTUNIT)
	    if (tdtype > 0) {
		call sprintf (Memc[tform+tform0], SZ_FTFORM, "%d%c")
		    call pargi (nelem)
		    call pargc (dtype_c)
	    } else if (nelem > 1) {		# array of char strings
		call sprintf (Memc[tform+tform0], SZ_FTFORM, "%d%c%d")
		    call pargi (-tdtype * nelem) # FITSIO special convention
		    call pargc (dtype_c)
		    call pargi (-tdtype)
	    } else {				# character string
		call sprintf (Memc[tform+tform0], SZ_FTFORM, "%d%c")
		    call pargi (-tdtype)
		    call pargc (dtype_c)
	    }
	    ttype0 = ttype0 + SZ_FTTYPE + 1		# +1 for EOS
	    tform0 = tform0 + SZ_FTFORM + 1
	    tunit0 = tunit0 + SZ_FTUNIT + 1
	}
	nrows = 0
	vsize = 0

	if (append) {
	    # Write required keywords in newly appended hdu.
	    call fsphbn (fd, nrows, nfields, Memc[ttype], Memc[tform],
			Memc[tunit], TB_EXTNAME(tp), vsize, status)
	} else {				# insert
	    # Insert an hdu following the current one, and write keywords
	    # that define a binary table extension.
	    call fsibin (fd, nrows, nfields, Memc[ttype], Memc[tform],
			Memc[tunit], TB_EXTNAME(tp), vsize, status)
	}
	if (status != 0)
	    call tbferr (status)

	# Add version number to header, if it was specified.
	if (TB_EXTVER(tp) > 0) {
	    call fspkyj (fd, "EXTVER",
			TB_EXTVER(tp), "extension version number", status)
	    if (status != 0)
		call tbferr (status)
	}

	# Write the display format and undefined value keywords.
	do i = 1, nfields {

	    cp = tbcnum (tp, i)
	    tdtype = COL_TDTYPE(cp)

	    call sprintf (Memc[keyword], SZ_FNAME, "TDISP%d")
		call pargi (i)
	    call tbcigt (cp, TBL_COL_FMT, Memc[tdisp], SZ_FNAME)
	    call tbfptf (Memc[tdisp], Memc[tdisp], SZ_FNAME)
	    call fspkys (fd, Memc[keyword], Memc[tdisp], "display format",
			status)
	    if (status != 0)
		call tbferr (status)

	    # Create TNULL string, and add to header.
	    if (tdtype == TY_INT || tdtype == TY_SHORT) {

		call sprintf (Memc[keyword], SZ_FNAME, "TNULL%d")
		    call pargi (i)
		if (tdtype == TY_INT)
		    ival = FITS_INDEFI
		else if (tdtype == TY_SHORT)
		    ival = FITS_INDEFS
		call fspkyj (fd, Memc[keyword],
			ival, "undefined value for column", status)
		if (status != 0)
		    call tbferr (status)
	    }

	    # Add scaling parameters to header, if the true data type
	    # is not the same as the apparent data type.
	    if (tdtype != COL_DTYPE(cp)) {

		call sprintf (Memc[keyword], SZ_FNAME, "TSCAL%d")
		    call pargi (i)
		call fspkyd (fd, Memc[keyword],
			COL_TSCAL(cp), 14, "scale factor for column", status)
		if (status != 0)
		    call tbferr (status)

		call sprintf (Memc[keyword], SZ_FNAME, "TZERO%d")
		    call pargi (i)
		call fspkyd (fd, Memc[keyword],
			COL_TZERO(cp), 14, "zero offset for column", status)
		if (status != 0)
		    call tbferr (status)
	    }
	}

	call fsrdef (fd, status)	# shouldn't be needed

	call sfree (sp)
end

procedure tbfroot (fullname, fname, maxch)

char	fullname[ARB]	# i: full file name, possibly including directory
char	fname[maxch]	# o: root+extension, no directory prefix
int	maxch		# i: allocated size of fname
#--
pointer sp
pointer extn		# scratch
int	nchar, fnroot(), fnextn()
errchk	fnroot, fnextn

begin
	call smark (sp)
	call salloc (extn, maxch, TY_CHAR)

	nchar = fnroot (fullname, fname, maxch)		# extract root
	nchar = fnextn (fullname, Memc[extn], maxch)	# extract extension

	if (Memc[extn] != EOS) {
	    call strcat (".", fname, maxch)
	    call strcat (Memc[extn], fname, maxch)	# append extension
	}

	call sfree (sp)
end
