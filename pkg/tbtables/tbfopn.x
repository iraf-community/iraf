include <tbset.h>
include "tbtables.h"
include "tblfits.h"		# for FITS_END_OF_FILE

# tbfopn -- open FITS file and table
# This routine opens an existing table in a FITS file.
#
# The unit number used by the FITSIO interface is gotten and assigned to
# what would be the fd file number for ordinary iraf I/O.
#
# If the HDU was specified by name in the input file name
# (i.e. root.fits[extname]), the extension with that value of EXTNAME
# will be found in the FITS file.  
# If the HDU was specified by number, that extension will be opened
# if it has XTENSION = 'TABLE' or 'BINTABLE'.
# If the extension was not specified in the file name (i.e. extname="",
# extver=-1, hdu=-1), the first extension of type TABLE or BINTABLE will be
# opened, and TB_HDU will be assigned the number of that extension.
#
# If the HDU was given as zero, the primary header will be opened, and
# the numbers of rows and columns will be set to zero.  This gives access
# to the primary header keywords.  It will be an error, though, to try to
# read or write table data, since the primary HDU cannot be a table.
#
# NOTE:
# On entry to this routine, TB_HDU is either a flag (-1) or a specific
# extension number, using the numbering convention of the user interface,
# where the primary HDU is zero.  On successful exit from this routine,
# TB_HDU will be the actual extension number, using the same numbering
# convention.
#
# Phil Hodge,  6-Jul-1995  Subroutine created.
# Phil Hodge,  2-Feb-1996  Use tbffnd to find table in file.
# Phil Hodge, 15-May-1998  If error opening the file, call fsfiou and
#			set TB_FILE(tp) to NULL before calling tbferr.
# Phil Hodge,  5-Mar-1999  Move 'TB_FILE(tp) = fd' to after the call to fsopen.
# Phil Hodge, 22-Mar-1999  Use TB_OS_FILENAME(tp) instead of TB_NAME(tp) as
#			the name of the file to open.
# Phil Hodge,  1-Jun-1999  Use both TB_FILE and TB_FILE2;
#	fd is a two-element array, eight-byte aligned.
# Phil Hodge,  7-Jun-1999  Set TB_SUBTYPE instead of TB_HDUTYPE.

procedure tbfopn (tp)

pointer tp		# i: pointer to table descriptor
#--
pointer sp
pointer extname		# for value of EXTNAME from table header
pointer errmess		# scratch for error message
int	blocksize
int	status		# zero is OK
int	hdu		# HDU number
int	extver		# extension version number
int	hdutype		# type of HDU
int	fd[2]		# unit number for FITS file; cfitsio pointer
double	dfd		# to force alignment of fd
#equivalence (fd, dfd)	# to force alignment of fd
int	tbffnd()
bool	strne()
errchk	tbffnd, tbferr

begin
	call smark (sp)
	call salloc (extname, SZ_LINE, TY_CHAR)

	status = 0

	# Get a unit number.
	# This call does nothing if linked with CFITSIO.  In that case,
	# fd is output from fsopen, and fd is actually a C pointer.
	fd[2] = 0		# not needed for four-byte C pointers
	call fsgiou (fd, status)

	# Open the FITS file.
	blocksize = 2880
	if (TB_IOMODE(tp) == READ_ONLY) {
	    call fsopen (fd, TB_OS_FILENAME(tp), 0, blocksize, status)
	} else if (TB_IOMODE(tp) == READ_WRITE) {
	    call fsopen (fd, TB_OS_FILENAME(tp), 1, blocksize, status)
	} else {
	    call fsfiou (fd, status)
	    TB_FILE(tp) = 0
	    TB_FILE2(tp) = 0
	    call error (1, "tbfopn:  invalid iomode")
	}
	if (status != 0) {
	    call fsfiou (fd, status)
	    TB_FILE(tp) = 0
	    TB_FILE2(tp) = 0
	    call tbferr (status)
	}
	TB_FILE(tp) = fd[1]
	TB_FILE2(tp) = fd[2]

	# Find the specified extension in the file.
	hdu = tbffnd (tp, Memc[extname], SZ_LINE, extver, hdutype)
	if (hdu == EOF) {
	    call salloc (errmess, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[errmess], SZ_LINE, 
		"table not found in FITS file `%s'")
		call pargstr (TB_NAME(tp))
	    call error (status, Memc[errmess])
	}

	# Update values in table descriptor.

	if (TB_HDU(tp) < 0)
	    TB_HDU(tp) = hdu

	# Update EXTNAME from header to get correct case.
	if (strne (TB_EXTNAME(tp), ""))
	    call strcpy (Memc[extname], TB_EXTNAME(tp), SZ_LINE)

	if (extver > 0)
	    TB_EXTVER(tp) = extver

	if (hdutype == TBL_FITS_BINARY)
	    TB_SUBTYPE(tp) = TBL_SUBTYPE_BINTABLE
	else if (hdutype == TBL_FITS_ASCII)
	    TB_SUBTYPE(tp) = TBL_SUBTYPE_ASCII
	else if (hdutype == TBL_FITS_IMAGE)		# primary header
	    TB_SUBTYPE(tp) = TBL_SUBTYPE_IMAGE
	else
	    TB_SUBTYPE(tp) = TBL_SUBTYPE_UNKNOWN

	call sfree (sp)
end
