include <chars.h>		# defines NEWLINE
include <mach.h>		# defines SZB_CHAR
include <tbset.h>
include "tbtables.h"

define	SZ_FITS_BLOCK	2880	# one FITS block

# This function returns the table type.  A flag is also returned in the
# calling sequence to indicate whether the file exists or not.  If the
# file cannot be opened read-only, it is presumed not to exist.  If the
# table name is null, a value of zero will be returned for the table type.
#
# For a nonexistent file, the type is based on the file name.  For a file
# that does exist, the type can be incorrect if the file is not really a
# table (e.g. an image pixel file); for the definitive table type, open
# the table read-only and use the function tbpsta (tp, TBL_WHTYPE).
#
# The function fstdfile is used to check for STDIN, STDOUT, etc.  If the
# file name is one of these, the table type is set to TBL_TYPE_TEXT, and
# exists is set to YES.
#
# If the file does exist, the beginning of the file (up to 2880 bytes)
# is read, and the contents are compared with what is expected for a
# FITS file or an STSDAS format binary table.  In the case of a FITS
# file, only the first block is checked, not the table extension itself,
# so in this case we're really checking file type rather than table type.
#
# To check for a FITS file, the first three 80-byte records are compared
# with what is required, SIMPLE, BITPIX, and NAXIS (see actual strings
# for values, including trailing blanks).  In addition, there must not be
# any newline characters (ASCII 0x0A) within the first 2880 bytes.
#
# If the file is not FITS, it is then compared with STSDAS format.  The
# beginning of the record that was read to check for FITS is copied to a
# local buffer that is equivalenced to an integer array.  The ninth
# element of that array is the table type, 11 for row-ordered and 12 for
# column-ordered.  If that element is neither 11 nor 12, the size-info
# record will be byte-swapped (into a scratch array) to check for the
# possibility that the table is STSDAS format but was created on a
# machine with different byte order.  If the byte-swapped value is either
# 11 or 12, the table type will be set accordingly; that is, the fact that
# the table is byte swapped is not taken to be an error (this can be
# handled later by e.g. tbtrsi).
#
# If the file is neither FITS nor STSDAS format, it is assumed to be a
# text file.  This will therefore be misleading if this function is called
# for a binary file that is not a table.
#
# If the file does not exist, the table type is decided based on the
# filename extension.  If the extension is ".fits", ".fit", or "??f",
# the type is set to fits (TBL_TYPE_FITS).  If the extension does not
# match one of those patterns, the table is assumed to be a row-ordered
# STSDAS format table (TBL_TYPE_S_ROW).  Since the file doesn't exist,
# however, we really can't tell what it will end up being.
#
# Phil Hodge, 16-Apr-1999  Function created.
# Phil Hodge, 22-Oct-2004  Check for byte-swapped size information record.

int procedure tbttyp (tablename, exists)

char	tablename[ARB]	# i: name of the file containing the table
int	exists		# o: YES if the file can be opened
#--
size_t	sz_val
size_t	c_1
pointer sp
pointer buf		# for reading from the file
int	ttype		# table type
int	i		# loop index
long	j
int	fd
size_t	nread, nelem	# chars to read, number actually read
int	open()
long	read()
int	len, strlen(), strncmp()
int	ofd, fstdfile()	# to check for STDIN, STDOUT, etc.; ofd is ignored
bool	streq()

pointer fname		# file name without trailing brackets
pointer fullname	# input file name with ".tab" appended
pointer	extname
int	hdu, dummy	# returned by tbparse and ignored
int	tbparse()

# These are used for checking for a FITS file.
int	len_simple, len_bitpix, len_naxis	# lengths of strings
string	simple_t "SIMPLE  =                    T"
string	simple_f "SIMPLE  =                    F"
string	bitpix   "BITPIX  =                "
string	naxis    "NAXIS   =                "
#                 123456789012345678901234567890

# These are used for checking for an STSDAS binary table.
long	i_sizinfo[LEN_SIZINFO]	# size information record
char	c_sizinfo[SZ_SIZINFO]
equivalence (i_sizinfo[1], c_sizinfo[1])
long	b_sizinfo[LEN_SIZINFO]	# byte-swapped size information record

errchk	open, read, tbparse

begin
	c_1 = 1

	# initial values
	exists = YES
	# zero is a flag to indicate we don't know the type yet
	ttype = 0

	if (fstdfile (tablename, ofd) == YES)
	    return (TBL_TYPE_TEXT)

	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fname, sz_val, TY_CHAR)
	call salloc (extname, sz_val, TY_CHAR)

	# Brackets on end of tablename?  Remove them.
	len = strlen (tablename)
	if (len < 1) {
	    exists = NO
	    return (ttype)		# zero, indeterminate type
	}
	if (tablename[len] == ']') {
	    dummy = tbparse (tablename, Memc[fname], Memc[extname], SZ_FNAME,
			hdu)
	} else {
	    call strcpy (tablename, Memc[fname], SZ_FNAME)
	}

	# From now on we'll use Memc[fname] as the file name.

	iferr {
	    fd = open (Memc[fname], READ_ONLY, BINARY_FILE)
	} then {
	    exists = NO
	}

	if (exists == NO) {
	    # Append default extension and try again.
	    sz_val = SZ_FNAME
	    call salloc (fullname, sz_val, TY_CHAR)
	    call strcpy (Memc[fname], Memc[fullname], SZ_FNAME)
	    call strcat (".tab", Memc[fullname], SZ_FNAME)
	    iferr {
		fd = open (Memc[fullname], READ_ONLY, BINARY_FILE)
	    } then {
		exists = NO
	    } else {
		exists = YES
	    }
	}

	if (exists == YES) {

	    # Read the beginning of the file to decide what type it is.

	    sz_val = SZ_FITS_BLOCK
	    call salloc (buf, sz_val, TY_CHAR)

	    nread = SZ_FITS_BLOCK / SZB_CHAR
	    nelem = read (fd, Memc[buf], nread)
	    call close (fd)

	    # Copy the beginning to a local buffer so we can extract integer
	    # values.  This will be used if we need to check for stsdas format.
	    # We need to copy this out because we're going to modify the
	    # buffer (unpack it).
	    do i = 1, SZ_SIZINFO
		c_sizinfo[i] = Memc[buf+i-1]

	    if (nelem == EOF)			# empty file
		ttype = TBL_TYPE_TEXT

	    if (nelem == nread) {		# did we read entire block?

		sz_val = SZ_FITS_BLOCK
		call strupk (Memc[buf], Memc[buf], sz_val)

		# It could be FITS; check the first three "cards."

		len_simple = strlen (simple_t)
		len_bitpix = strlen (bitpix)
		len_naxis = strlen (naxis)
		if ((strncmp (simple_t, Memc[buf], len_simple) == 0 ||
		     strncmp (simple_f, Memc[buf], len_simple) == 0) &&
		     strncmp (bitpix, Memc[buf+80], len_bitpix) == 0 &&
		     strncmp (naxis, Memc[buf+160], len_naxis) == 0) {
		    ttype = TBL_TYPE_FITS
		}

		# But if there are any newlines, it's not a FITS file.
		do j = 0, nelem-1 {
		    if (Memc[buf+j] == NEWLINE) {
			ttype = 0
			break
		    }
		}
	    }

	    if (ttype == 0 && nelem >= SZ_SIZINFO) {

		# It could be an STSDAS format binary table.  If so,
		# it begins with a size information record, and we can
		# check the table type and software version number.

		# Check the part of the size information buffer that
		# specifies table type.
		if (S_TYPE(i_sizinfo) == TBL_TYPE_S_ROW) {
		    ttype = TBL_TYPE_S_ROW
		} else if (S_TYPE(i_sizinfo) == TBL_TYPE_S_COL) {
		    ttype = TBL_TYPE_S_COL
		} else {
		    # Check whether i_sizinfo is byte swapped.  If so, set
		    # the table type if it matches row or column.  Note that
		    # this is not considered an error (at this point).
		    sz_val = SZ_SIZINFO*SZB_CHAR
		    if ( SZ_LONG == 2 ) {
			call bswap4 (i_sizinfo, c_1, b_sizinfo, c_1, sz_val)
		    } else {
			call bswap8 (i_sizinfo, c_1, b_sizinfo, c_1, sz_val)
		    }
		    if (S_TYPE(b_sizinfo) == TBL_TYPE_S_ROW)
			ttype = TBL_TYPE_S_ROW
		    if (S_TYPE(b_sizinfo) == TBL_TYPE_S_COL)
			ttype = TBL_TYPE_S_COL
		}

		# Reset table type to unknown if the version is not reasonable.
		if (S_VERSION(i_sizinfo) < 0)
		    ttype = 0
# disable this test for the time being:
#		if (S_VERSION(i_sizinfo) > TBL_CURRENT_VERSION)
#		    ttype = 0
	    }

	    if (ttype == 0)
		ttype = TBL_TYPE_TEXT		# default

	} else {				# exists = NO

	    # Check the filename extension.  If it's not FITS, assume it
	    # will be row-ordered STSDAS format.
	    len = strlen (Memc[fname])
	    if (streq (Memc[fname+len-5], ".fits") ||
		streq (Memc[fname+len-4], ".fit") ||
		(Memc[fname+len-4] == '.' && Memc[fname+len-1] == 'f'))	# ".??f"
		ttype = TBL_TYPE_FITS
	    else
		ttype = TBL_TYPE_S_ROW
	}

	call sfree (sp)
	return (ttype)
end
