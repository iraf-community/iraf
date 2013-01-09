include <tbset.h>
include "tblerr.h"

# tbfpri -- copy primary header
# This routine may copy the primary header of an input FITS file to
# an output FITS file.  The input header will only be copied under the
# following circumstances:
#
#   The output file does not already exist.
#   The intable and outtable file names imply they are FITS files
#   (i.e. the filename extensions are ".fits", ".fit", or "%%f").
#   The primary header/data unit of the input file has a null data
#   portion (i.e. NAXIS = 0).
#
# If the input primary header was in fact copied, creating the output
# file, copied will be set to YES; otherwise, copied will be NO.
#
# Phil Hodge, 18-Jan-1999  Subroutine created.
# Phil Hodge,  8-Apr-1999  Call vfn_expand_ldir to get host OS file names.
# Phil Hodge, 12-Apr-1999  Call tbttyp to get file type;
#		remove table type from calling sequence of tbparse.
# Phil Hodge,  1-Jun-1999  Declare ifd & ofd as two-element arrays.

procedure tbfpri (intable, outtable, copied)

char	intable[ARB]	# i: name of FITS file to be copied
char	outtable[ARB]	# i: name of new FITS file
int	copied		# o: YES if input header was copied to output
#--
pointer sp
pointer ifname, ofname	# input & output file names
pointer os_infile, os_outfile	# host operating system file names
pointer fname, extn	# for discarding directory from output file name
pointer dummy		# misc ignored strings
pointer	intbl, cnv, url
int	ifd[2]		# C pointer for input (template) FITS file
int	ofd[2]		# C pointer for output FITS file
# These variables and equivalence statements are used to force 8-byte
# alignment of ifd and ofd.
#double	d_ifd, d_ofd
#equivalence (ifd, d_ifd)
#equivalence (ofd, d_ofd)
int	naxis		# NAXIS from primary header of input
int	status		# zero is OK
int	itype, otype	# file type based on filename extension
int	hdu		# HDU number (ignored)
int	exists		# YES if the file exists
int	blocksize
int	nchar
int	morekeys	# extra space (none) in primary header
int	fnroot(), fnextn(), tbparse(), tbttyp(), vot_to_fits()
int	access(), strncmp()
bool	is_votable()
errchk	tbferr, tbparse, tbttyp, vfn_expand_ldir

begin
	call smark (sp)
	call salloc (ifname, SZ_FNAME, TY_CHAR)
	call salloc (ofname, SZ_FNAME, TY_CHAR)
	call salloc (intbl, SZ_PATHNAME, TY_CHAR)
	call salloc (cnv, SZ_PATHNAME, TY_CHAR)
	call salloc (url, SZ_PATHNAME, TY_CHAR)
	call salloc (dummy, SZ_FNAME, TY_CHAR)
	call salloc (os_infile, SZ_FNAME, TY_CHAR)
	call salloc (os_outfile, SZ_FNAME, TY_CHAR)

	# Get name of output file; i.e. strip off any extension name or
	# number, row & column selectors.
	nchar = tbparse (outtable, Memc[ofname], Memc[dummy], SZ_FNAME, hdu)

	# Get file type, and check whether output file already exists.
	otype = tbttyp (Memc[ofname], exists)

	# Convert from iraf virtual file name to actual file name.
	call vfn_expand_ldir (Memc[ofname], Memc[os_outfile], SZ_FNAME)

	if (exists == YES || otype != TBL_TYPE_FITS) {
	    copied = NO
	    call sfree (sp)
	    return
	}


	# Check if we're opening a URL, and whether it is already cached.
        call aclrc (Memc[cnv], SZ_PATHNAME)
        call aclrc (Memc[intbl], SZ_PATHNAME)
        if (strncmp ("http:", intable, 5) == 0) {
            call strcpy (intable, Memc[url], SZ_PATHNAME)
            call fcname ("cache$", Memc[url], "f", Memc[intbl], SZ_PATHNAME)
            call strcpy (Memc[intbl], Memc[cnv], SZ_PATHNAME)
            call strcat (".fits", Memc[cnv], SZ_PATHNAME)

            if (access (Memc[cnv], 0, 0) == NO) {
                call fcadd ("cache$", Memc[url], "", Memc[intbl], SZ_PATHNAME)
                if (access (Memc[cnv],0,0) == YES && is_votable (Memc[cnv])) {
                    if (vot_to_fits (Memc[intbl], Memc[intbl]) != OK) {
                        call error (ER_TBCONVERT,
                            "tbtopn: cannot convert table format")
                    }
                }
            } else
                call strcpy (Memc[cnv], Memc[intbl], SZ_PATHNAME)
	} else
            call strcpy (intable, Memc[intbl], SZ_PATHNAME)


	# Get name of input file, and get file type.
	nchar = tbparse (Memc[intbl], Memc[ifname], Memc[dummy], SZ_FNAME, hdu)
	call vfn_expand_ldir (Memc[ifname], Memc[os_infile], SZ_FNAME)

	itype = tbttyp (Memc[ifname], exists)	# exists for input is ignored

	# Only relevant for FITS tables.
	if (itype != TBL_TYPE_FITS) {
	    copied = NO
	    call sfree (sp)
	    return
	}

	status = 0
	ifd[2] = 0		# not needed for four-byte C pointers
	ofd[2] = 0

	# Get a unit number for the input file, and open the file.
	call fsgiou (ifd, status)
	blocksize = 2880
	call fsopen (ifd, Memc[os_infile], 0, blocksize, status)
	if (status != 0) {
	    call fsfiou (ifd, status)
	    call tbferr (status)
	}

	# Check whether the primary header/data unit contains a data portion.
	# We'll only copy the primary header if there's no data.

	call fsgkyj (ifd, "NAXIS", naxis, Memc[dummy], status)
	if (status != 0)
	    call tbferr (status)

	if (naxis == 0) {	# no data portion

	    # Open the output file.
	    call fsgiou (ofd, status)
	    call fsinit (ofd, Memc[os_outfile], blocksize, status)
	    if (status != 0)
		call tbferr (status)

	    # Copy the primary header.
	    morekeys = 0
	    call fscopy (ifd, ofd, morekeys, status)
	    if (status != 0)
		call tbferr (status)

	    # Extract root and extension (discarding directory).
	    call salloc (fname, SZ_FNAME, TY_CHAR)
	    call salloc (extn, SZ_FNAME, TY_CHAR)
	    nchar = fnroot (Memc[os_outfile], Memc[fname], SZ_FNAME)
	    nchar = fnextn (Memc[os_outfile], Memc[extn], SZ_FNAME)
	    call strcat (".", Memc[fname], SZ_FNAME)
	    call strcat (Memc[extn], Memc[fname], SZ_FNAME)
	    call fsukys (ofd, "FILENAME", Memc[fname], "name of file", status)
	    if (status != 0)
		call tbferr (status)

	    call fsclos (ofd, status)
	    call fsfiou (ofd, status)

	    copied = YES

	} else {

	    # Input header will not be copied because the primary HDU
	    # does contain a data portion.
	    copied = NO
	}

	call fsclos (ifd, status)
	call fsfiou (ifd, status)

	call sfree (sp)
end
