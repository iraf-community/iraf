include <fset.h>
include	<error.h>
include <mach.h>
include <imhdr.h>
include <tbset.h>
include "wfits.h"

# TAB_WRITE_FITZ -- Procedure to convert a single Table file or trailer
# ascii text to a FITS file.

procedure tab_write_fitz (tab_file, fits_file, fits_fd)

char	tab_file[SZ_FNAME]	# IRAF file name
char	fits_file[SZ_FNAME]	# FITS file name
int	fits_fd			# Output FITS descriptor

int	chars_rec, dev_blk, ncols
pointer	tp, ext

pointer	tbtopn()
int	mtopen(), open(), fstati(), mtfile()
char	extn[SZ_EXTN], line[SZ_LINE]

errchk	immap, imunmap, open, mtopen, close, smark, salloc, sfree
errchk	delete, tab_write_header, tab_write_data, tbtopn
errchk  txt_wfits, open, tbtopn
int	trl, nch, fnextn(), fnldir(), tbpsta(), in_type, strdic()

string  tables_exts "|trl|txt|log|ocx|pdq|pod|cmh|trx|rpt|cgr|dgr|dta|poa"
define	err_ 99
include "wfits.com"

begin
	# Do not write table data in ieee format. FITS for tables
	# is all ascii. BINTABLE will be written in ieee regardless 
	# of the ieee value.
	ieee = NO
	in_type = 0
	ext_type = TABLE
	if (bintable == YES)
	   ext_type = BINTABLE


	# Open input table. Actually it can be an SDAS table (.tab) or
	# a text file with the extensions in tables_exts
	trl = NO
	nch = fnextn (tab_file, extn, SZ_EXTN)
        call strlwr(extn)
	if (strdic (extn, extn, SZ_EXTN, tables_exts) != 0)
	   trl = YES

	ncols = 1
	if (trl == YES) {
	   tp = open (tab_file, READ_ONLY, TEXT_FILE)
	} else {
	   tp = tbtopn (tab_file, READ_ONLY, 0)
	   ncols = tbpsta (tp, TBL_NCOLS)
	}
	 

	# Open output file.
	#
	if (mtfile (fits_file) == YES) {
	    chars_rec = (blkfac * len_record * FITS_BYTE) / (SZB_CHAR *
	        NBITS_BYTE)
	    if (first_time == YES)
	       fits_fd = mtopen (fits_file, WRITE_ONLY, chars_rec)
	    dev_blk = fstati (fits_fd, F_MAXBUFSIZE)
	    if (dev_blk != 0 && chars_rec > dev_blk) {
		call flush(STDOUT)
		call error (0, "Blocking factor too large for tape drive")
	    }
	} else if (first_time == YES)
	    fits_fd = open (fits_file, NEW_FILE, BINARY_FILE)

	# Allocate memory for program data structure.
	call calloc (ext, LEN_EXTENSION, TY_STRUCT)
	call calloc(EXT_PCOL(ext), ncols, TY_POINTER)
	call calloc(EXT_PCUNDEF(ext), ncols, TY_BOOL)

	nch = fnldir (tab_file, EXTNAME(ext), SZ_FNAME)
	call strcpy (tab_file[nch+1], EXTNAME(ext), SZ_FNAME)

	EXT_TYPE(ext) = in_type

	if (long_header == YES)
	   call printf ("\n")
	# Write header and image.
	iferr {
	    if (trl == YES) {
	       call txt_wfits (tp, fits_file, ext, fits_fd)
	    }else {
	       call tab_write_header (tp, fits_file, ext, fits_fd)
	       if (short_header == YES)
	          call prtbinfo_key (tp, EXTNAME(ext))

	       call tab_write_data (tp, ext, fits_fd)
	    }

	} then {
	    # Close files and cleanup.
	    if (trl == YES)
	       call close (tp)
	    else
	       call tbtclo (tp)
	    call close (fits_fd)
	    call mfree(EXT_PCUNDEF(ext), TY_BOOL)
	    call mfree(EXT_PCOL(ext), TY_INT)
	    call mfree(ext, TY_STRUCT)

	    call errget (line, SZ_LINE)
	    call error (13, line)

	} else {
	    if (long_header == YES)
	        call printf ("\n")
	    # Close files and cleanup.
	    if (trl == YES)
	       call close (tp)
	    else
	       call tbtclo (tp)
            if (extensions == NO)
	       call close (fits_fd)
	    call mfree(EXT_PCUNDEF(ext), TY_BOOL)
	    call mfree(EXT_PCOL(ext), TY_INT)
	    call mfree(ext, TY_STRUCT)
	}
	return
end
