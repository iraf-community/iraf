include <ctype.h>
include <tbset.h>
include "tbtables.h"

define	SZ_FITS_REC	80	# size of a FITS header record

# tbfanp -- add new parameter to FITS table
#
# Phil Hodge, 24-Jul-1995  Subroutine created.
# Phil Hodge, 20-Jul-1998  For blank keyword, call fsprec.

procedure tbfanp (tp, keyword, dtype, str, parnum)

pointer tp			# i: pointer to table descriptor
char	keyword[SZ_KEYWORD]	# i: keyword for the parameter
int	dtype			# i: data type
char	str[ARB]		# i: string containing the value of the param.
int	parnum			# o: number of the parameter in the table
#--
pointer sp
pointer fitsrec			# scratch for FITS output record
pointer value			# scratch for first "word" in input str
pointer blanks			# scratch for blank fill
char	ukey[SZ_KEYWORD]	# keyword in upper case
int	status			# used for fitsio
int	keysadd			# returned by fsghsp and ignored
int	vlen			# length of string
int	i
int	strlen()
int	ip, ip2, nchar, ival, ctoi(), ctowrd()
bool	streq()
errchk	tbferr

begin
	status = 0

	call strcpy (keyword, ukey, SZ_KEYWORD)
	call strupr (ukey)
	do i = strlen (ukey), 1, -1 {		# trim trailing blanks
	    if (IS_WHITE(ukey[i]))
		ukey[i] = EOS
	    else
		break
	}

	if (streq (ukey, "HISTORY")) {

	    call fsphis (TB_FILE(tp), str, status)

	} else if (streq (ukey, "COMMENT")) {

	    call fspcom (TB_FILE(tp), str, status)

	} else if (ukey[1] == EOS) {		# blank keyword

	    call smark (sp)
	    call salloc (fitsrec, SZ_FITS_REC, TY_CHAR)
	    call sprintf (Memc[fitsrec], SZ_FITS_REC, "          %s")
		call pargstr (str)
	    call fsprec (TB_FILE(tp), Memc[fitsrec], status)
	    call sfree (sp)

	} else {

	    call smark (sp)
	    call salloc (fitsrec, SZ_FITS_REC, TY_CHAR)
	    call salloc (value, SZ_FITS_REC, TY_CHAR)

	    # Extract one "word".
	    ip = 1
	    nchar = ctowrd (str, ip, Memc[value], SZ_FITS_REC)
	    while (str[ip] == ' ')
		ip = ip + 1

	    if (dtype == TY_CHAR) {

		# Check whether the value is quoted.  If so, then Memc[value]
		# already contains the value, and there's no comment.
		if (str[1] != '"' && str[1] != '\'') {
		    call strcpy (str, Memc[value], SZ_FITS_REC)
		    ip = strlen (str) + 1	# str[ip] = EOS, so no comment
		}

		# Pad value with blanks if it's smaller than eight characters.
		vlen = strlen (Memc[value])
		if (vlen < 8) {
		    do i = vlen+1, 8
			Memc[value+i-1] = ' '
		    Memc[value+8] = EOS
		}

		# Format the info into the buffer.
		call sprintf (Memc[fitsrec], SZ_FITS_REC, "%-8s= '%s'")
		    call pargstr (ukey)
		    call pargstr (Memc[value])
		vlen = strlen (Memc[fitsrec])
		if (vlen < 30) {
		    do i = vlen+1, 30
			Memc[fitsrec+i-1] = ' '
		    Memc[fitsrec+30] = EOS
		}
		call strcat (" / ", Memc[fitsrec], SZ_FITS_REC)
		if (str[ip] != EOS)			# append comment
		    call strcat (str[ip], Memc[fitsrec], SZ_FITS_REC)

	    } else if (dtype == TY_BOOL) {

		call strlwr (Memc[value])
		ip2 = 1
		nchar = ctoi (Memc[value], ip2, ival)
		if (streq (Memc[value], "t") || streq (Memc[value], "true") ||
		    streq (Memc[value], "yes") || ival == 1) {
		    call sprintf (Memc[fitsrec], SZ_FITS_REC,
				"%-8s=                    T / ")
		    call pargstr (ukey)
		} else {
		    call sprintf (Memc[fitsrec], SZ_FITS_REC,
				"%-8s=                    F / ")
		    call pargstr (ukey)
		}
		if (str[ip] != EOS)			# append comment
		    call strcat (str[ip], Memc[fitsrec], SZ_FITS_REC)

	    } else {

		vlen = strlen (Memc[value])
		if (vlen < 21) {
		    # Right justify at column 30.
		    call salloc (blanks, 21-vlen, TY_CHAR)
		    do i = 1, 21-vlen
			Memc[blanks+i-1] = ' '
		    Memc[blanks+21-vlen] = EOS
		    call sprintf (Memc[fitsrec], SZ_FITS_REC, "%-8s=%s%s / ")
			call pargstr (ukey)
			call pargstr (Memc[blanks])
			call pargstr (Memc[value])
		} else {
		    call sprintf (Memc[fitsrec], SZ_FITS_REC, "%-8s=%s / ")
			call pargstr (ukey)
			call pargstr (Memc[value])
		}
		if (str[ip] != EOS)			# append comment
		    call strcat (str[ip], Memc[fitsrec], SZ_FITS_REC)
	    }

	    # Add the record to the FITS file.
	    call fsprec (TB_FILE(tp), Memc[fitsrec], status)

	    call sfree (sp)
	}

	if (status != 0)
	    call tbferr (status)

	# Get the number of header parameters, and assume that that
	# is the number of the parameter we just added to the header.
	call fsghsp (TB_FILE(tp), parnum, keysadd, status)
	if (status != 0)
	    call tbferr (status)
	TB_NPAR(tp) = parnum
end
