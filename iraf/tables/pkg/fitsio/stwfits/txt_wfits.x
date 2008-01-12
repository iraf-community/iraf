include <error.h>
include "wfits.h"

define	SZ_KEYWORD	8
define	LEN_TRLLINE	132

# TXT_WFITS -- routine to write a trailer file (with extension 'trl')
#	       as a fits table. A trailer file is a fix record length
#	       ascii file with around 132 character per line. It will
#	       write a fits table with 132 character per row.

procedure txt_wfits (tp, fits_file, ext, fits_fd)

pointer	tp			# pointer to the IRAF image
char    fits_file[SZ_FNAME]	# FITS filename
pointer	ext			# pointer to the extension structure
int	fits_fd			# the FITS file descriptor

char	newline, blank
char	card[LEN_CARD+1], line[SZ_LINE]
int	nlines, stat, nrecords, nch, nblanks, ind, lenp1

data	newline, blank  / '\n', ' ' /

int	getline(), wft_last_card(), stridx(), strlen()

errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record
errchk	getline

include "wfits.com"

begin
	EXT_BITPIX(ext) = FITS_BYTE
	EXT_LENAXIS(ext,1) = LEN_TRLLINE        # Trailer file line length

	# Count the number of lines in the input file and
	# look if there are lines with more than 132 chars.
	nlines = 0
	nch = getline (tp, line) 

        lenp1 = LEN_TRLLINE + 1
	repeat {
	    nlines = nlines + 1
	    if (nch > lenp1) {
		call eprintf("ERROR: Trailer file '%s' contains lines ")
		call pargstr(EXTNAME(ext))
		call eprintf("longer than 132 chars\n")
		call erract(EA_WARN)
		return
	    }
            nch = getline (tp, line)
	} until (nch == EOF)

	# Write fit table header		
	call wft_init_write_pixels (len_record, TY_CHAR, FITS_BYTE)

	# Write dummy main FITS header for tables only.
	if (first_time == YES)
	    call tab_dummy_main_header (fits_file, fits_fd)

	# print short header
	if (short_header == YES)
	    call prtxinfo_key (nlines, ext)

	call txt_wcardc (fits_fd,"XTENSION", "TABLE", "FITS STANDARD")
	call txt_wcardi (fits_fd,"BITPIX", EXT_BITPIX(ext), 
			 "8-bits per 'pixels'")
	call txt_wcardi (fits_fd,"NAXIS", 2, "Simple 2-D matrix")
	call txt_wcardi (fits_fd,"NAXIS1",EXT_LENAXIS(ext,1),
			 "No of characters per row")	
	call txt_wcardi (fits_fd,"NAXIS2", nlines, "The number of rows")
	call txt_wcardi (fits_fd,"PCOUNT", 0, "No 'random' parameters")
	call txt_wcardi (fits_fd,"GCOUNT", 1, "Only one group")
	call txt_wcardi (fits_fd,"TFIELDS", 1, "Number of fields per row")
	call txt_wcardc (fits_fd,"EXTNAME", EXTNAME(ext), "Name of table")

	# Put fits table column descriptor

	if (EXT_TYPE(ext) == TRL_FILE)
	    call txt_wcardc (fits_fd,"TTYPE1", "TRAILER_FILE", 
			     "One column per line")
	else
	    call txt_wcardc (fits_fd,"TTYPE1", "TEXT_FILE", 
			     "One column per line")
	call txt_wcardi (fits_fd,"TBCOL1", 1, "Starting column number")
	call txt_wcardc (fits_fd,"TFORM1", "A132", "Format")
	stat = wft_last_card(card)
	call wft_write_pixels (fits_fd, card, LEN_CARD)


	# Write last header records.
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	    call printf ("%d Header  ")
	    call pargi (nrecords)
	}

	# Now write the data
	call seek (tp, BOFL)

	nch = getline (tp, line)
	repeat {
	    if (nch < lenp1) {                   # Pad
		ind = stridx (newline, line)
		if (ind == 0) # (BPS 04.08.98 Fix for no newline case)
		    ind = strlen (line) + 1

		nblanks = LEN_TRLLINE - ind + 1
		call amovkc (blank, line[ind], nblanks)
	    }
	    call wft_write_pixels (fits_fd, line, LEN_TRLLINE)
	    nch = getline (tp, line)
	} until (nch == EOF)

	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	    call printf ("%d  Data records written\n")
	    call pargi (nrecords)
	}
	
end

# TXT_WCARD -- Procedure to write a card into the fits header file

procedure txt_wcardc (fd, keyword, value, comment)

int	fd			# fits file descriptor
char	keyword[SZ_KEYWORD]
char	value[LEN_CARD]
char	comment[LEN_CARD]

char	card[LEN_CARD+1]

include "wfits.com"

begin	
	call wft_encodec (keyword, value, card, comment)
	call wft_write_pixels (fd, card, LEN_CARD)
	if (long_header == YES) {
	    call printf("%s\n")
	    call pargstr(card)
        }
end

procedure txt_wcardi (fd, keyword, value, comment)

int	fd			# fits file descriptor
char	keyword[SZ_KEYWORD]
int	value
char	comment[LEN_CARD]

char	card[LEN_CARD+1]

include "wfits.com"

begin	
	call wft_encodei (keyword, value, card, comment)
	call wft_write_pixels (fd, card, LEN_CARD)
	if (long_header == YES) {
	    call printf("%s\n")
	    call pargstr(card)
        }
	
end

include <tbset.h>
include "dfits.h"

# PRTXINFO_KEY - Searches in the IM_USERAREA for a card that matchs a given
# keyword, extracts the data from that card and prints it according to a
# given format. Leading spaces, single quotes and comments are removed from
# the data.

procedure prtxinfo_key (nlines, ext)

int     nlines
pointer ext

char	str[LEN_CARD]		# card data string
int	nk,strlen(), nch
char    line[SZ_LINE]

int	strmatch()
include "wfits.com"
include	"dfits.com"

begin
	# Search the keyword in the card table
	line[1] = EOS
        do nk = 1, nkeywords {
	    if (strmatch (Memc[key_table[nk]], "FILENAME") > 0)
		call strcpy (EXTNAME(ext), str, LEN_CARD)
	    else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0)
		call strcpy ("  TABLE", str, LEN_CARD)
	    else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
		str[1] = EOS
		call sprintf (str, LEN_CARD, "%dCHx%dR")
		call pargi (EXT_LENAXIS(ext,1))
		call pargi (nlines)
	    } else if (strmatch (Memc[key_table[nk]], "BITPIX") > 0)
		call strcpy ("8tab", str, LEN_CARD)
	    else
		str[1] = EOS 
	    call print_string (line, str, Memc[fmt_table[nk]], opt_table[nk])
	}
	call printf ("%80.80s\n")
	call pargstr(line)
	nch = strlen (line)
	line[nch+1] = '\n'
	call put_in_log (line)
end
