include <imhdr.h>
include <mach.h>
include <tbset.h>
include "wfits.h"

# TAB_WRITE_HEADER -- Procedure to write table FITS headers. The FITS header
# parameters are encoded one by one until the FITS END keyword is detected.
# If the long_header switch is set the full FITS header is printed on the
# standard output. If the short header parameter is specified only the image
# title and dimensions are printed.

procedure tab_write_header (tp, fits_file, ext, fits_fd)

pointer	tp		# pointer to the IRAF image
char	fits_file[SZ_FNAME]
pointer	ext		# pointer to the extension structure
int	fits_fd		# the FITS file descriptor

char	card[LEN_CARD+1], trim_card[LEN_CARD+1]
int	nrecords, recntr, cardptr, cardcnt, stat, cards_per_rec

int	tab_card_encode(), strncmp()
int	tab_init_card_encode()

errchk	tab_init_card_encode, tab_card_encode
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record

include "wfits.com"

begin
                                                              
	EXT_BITPIX(ext) = FITS_BYTE

	# initialize card counters, these counters are used only for
	# information printed to the standard output
	recntr = 1
	cardptr = 1
	cardcnt = 1
	cards_per_rec = len_record / LEN_CARD

	# Get set up to write header
	stat = tab_init_card_encode (tp, ext)
	call wft_init_write_pixels (len_record, TY_CHAR, FITS_BYTE)


	# Write dummy main FITS header for tables only.
	if (first_time == YES)
	   call tab_dummy_main_header (fits_file, fits_fd)

	# Preread the table for existance of undefined values
	call check_undef (tp, ext)

	# Write the cards to the FITS header
	repeat {
	    stat = tab_card_encode (tp, ext, card)
	    if (stat == NO)
		next

	    call wft_write_pixels (fits_fd, card, LEN_CARD)

	    if (long_header == YES) {
		call wft_trimstr (card, trim_card, LEN_CARD)
	        call printf ("%2d/%2d:--  %s\n")
		    call pargi (recntr)
		    call pargi (cardptr)
		    call pargstr (trim_card)

	        if (mod (cardcnt, cards_per_rec) == 0) {
	            recntr = recntr + 1
	            cardptr = 1
	        } else
		    cardptr = cardptr + 1
	        cardcnt = cardcnt + 1
	    }

	} until (strncmp (card, "END     ", LEN_KEYWORD) == 0)

	# Write last header records.
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	   call printf ("%d Header  ")
		call pargi (nrecords)
	}
end




# TAB_INIT_CARD_ENCODE -- This procedure initializes the card encoding
# procedure.  The cards counters are initialized and the number of history cards
# calculated.

int procedure tab_init_card_encode (tp, ext)

# both entry points
pointer	tp	# pointer to the IRAF image
pointer	ext	# pointer to the extension structure

# entry tab_card_encode
pointer colp, cp
int	tab_card_encode		# entry point
char	card[LEN_CARD+1]

int	cardno		# Header card counter
int	colno		# Column number index
int	descno		# Index for column card used in tab_column_card
int	ncount		# Counter for user parameter cards
int	nstandard	# Number of standard cards in header

int	rowlen, lenfmt, npar, nuserp, stat, ncols, i, ndesc
int	datatype, num_tabdesc, nelem
char	sppfmt[SZ_COLFMT], fntfmt[SZ_COLFMT]

int	tab_standard_card(), tab_column_card(), tbpsta()
int	wft_last_card(), tab_user_card(), tbcigi()
pointer	tbcnum()
errchk	tab_standard_card, tab_column_card, wft_last_card

include "wfits.com"

begin
	# Initialize the card pointers.
	cardno = 1
	colno = 1
	descno = 1

	# Initialize the card counters.
	nstandard = 10    # 9+1 to allow for a blank card
	num_tabdesc = 7   # 6+1 ditto
	if (ext_type == BINTABLE)
	   num_tabdesc = 5+1

	ncols = tbpsta (tp, TBL_NCOLS)
	EXT_NCOLS(ext) = ncols
	cp = EXT_PCOL(ext)

	rowlen = 0
	maxlen = 0
	do i = 1, ncols {
	   colp = tbcnum (tp, i)
	   call tbcigt (colp, TBL_COL_FMT, sppfmt, SZ_COLFMT)
	   datatype = tbcigi (colp, TBL_COL_DATATYPE)
	   nelem = tbcigi (colp, TBL_COL_LENDATA)

	   call chgfmt (sppfmt, datatype, fntfmt, lenfmt)
	   if (ext_type == BINTABLE) {
	      switch(datatype) {
	      case TY_BOOL:
		 lenfmt = nelem 		# Boolean is 1 char long
	      case TY_SHORT:
		 lenfmt = nelem*SZ_SHORT*SZB_CHAR
	      case TY_INT,TY_REAL,TY_LONG:
		 lenfmt = nelem*SZ_INT*SZB_CHAR
	      case TY_DOUBLE:
		 lenfmt = nelem*SZ_DOUBLE*SZB_CHAR
	      default:
	         lenfmt = abs(datatype)
	      }
	   }
	   rowlen = rowlen + lenfmt
	   maxlen = max (maxlen, lenfmt)
	   Memi[cp+i-1] = colp
	}
	# For binary tables 'rowlen' is the number of bytes per row
	# Make this is an even number so we can use TY_CHAR i/o.
	#
	EXT_LENAXIS(ext,1) = rowlen

	# there are up to 'num_tabdesc' descriptor per columns.
	#
	ndesc = ncols*num_tabdesc + nstandard	
	npar = tbpsta (tp, TBL_NPAR)
	nuserp = ndesc + npar
	ncount = 1
	
	return (YES)

# TAB_CARD_ENCODE -- Procedure to encode the FITS header parameters into
# FITS card images.

entry	tab_card_encode (tp, ext, card)
	# fetch the appropriate FITS header card image
	if (cardno <= nstandard) {
	    stat = tab_standard_card (cardno, tp, ext, card)
	} else if (cardno <= ndesc) {
	    if (descno == 1)
		colp = tbcnum (tp, colno)
	    stat = tab_column_card (tp, colp, ext, colno, descno, card)
	} else if (cardno <= nuserp) {
	    stat = tab_user_card (tp, ncount, card)
	    ncount = ncount + 1
	} else {
	    stat = wft_last_card (card)
	}

	cardno = cardno + 1

	return (stat)
end


# TAB_DUMMY_MAIN_HEADER -- procedure to write a FITS main header for tables.
 
procedure tab_dummy_main_header (fits_file, fits_fd)

char    fits_file[SZ_FNAME]
int	fits_fd		# pointer to the fits structure
char	card[LEN_CARD]	# FITS card image

char	dname[SZ_FNAME]
char	datestr[LEN_DATE]
int	nrecords, wft_last_card(), stat
errchk	wft_encodeb, wft_encodec, wft_encodei, wft_encodel, wft_axis_encode
include "wfits.com"

begin
	call wft_encodeb ("SIMPLE", YES, card, "FITS STANDARD")
	call wft_write_pixels (fits_fd, card, LEN_CARD)
	
	call wft_encodei ("BITPIX", 8, card, "Character information")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	call wft_encodei ("NAXIS", 0, card, "No image data array present")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	call wft_encodeb ("EXTEND", YES, card,
	    "There maybe standard extensions")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	call wft_encode_date (datestr, LEN_DATE)
	call wft_encodec ("DATE" , datestr, card, 
	    "Date tape was written")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	call wft_encodec ("ORIGIN", "STScI-STSDAS", card,
		"Fitsio version 21-Feb-1996")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	call strcpy ("null_image", dname, SZ_FNAME)
	call wft_encodec ("FILENAME", dname, card, 
		"ZERO LENGTH DUMMY IMAGE")
	call wft_write_pixels (fits_fd, card, LEN_CARD)

	if (short_header == YES)
	   call prtdumm_key (fits_file, datestr)
	stat = wft_last_card (card)
	call wft_write_pixels (fits_fd, card, LEN_CARD)
	call wft_write_last_record (fits_fd, nrecords)
end

# TAB_USER_CARD -- Procedure to write FITS table user's parameters

int procedure tab_user_card (tp, ncount, card)

pointer	tp		# i: pointer to table descriptor
int	ncount		# i: current user parameter number
char	card[SZ_PARREC] # o: FITS card with user parameter

char	keyword[SZ_KEYWORD], str[SZ_PARREC], comment[SZ_PARREC]
int	len_object, strlen(), ival, sscan(), stat, strmatch()
int	dtype, ip, k
char    out[SZ_PARREC]
int	strsearch(), stridx(), eqindex, qoindex, strcmp(), strncmp()

include "wfits.com"
begin
	
	call tbhgnp (tp, ncount, keyword, dtype, str)
	call get_string(str,str,SZ_PARREC]
	call tbhgcm (tp, keyword, comment, SZ_PARREC)

	if (strncmp("/ ", comment, 2) == 0) {
	   for (ip=1; ip<=SZ_PARREC-2; ip=ip+1)
	       comment[ip] = comment[ip+2]
	}
	if (str[1] == EOS) {
	    if (keyword[1] == EOS)
	       call amovkc(" ",card, LEN_CARD)
	    else
	       call wft_ncencode (keyword, str, card)
	    return(YES)
	}
	if (dtype == TY_CHAR) {
	   # See if content of str has a '=' on it; indicating a long ( >8 )
	   # keyword name
	   # 
	   if (strsearch (str, "= ") == 0 &&
	       strmatch (keyword, "^HISTORY") == 0 &&
	       strmatch (keyword, "^COMMENT") == 0 ) {
		 k = 1
	         if (str[1] == '"') k=2
	         ip = strlen(str)
		 if (str[ip] == '"')  str[ip] = EOS
		 call strcpy (str[k], str, ip)
		 if (keyword[1] != EOS)
	            call wft_encodec (keyword, str, card, comment)
		 else {
                    call sprintf (card, LEN_CARD, "%10t%-71.71s")
	    	      call pargstr (str)
		 }  
	   } else {
	      # see if we have a '= ' in the str before the opening quote
	      eqindex = stridx("=", str)
	      qoindex = stridx("'", str)
	      if (eqindex > 0 && eqindex < qoindex) {
		 # find the closing quote
		 ip = strlen(str[qoindex])
	         if (str[qoindex+ip-1] == '\'')
		    str[qoindex+ip-1] = EOS
		 call wft_ncencode (keyword, str[qoindex+1], card)
	      } else {
		 if (strcmp(keyword, "HISTORY") == 0 || 
		     strcmp(keyword, "COMMENT") == 0) {
                    call sprintf (card, LEN_CARD, "%-8.8s%s")
	    	      call pargstr (keyword)
	    	      call pargstr (str)
	            len_object = strlen(card) + 1
	            do ip = len_object, LEN_CARD
	               card[ip] = ' '
		 } else {     
		    if (keyword[1] != EOS)
	               call wft_encodec(
			 keyword, str, card, comment)
		    else {
                       call sprintf (card, LEN_CARD, "%10t%-71.71s")
	    	         call pargstr (str)
 		    }
		 }
	      }
	   }
	} else if (dtype == TY_INT ) {
	   stat = sscan (str)
		call gargi (ival)
	   call wft_encodei (keyword, ival, card, comment)	   
	} else if (dtype == TY_BOOL) {
	   stat = sscan (str)
		call gargi (ival)
	   call wft_encodeb (keyword, ival, card, comment)	   
	} else if (dtype == TY_DOUBLE || dtype == TY_REAL) {
	   call get_string (str, out, SZ_PARREC)
	   if (comment[1] == EOS) {
	      call sprintf (card, LEN_CARD, "%-8.8s= %20s")
		   call pargstr (keyword)
		   call pargstr (out)
	   } else {
	      call sprintf (card, LEN_CARD, "%-8.8s= %20s / %-47.47s")
		   call pargstr (keyword)
		   call pargstr (out)
		   call pargstr (comment)
	   }
	   len_object = strlen(card) + 1
	   do ip = len_object, LEN_CARD
	      card[ip] = ' '
	} else  {
	   call wft_encodec (keyword, str, card, comment)
	}
	return(YES)
end

# CHECK_UNDEF -- Procedure to preread the input table and see if null
# values are defined in it. Set an array with flags for each column
# in case UNDEF is encountered.

procedure check_undef (tp, ext)

pointer	tp		# IRAF table descriptor
pointer ext		# Pointer to extension structure

pointer colp, cp,up
real	rval
double  dval
int	ival
short   sval
bool    bval, bnull
char	chval[SZ_LINE]

int	i, j, datatype, ir, nlines, ncols
int	tbpsta(), tbcigi() 

include "wfits.com"

begin
	nlines = tbpsta (tp, TBL_NROWS)
	ncols = tbpsta (tp, TBL_NCOLS)
	if (nlines == 0)
	    return


	up = EXT_PCUNDEF(ext)
	cp = EXT_PCOL(ext)
	do j = 1, ncols
	   Memb[up+j-1] = false	  

        do ir = 1, nlines {

 	   do i = 0, ncols-1 {
	      colp = Memi[cp+i]
              datatype = tbcigi (colp, TBL_COL_DATATYPE)
              switch (datatype) {
	      case TY_REAL:
	        call tbrgtr (tp, colp, rval, bnull, 1, ir) 
	      case TY_DOUBLE:
	        call tbrgtd (tp, colp, dval, bnull, 1, ir) 
	      case TY_INT:
	        call tbrgti (tp, colp, ival, bnull, 1, ir) 
	      case TY_SHORT:
	        call tbrgts (tp, colp, sval, bnull, 1, ir) 
	      case TY_BOOL:
	        call tbrgtb (tp, colp, bval, bnull, 1, ir) 
	      default:
	        if (datatype < 0 || datatype == TY_CHAR) {
	           call tbrgtt (tp, colp, chval, bnull, SZ_LINE, 1, ir)
	        } else {
	          call error (13, "CHECK_UNDEF: invalid data type")
		}
	      }
	      if (bnull)
	         Memb[up+i] = bnull
	   }
        }
end

include "dfits.h"

# PRTDUMM_KEY -  Print information about the dummy header if the 
# corresponding keywords are selected.
 
procedure prtdumm_key (fits_file, datestr)
 
char	fits_file[SZ_FNAME]
char    datestr[LEN_STRING]

char	str[LEN_CARD]		# card data string
int	nk,strlen(), nch, tape, mtfile()
char    line[SZ_LINE]
 
int	strmatch(), itoc()
include "wfits.com"
include	"dfits.com"
 
begin
	# Search the keyword in the card table
	line[1] = EOS
	tape = mtfile(fits_file) 
        do nk = 1, nkeywords {
	   if (strmatch (Memc[key_table[nk]], "FILENAME") > 0)
	      call strcpy ("null_image", str, LEN_CARD)
	   else if (strmatch (Memc[key_table[nk]], "FITSNAME") > 0)
	      if (tape == YES)
		 nch = itoc (file_number, str, LEN_CARD)
	      else
	         call strcpy (fits_file, str, LEN_CARD)
	   else if (strmatch (Memc[key_table[nk]], "DIMENS") > 0) {
	      str[1] = '0'
	      str[2] = EOS
	   } else
	       str[1] = EOS 
	   call print_string (line, str, Memc[fmt_table[nk]], opt_table[nk])
	}
	call printf ("%80.80s\n")
	     call pargstr(line)
	nch = strlen (line)
	line[nch+1] = '\n'
	call put_in_log (line)
end
