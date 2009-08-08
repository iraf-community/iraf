include <mach.h>
include <imhdr.h>
include	"rfits.h"

# RFT_READ_HEADER -- Read a FITS header.
# If BSCALE and BZERO are different from 1.0 and 0.0  scale is set to true
# otherwise scale is false.
# EOT is detected by an EOF on the first read and EOF is returned to the calling
# routine.  Any error is passed to the calling routine.

int procedure rft_read_header (fits_fd, fd_usr, fits)

int	fits_fd			# FITS file descriptor
int	fd_usr			# Fits header spool file pointer
pointer	fits			# FITS data structure

size_t	sz_val, c_1
long	i
int	stat, nread
char	card[LEN_CARD+1]

int	rft_decode_card(), strmatch()
long	rft_init_read_pixels(), rft_read_pixels()

errchk	rft_decode_card, rft_init_read_pixels, rft_read_pixels
errchk	close

include "rfits.com"

begin
	c_1 = 1

	card[LEN_CARD + 1] = '\n'
	card[LEN_CARD + 2] = EOS

	# Initialization
	FITS_BSCALE(fits) = 1.0d0
	FITS_BZERO(fits) = 0.0d0
	BLANKS(fits) = NO
	BLANK_VALUE(fits) = INDEFL
	SCALE(fits) = NO
	SIMPLE(fits) = YES
	SDASMGNU(fits) = NO
	# This xtension was set in rft_read_header
	if (EXTEND(fits) != IMAGE)
	    EXTEND(fits) = NO
	NRECORDS(fits) = 0
	OBJECT(fits) = EOS
	IRAFNAME(fits) = EOS
	OPSIZE(fits) = -1
	GCOUNT(fits) = -1
	byte_input = NO


	# Do not call again if coming from tab_rheader
	if (EXTEND(fits) != IMAGE)
	   i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	# Loop until the END card is encountered
	nread = 0
	repeat {
	    sz_val = LEN_CARD
	    i = rft_read_pixels (fits_fd, card, sz_val, NRECORDS(fits), c_1)
	    if ((i == EOF) && (nread == 0)) {		# At EOT
		call close(fd_usr)
		return (EOF)
	    } else if (EXTEND(fits) == IMAGE) {
		nread = nread + 1
		EXTEND(fits) = NO
	    } else if ((nread == 0) && strmatch (card, "^SIMPLE  ") == 0) {
		call flush (STDOUT)
		call error (30, "RFT_READ_HEADER: Not a FITS file")
	    } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    } else
	        nread = nread + 1

	    # Print FITS card images if long_header option specified
	    if (long_header == YES) {
		call printf ("%s")
		    call pargstr (card)
	    }
	    stat = rft_decode_card (fits, fd_usr, card)
	} until (stat == YES)   # stat == YES if END card encountered.

	if (OPSIZE(fits) == -1 && gkey == TO_MG) {   # NO OPSIZE keyword
	   gkey = DEF_GPB 
#	 call printf ("Warning: fits file cannot be converted to multigroup\n")
	}

	return (nread)
end

include <ctype.h>
define	NBITS_CHAR	(SZB_CHAR * NBITS_BYTE)
# RFT_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in fits.h.

int procedure rft_decode_card (fits, fd_usr, card)

pointer	fits		# FITS data structure
int	fd_usr		# file descriptor of user area
char	card[ARB]	# FITS card

pointer pn
char	cval, str[LEN_CARD], cdpat[SZ_LINE]
double	dval
int	nchar, icol, j, k, ndim, date, origin

bool	rft_equald()
int	strmatch(), ctoi(), ctol(), ctod(), cctoc(), strlen()
int	patmake(), patmatch(), check_index()
errchk	putline

include	"rfits.com"

begin
	icol = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	    return(YES)
	} else if (strmatch (card, "^SIMPLE  ") != 0) {
	    nchar = cctoc (card, icol, cval)
	    if (cval != 'T') {
		call printf("RFT_DECODE_CARD: Non-standard FITS format \n")
		SIMPLE(fits) = NO
	    }
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    nchar = ctoi (card, icol, BITPIX(fits))
	    ieee = NO
	    if (BITPIX(fits) < 0) {
	       ieee = YES
	       BITPIX(fits) = -BITPIX(fits)
	    }
	    nchar = patmake ("CD[1-7]_[1-7]", cdpat, SZ_LINE)
	} else if (strmatch (card, "^BLANK   ") != 0) {
	    BLANKS(fits) = YES
	    nchar = ctol (card, icol, BLANK_VALUE(fits))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    nchar = ctoi (card, icol, j)
	    NAXIS(fits) = check_index (j, ENAXIS)
	    # assume default values for CWS
	    ndim = NAXIS(fits)
	    do k = 1, ndim {
	       pn = WCS_PDES(fits,k)
	       CRVAL(pn) = 1.0
	       CRPIX(pn) = 1.0
     	       CDELT(pn) = 1.0
	       CROTA(pn) = 0.0
	       call strcpy ("PIXEL", CTYPE(pn), SZ_WCSCTYPE)
	       do j = 1, ndim {
	          if (k == j)
		     CDMATRIX(pn,j) = 1.0
		  else
		     CDMATRIX(pn,j) = 0.0
	       }
	    }
	    date= YES
	    origin = YES
	    MAKE_CD(fits) = YES
	    # To avoid warning messages when reading fits files with
	    # tables only.
	    if (ndim == 0 && gkey != IMH) gkey = DEF_GPB
	} else if (strmatch (card, "^NAXIS") != 0) {

	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)

	    j = check_index (j, ENAXISN)

	    nchar = ctol (card, icol, NAXISN(fits,j))
	} else if (strmatch (card, "^BLOCKED ") != 0) {
	    # Just ignore the card
	} else if (strmatch (card, "^GROUPS  ") != 0) {
	    nchar = cctoc (card, icol, cval)
	    if (cval == 'T' && NAXIS(fits) != 0) {
		call error (6, "RFT_DECODE_CARD: Group data not implemented")
	    }
	} else if (strmatch (card, "^SDASMGNU") != 0) {
	    SDASMGNU(fits) = YES
	    nchar = ctoi (card, icol, GCOUNT(fits))
	    # If the number of rows is zero, then there is no attached
	    # table, since the original file has PCOUNT = 0.
#	    if (GCOUNT(fits) >= 1 && gkey != TO_MG) {
#	       call putline (fd_usr, card)
#	       gkey = NON_GPB
#	    }
	    MAKE_CD(fits) = NO
	} else if (strmatch (card, "^EXTEND  ") != 0) {
	    nchar = cctoc (card, icol, cval)
	    if (cval == 'T') {
		EXTEND(fits) = YES
	    }
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    call rft_get_fits_string (card, OBJECT(fits), LEN_CARD)
	    call strcat (" (Xtension)",OBJECT(fits), LEN_CARD)
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^BSCALE  ") != 0) {
	    nchar = ctod (card, icol, dval)
	    if (! rft_equald (dval, 1.0d0) && scale == YES)
		SCALE(fits) = YES
	    FITS_BSCALE(fits) = dval
	} else if (strmatch (card, "^BZERO   ") != 0) {
	    nchar = ctod (card, icol, dval)
	    if (! rft_equald (dval, 0.0d0) && scale == YES)
		SCALE(fits) = YES
	    FITS_BZERO(fits) = dval
	} else if (strmatch (card, "^DATAMAX ") != 0) {
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^DATAMIN ") != 0) {
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else if ((strmatch (card, "^IRAF-MAX") != 0) ||
	           (strmatch (card, "^ALLG-MAX") != 0 )) {
	    if (gkey != TO_MG)
	       call putline (fd_usr, card)
	} else if ((strmatch (card, "^IRAF-MIN") != 0) ||
	           (strmatch (card, "^ALLG-MIN") != 0 )) {
	    if (gkey != TO_MG)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^IRAF-B/P") != 0) {
	    if (gkey != TO_MG)
	       call putline (fd_usr, card)
	} else if ((strmatch (card, "^IRAFTYPE") != 0) ||
	           (strmatch (card, "^ODATTYPE")!= 0 )) {
	    call rft_get_fits_string (card, FITSTYPE(fits), LEN_CARD)
#	    if (GCOUNT(fits) >= 1 && gkey != TO_MG)
#	       call putline (fd_usr, card)
	} else if (strmatch (card, "^TARGNAME") != 0) {
	    call rft_get_fits_string (card, OBJECT(fits), LEN_CARD)
#	    if ((GCOUNT(fits) == -1) && (OPSIZE(fits) == -1))
	       call putline (fd_usr, card)
	} else if ((strmatch (card, "^IRAFNAME") != 0) ||
	           (strmatch (card, "^FILENAME") != 0 )) {
	    call rft_get_fits_string (card, IRAFNAME(fits), LEN_CARD)
	    call strlwr (IRAFNAME(fits))
	} else if (strmatch (card, "^ORIGIN  ") != 0) {
	    if (origin == NO)  # don'take the first one if more than one
	       call putline (fd_usr, card)
	    origin = NO
	} else if (strmatch (card, "^OPSIZE  ") !=  0) {
	    # Save if we want to create a multigroup image
	    if (gkey == TO_MG)
	       nchar = ctoi (card, icol, OPSIZE(fits))
	} else if (strmatch (card, "^FITSDATE") !=  0) {
	       # dont put in image header
	} else if (strmatch (card, "^DATE    ") !=  0) {
	    if (date == YES)
	       call rft_get_fits_string (card, DATE(fits), LEN_CARD)
	    call putline (fd_usr, card)
	    date = NO
	} else if (strmatch (card, "^CRVAL") != 0) {
	    k = strmatch (card, "^CRVAL")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECRVAL)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, icol, dval)
	    CRVAL(pn) = dval
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^CRPIX") != 0) {
	    k = strmatch (card, "^CRPIX")

	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECRPIX)
	    if (j <=0) j=1
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, icol, dval)
	    CRPIX(pn) = dval
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^CDELT") != 0) {
	    k = strmatch (card, "^CDELT")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECDELT)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, icol, dval)
	    CDELT(pn) = dval
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^CROTA") != 0) {
	    k = strmatch (card, "^CROTA")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECROTA)
	    pn = WCS_PDES(fits,j)
	    nchar = ctod (card, icol, dval)
	    CROTA(pn) = dval
	    call putline (fd_usr, card)
	} else if (strmatch (card, "^CTYPE") != 0) {
	    k = strmatch (card, "^CTYPE")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECTYPE)
	    pn = WCS_PDES(fits,j)
	    call rft_get_fits_string (card, CTYPE(pn), SZ_OBJECT)
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else if (strmatch (card, "^DATATYPE") != 0) {
	    call strcpy ("HISTORY   ", str, LEN_CARD)
	    call strcat (card, str, LEN_CARD)
	    call strcpy (str, card, LEN_CARD)
	    card[LEN_CARD+1] = '\n'
	    card[LEN_CARD+2] = EOS
	    call putline (fd_usr, card)
	} else if (patmatch (card, cdpat) != 0) {
	    k = strmatch (card, "^CD")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECD1)
	    pn = WCS_PDES(fits,j)
	    k = strmatch (card, "^CD?_")
	    nchar = ctoi (card, k, j)
	    j = check_index (j, ECD2)
	    nchar = ctod (card, icol, dval)
	    CDMATRIX(pn,j) = dval
	    MAKE_CD(fits) = NO
	    if (gkey != DEF_GPB || ndim == 0)
	       call putline (fd_usr, card)
	} else {
            if (strlen(card) != 81) {    # Verify that we have 80 character
					 # records (80+nl)
	       call eprintf(
                "\n ERROR: input FITS keyword has a non_FITS value\n")
	       call eprintf("Old card:\n(")
	       do k = 1, LEN_CARD{
		  call eprintf("%c")
		   call pargc(card[k])
		  if (!IS_PRINT(card[k]))
		     card[k] = ' '
	       }
	       call eprintf(")\nNew card:\n(%s)\n")
		  call pargstr(card)
            }

	    call putline (fd_usr, card)
	}
	return (NO)
end

int procedure check_index (ind, what)

int	ind
int	what

char	line[LEN_CARD]

begin

	switch (what) {
	case ENAXIS:
	  if (ind < 0 || ind > IM_MAXDIM)
	     call error (0,"Value of FITS 'NAXIS' out of range")
	case ENAXISN:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS NAXISn index out of range: %d")
	       call pargi(ind)
	     call error (0,line)
	  }
	case ECRVAL:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CRVALn index out of range: %d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECRPIX:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CRVALn index out of range: %d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECDELT:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CRDELTn index out of range: %d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECROTA:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CROTAn index out of range: %d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECTYPE:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CTYPEn index out of range: %d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECD1:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CDn_m index out of range: n=%d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	case ECD2:
	  if (ind <=0 || ind > IM_MAXDIM) {
	     call sprintf(line, LEN_CARD,
	       "Value of FITS CDn_m index out of range: m=%d")
	       call pargi(ind)
	     call eprintf("\n%s\n")
		call pargstr(line)
	     call eprintf("** Changed to 1\n")
	     ind = 1 
	  }
	}

	return(ind)
end
