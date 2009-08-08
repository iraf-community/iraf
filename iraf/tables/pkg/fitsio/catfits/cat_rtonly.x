# Copyright restrictions apply - see tables$copyright.tables 
# 
# Copyright restrictions apply - see stsdas$copyright.stsdas 
# 
include "../stwfits/dfits.h"
include "catf.h"

#READ_TAPE_ONLY -- Procedure to read tape or disk file only without creating
# and output image file or table.

int procedure read_tape_only (fi, fits_fd, fitsfile, number)

pointer	fi
int	fits_fd		    # tape descriptor
char	fitsfile[SZ_FNAME]  # Fits file name
long	number		    # input file number

int	nread, i, stat, enumber, ntab
char	card[LEN_CARD]
int	read_card(), get_tape_info()
long	rft_init_read_pixels()

string	badfits  "%s  ** Unexpected data at end of fits file ** \n"

include	"catfits.com"
include "../stwfits/dfits.com"

begin
	main_header = true
	enumber = EXT_NUMBER(fi)

	if (enumber == -1) {
	    EXT_NUMBER(fi) = 0
	}

	if (short_header == YES) {
	    EXTEND(fi) = NO
	    IRAFNAME(fi) = EOS
	    #
	    # CATV define in catf.h will have the keyword values. So
	    # clear them for every input fits file.
	    #
	    for (i = 1; i <= nkeywords; i=i+1) 
		CATV(fi,i) = EOS

	    BITPIX(fi) = 0 
	    i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	    nread = 0
	    repeat {
		i = read_card (fits_fd, card, nread)
		if ( i == -10) {  # Bad fits file, it's not SIMPLE nor EXTEND
		    call eprintf (badfits)
		    call pargstr(fitsfile)
		    return(0)
		}

		if (i == EOF) {
		    call eprintf ("EOF encountered\n")
		    return (EOF)
		}

		stat = get_tape_info (fi, card)
	    } until (stat == YES)   # stat == YES if END card encountered.
	    
	    # Print short header
	    #
	    if (EXT_NUMBER(fi) == 0 || enumber == -1) {
		call cat_print_main (number, fitsfile, fi)
		if (enumber != -1) 
		    return(0)
	    }

	    if (EXTEND(fi) != NO && print_ext == YES) {	# read extension
		ntab = 1
		repeat {	   # all the extensions in the file
		    EXT_NUMBER(fi) = ntab
		    EXTVER(fi) = INDEFI
		    IRAFNAME(fi) = EOS

		    call skip_data (fits_fd, fi)       # skip extension data
		    main_header = false

		    # Clear previous keywords values.
		    BITPIX(fi) = 0 
		    for (i = 1; i <= nkeywords; i=i+1) 
			CATV(fi,i) = EOS

		    # Get ready to read extension header.
		    i = rft_init_read_pixels (len_record, FITS_BYTE, 
					      LSBF, TY_CHAR)

		    nread = 0
		    repeat {
			i = read_card (fits_fd, card, nread)
			if (i == -10) {
			    call eprintf (badfits)
			    call pargstr(fitsfile)
			    return(0)
			}

			if (i == EOF) {
			    if (nread == 0) {
				return (0)
			    } else {
				call eprintf ("EOF encountered\n")
				return (EOF)
			    }
			}

			stat = get_tape_info (fi, card)
		    } until (stat == YES)

		    if (enumber == -1 || enumber == ntab) 
			call cat_print_ext (fi)

		    if (enumber == ntab)
			return(0)
		    ntab = ntab + 1

		} until (i == EOF) 	
	    }

	} else if (long_header == YES) { 
	    EXTEND(fi) = NO

	    call printf("\n*** File: %s\n")
	    call pargstr(fitsfile)

	    i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	    nread = 0
	    repeat {
		i = read_card (fits_fd, card, nread)
		if ( i == -10) {
		    call eprintf (badfits)
		    call pargstr(fitsfile)
		    return (0)
		}

		if (i == EOF) {
		    call eprintf ("EOF encountered\n")
		    return (EOF)
		}

		stat = get_tape_info (fi, card)

		if (enumber == -1 || enumber == 0) {
		    call printf ("%s\n")
		    call pargstr (card)
		}
	    } until (stat == YES)   # stat == YES if END card encountered.

	    if (enumber == 0) 
		return(0)

	    if (EXTEND(fi) != NO) {	# read extension
		ntab = 1
		repeat {	   # all the extension in the file
		    EXT_NUMBER(fi) = ntab
		    call skip_data (fits_fd, fi) # skip table data
		    main_header = false
		    i = rft_init_read_pixels (len_record, FITS_BYTE, 
					      LSBF, TY_CHAR)
		    nread = 0
		    repeat {
			i = read_card (fits_fd, card, nread)
			if (i == -10) {
			    call eprintf (badfits)
			    call pargstr(fitsfile)
			    return(0)
			}

			if (i == EOF) {
			    if (nread == 0) {
				return (0)
			    } else {
				call eprintf ("EOF encountered\n")
				return (EOF)
			    }
			}

			stat = get_tape_info (fi, card)

			if (enumber == -1 || enumber == ntab) {
			    call printf ("%s\n")
			    call pargstr (card)
			}
		    } until (stat == YES)

		    if (enumber == ntab)
			return(0)
		    ntab = ntab + 1

		} until (i == EOF) 	
	    }   
     }

    return (0)
end

#READ_CARD -- Procedure to read one fits card from the input tape or
#	      disk file.
int procedure read_card (fits_fd, card, nread)

int	fits_fd		# tape descriptor
char	card[LEN_CARD]	
int	nread		# card counter

size_t	sz_val
size_t	c_1
long	nrec, i
bool	isblank()
long	rft_read_pixels()
int	strmatch()

begin
	   c_1 = 1
	   sz_val = LEN_CARD
	   i = rft_read_pixels (fits_fd, card, sz_val, nrec, c_1)
	   if (i == EOF && nread == 0)  {		# At EOT
		return (EOF)
	   } else if (nread == 0 && isblank (card)) {
		return (EOF)
	   } else if (nread == 0 && ( strmatch (card, "^SIMPLE  ") == 0 &&
		  strmatch (card, "^XTENSION") == 0))  {
	        return (-10)
	   } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	   } else
	     nread = nread + 1

	   return (0)
end

include <mach.h>
include <fset.h>
define  NB_DOUBLE   64
# SKIP_DATA --  Precedure to skip over the image data in case the 
# flag make_image is set to NO.

procedure skip_data (fits_fd, fits)

int	fits_fd		# FITS file descriptor
pointer	fits		# FITS data structure

size_t	npix_record
int	i, pixsize, bitpix
long	blksize, nlines, gc, pc, off
long	l_val

# totpix is the number of SPP char in the data portion, rounded up to a
# multiple of 2880/SZB_CHAR.
long	totpix

# bytes_in_data is the number of bytes in the data portion, NOT rounded up
# to a multiple of 2880.
long	bytes_in_data

# bytes_in_par is the number of bytes in the random groups parameter section,
# NOT rounded up to a multiple of 2880.
long	bytes_in_par

long	rft_init_read_pixels(), fstatl(), note(), lmod()
errchk	rft_init_read_pixels, rft_read_pixels

include "catfits.com"

begin
	if (NAXIS(fits) == 0) {
	    return
	}
 
	nlines = 1
	do i = 2, NAXIS(fits)
	    nlines = nlines * NAXISN(fits, i)

	# The involved calculation of pixsize is to prevent
	# integer overflow for large values of totpix

	if (NAXISN(fits,1) > 0)		# would be zero for random groups format
	    totpix = NAXISN(fits,1) * nlines
	else
	    totpix = nlines
	bytes_in_data = totpix * (iabs (BITPIX(fits)) / NBITS_BYTE)
	if (iabs (BITPIX(fits)) < (SZB_CHAR * NBITS_BYTE)) {
	    pixsize = (SZB_CHAR * NBITS_BYTE) / iabs(BITPIX(fits))
	    totpix = totpix / pixsize
	} else {
	    pixsize = iabs(BITPIX(fits)) / (SZB_CHAR * NBITS_BYTE)
	    totpix = totpix * pixsize
	}

	# Set in multiple of 1440
	totpix = ((totpix + 1439)/1440) * 1440

	# FITS data is converted to type  LONG.  If BITPIX is not one
	# of the MII types then rft_read_pixels returns an ERROR.

	bitpix = iabs (BITPIX(fits))

	npix_record = len_record * FITS_BYTE / bitpix
	i = rft_init_read_pixels (npix_record, bitpix, LSBF, TY_LONG)

	blksize = fstatl (fits_fd, F_SZBBLK)
	l_val = 2880
	if (lmod (blksize, l_val) == 0)
	    blksize = blksize / 2880
	else
	    blksize = 1

        gc = GCOUNT(fits)
        pc = PCOUNT(fits)
        if (pc > 0) {
	    if (main_header) {
		bytes_in_par = pc * (iabs (BITPIX(fits)) / NBITS_BYTE)
		off = note (fits_fd)
		off = off + (gc * (bytes_in_par + bytes_in_data) + 2879) /
			2880 * (2880 / SZB_CHAR)
		call seek (fits_fd, off)
	    } else {
		# We get here for the case of an extension with pcount > 0,
		# e.g. a bintable with variable-length arrays.  Note that
		# pcount does not have to be an even number of bytes.
		off = note (fits_fd)
		off = off +
			(bytes_in_data + pc + 2879) / 2880 * (2880 / SZB_CHAR)
		call seek (fits_fd, off)
	    }
	} else {
	    # pcount = 0
	    off = note (fits_fd)
	    off = off + totpix
	    call seek (fits_fd, off)
	}

end

# GET_TAPE_INFO -- Procedure to obtain minimum information from
# the fits tape to be output as short header.

int procedure get_tape_info (fits, card)

pointer	fits		# FITS data structure
char	card[ARB]	# FITS card

int	len
char    cval, ext_value[SZ_OBJECT]
int	nk, nchar, i, k, j
int	strmatch(), strncmp(), strlen()
int	cctoc(), ctoi(), ctol(), strcmp()

include "../stwfits/dfits.com"
include "catfits.com"

begin
	i = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	   return(YES)
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	   nchar = ctoi (card, i, BITPIX(fits))	
	   DATATYPE(fits) = EOS
	} else if (strmatch (card, "^FILENAME") != 0 && EXT_NUMBER(fits)== 0) {
	   call rft_get_fits_string (card, IRAFNAME(fits), SZ_OBJECT)
	} else if (strmatch (card, "^IRAFNAME") != 0) {
	   call rft_get_fits_string (card, IRAFNAME(fits), SZ_OBJECT)
	} else if (strmatch (card, "^ODATTYPE") != 0) {
	   call rft_get_fits_string (card, ext_value, SZ_OBJECT)
	   DATATYPE(fits) = ext_value[1]		# Get 1st character
	   # nk hasn't been assigned a value; CATV not necessarily allocated:
	   # call rft_get_fits_string (card, CATV(fits,nk), SZ_OBJECT)
	   # DATATYPE(fits) = CATV(fits,nk)    # Get 1st character
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	   nchar = ctoi (card, i, NAXIS(fits))	
	} else if (strmatch (card, "^EXTNAME ") != 0 && EXT_NUMBER(fits) > 0) {
	   call rft_get_fits_string (card, IRAFNAME(fits), SZ_OBJECT)
        } else if (strmatch (card, "^EXTVER  ") != 0) {
	   nchar = ctoi (card, i, EXTVER(fits))
	   if (main_header)
	      EXTVER(fits) = INDEFI
        } else if (strmatch (card, "^GCOUNT ") != 0) {
	   nchar = ctoi (card, i, GCOUNT(fits))
	   #if (main_header)
	   #   GCOUNT(fits) = 0
        } else if (strmatch (card, "^PCOUNT ") != 0) {
	   nchar = ctol (card, i, PCOUNT(fits))
	   #if (main_header)
	   #   PCOUNT(fits) = 0
        } else if (strmatch (card, "^TFIELDS ") != 0) {
	   nchar = ctoi (card, i, NCOLS(fits))
        } else if (strmatch (card, "^NAXIS") != 0) {
	   k = strmatch (card, "^NAXIS")
	   nchar = ctoi (card, k, j)
	   nchar = ctol (card, i, NAXISN(fits, j))
	} else if (strmatch (card, "^XTENSION") != 0) {
	   call rft_get_fits_string (card, ext_value, SZ_OBJECT)
	      if (strcmp (ext_value, "TABLE") == 0 )
	         XTENSION(fits) = TABLE
	      else if (strcmp (ext_value, "BINTABLE") == 0 )
	         XTENSION(fits) = BINTABLE
	      else if (strcmp (ext_value, "IMAGE") == 0 )
	         XTENSION(fits) = IMAGE
	      else
	         XTENSION(fits) = OTHER
	} else if (strmatch (card, "^EXTEND  ") != 0) {
	   nchar = cctoc (card, i, cval)
	   if (cval == 'T')
	      EXTEND(fits) = YES
	} else if (short_header == YES)
	   do nk = 1, nkeywords {
	      len = strlen(Memc[key_table[nk]]) 
	      if (strncmp (Memc[key_table[nk]], card, len) == 0) {
	         call get_kval (card, CATV(fits,nk), SZ_OBJECT)
	         return(NO)
	      }
	   }
	return(NO)
end


# GET_KVAY -- Extract the value of FITS header card. A string value
# will nto have quotes around it.

procedure get_kval (card, str, maxchar)

char	card[LEN_CARD]		# FITS card
char	str[LEN_CARD]		# FITS string
int	maxchar			# maximum number of characters

int	j, istart, nchar
bool    quote

begin
	# Check for opening quote
	quote = false
	for (istart = COL_VALUE; istart <= LEN_CARD && card[istart] == ' ';
	    istart = istart + 1)
	    ;
	if (card[istart] == '\'') {    # skip initial quote
	   istart = istart + 1
	   quote = true
	}
	if (!quote)
	   for (j = istart; (j<LEN_CARD)&&(card[j]!=' ') &&
	       ( card[j]!='/'); j = j + 1)
	       ;
	else 
	   for (j = istart;  (j<LEN_CARD)&&(card[j]!='\'' ); j = j + 1)
	       ;
	if (j<LEN_CARD) j = j - 1
	nchar = min (maxchar, j - istart + 1)

	# copy string
	if (nchar <= 0)
	    str[1] = EOS
	else {
	    call strcpy (card[istart], str, nchar)
	}
end


