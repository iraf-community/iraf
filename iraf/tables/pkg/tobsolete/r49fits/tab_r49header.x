include <imhdr.h>
include <imio.h>
include <tbset.h>
include <mach.h>
include "rfits.h"

define  TBC_DTYPE  tbcol     # Redefine name in tab.com
			     # to be use with 3dtables.
# TAB_READ_HEADER -- Read a FITS header for a table extension.
# EOT is detected by an EOF on the first read and EOF is returned to the calling
# routine.  Any error is passed to the calling routine.

int procedure tab_read_header (fits_fd, im, ext, tp)

int	fits_fd			# FITS file descriptor
pointer im			# Image descriptor
pointer	ext			# Extension data structure
pointer	tp			# IRAF table descriptor

int	i, stat, ind
char	card[LEN_CARD+1]

int	rft_init_read_pixels(), rft_read_pixels()
int	tab_decode_card(), strncmp(), strmatch()
int	rec_count

errchk	rft_read_pixels
errchk	stropen, close

include "rfits.com"
include "tab.com"

begin
	card[LEN_CARD + 1] = '\n'
	card[LEN_CARD + 2] = EOS
	

	# Initialization
	maxlen = 0
#	BLANKS(fits) = NO
#	BLANK_VALUE(fits) = INDEFL
	if (gkey != TO_MG)
           EXTNAME(ext) = EOS
	EXT_TTYPE(ext) = SDAS_TABLE
	
	BIN_DTYNSP = false	        # Temporary flag to note the presence
				# of input binary table with multiple
				# elements per table cell.
	rec_count = 0
	# Header is character data in FITS_BYTE form
	i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	i = rft_read_pixels (fits_fd, card, LEN_CARD, rec_count, 1)

	if (i == EOF) 		# At EOT
	   return (EOF)
	ext_type = TABLE
	if (strmatch (card, "^XTENSION") != 0) {
	   if (strncmp( card[12], "IMAGE", 5) == 0) {
	      ext_type = IMAGE
	      return (IMAGE)
	   } else if ( strncmp( card[12], "BINTABLE", 8) == 0)
	      ext_type = BINTABLE
	   else if (strncmp( card[12], "TABLE", 5) != 0) 
	        call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else {
	   return (EOF)
	}
	# Loop until the END card is encountered
	repeat {
	    i = rft_read_pixels (fits_fd, card, LEN_CARD, rec_count, 1)

	    if (i == EOF) {	# At EOT
		return (EOF)
	    } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    }

	    # Print FITS card images if long_header option specified
	    ind = strncmp (card, "        ", 8)
	    if (long_header == YES && ind != 0) {
		call printf ("%s")
		    call pargstr (card)
	    }
	    if (ind != 0)
	       stat = tab_decode_card (im, ext, tp, card)

	} until (stat == YES)

	return (stat)
end

define MAX_UPARM 50      # define max number of user parameter for a buffer
define LEN_CARD1 81

# RTB_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in fits.h.

int procedure tab_decode_card (im, ext, tp, card)

pointer im		# Image descriptor
pointer	ext		# Extension data structure
pointer	tp		# IRAF table descriptor
char	card[LEN_CARD]	# FITS card

pointer colptr, pc,pp
char	ftnfmt[SZ_COLFMT], pfmt[SZ_COLFMT], extn[SZ_EXTN]
pointer	ppar
int	nchar, ival, dtype, upar, ioff, mtsize
int	icc, j, k, jc, tnaxis, npar, ncoln, len, icol, kcol
int	strmatch(), ctoi(), ctol(), ctor(), strncmp(), strcmp(), fnextn()

include	"rfits.com"
include "tab.com"
data	upar /NO/
data    ppar /NULL/

begin
	icc = COL_VALUE
	if (strmatch (card, "^END     ") != 0) {
	   if (strcmp (EXT_TTYPE(EXT), "TRL") != 0) {
	      # define the last column
	      if (gkey != TO_MG) {
	         call tbcdef (tp, colptr, colname, colunits, colfmt,
			   datat, lendata, 1)
	         call tbpset (tp, TBL_MAXPAR, npar+5)
	         call tbtcre (tp)
	         if (upar == YES) {  
	            # now write the user parameters to the table
	            call ftb_put_upar (tp, npar, Memc[ppar])
	            upar = NO
	            call mfree(ppar, TY_CHAR)
	         }
	      } else {
	          if (wf2_49 == 49)
	             call gi_pdes (im, colname, datat, lendata, kcol)
		  else
	             call gi_pdes (im, colname, datat, lendata, ncoln)
	          if (upar == YES) {
	             call gi_gcomm (im, npar, Memc[ppar])
		     upar = NO
	             call mfree(ppar, TY_CHAR)
	          }
	      }
	   }
	   return(YES)
	} else if (strmatch (card, "^XTENSION") != 0) {
	    if (strncmp( card[icc+1], "TABLE", 5) != 0)
	      call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    nchar = ctoi (card, icc, EXT_BITPIX(ext))
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    nchar = ctoi (card, icc, tnaxis)
	    if (tnaxis > 2) 
		call error (5, "RTB_DECODE_CARD: FITS table NAXIS too large")
	    coln = 1			# init column index
	} else if (strmatch (card, "^NAXIS") != 0) {
#	    call strcpy("  ", DATE(fits), LEN_CARD)
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    if (j == 1 )
	       nchar = ctol (card, icc, EXT_ROWLEN(ext))
	    else
	       nchar = ctol (card, icc, EXT_NROWS(ext))
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	    nchar = ctoi (card, icc, ival)
	    if (ival != 0) 
	call error (6, "FITS table header has PCOUNT not zero. (Not supported)")
	} else if (strmatch (card, "^GCOUNT ") != 0) {
	    nchar = ctoi (card, icc, ival)
	    if (ival > 1)
		call eprintf ("Warning: FITS can only read one group per table")
	} else if (strmatch (card, "^TFIELDS ") != 0) {
	    nchar = ctoi (card, icc, ival)
	    if (gkey != TO_MG) {
	       # set the number of columns
	       call tbpset (tp, TBL_MAXCOLS, ival)    
	       # initialize defaults values
	    } else {
	       # The number of fields (or columns) in the table is the 
	       # number of parameter for the new GEIS file.
	       if (wf2_49 == 49 && ival == 49) {
		  ival = 46
	       }
	       call gi_pstfval (im, "PCOUNT", ival)
	       if (ival > 0)
		  # Realloc space needed for the stf descriptor
		  call gi_realloc (im)
	    }
	    if (ival > 0) {
	       if (wf2_49 == 49 && ival == 46) {
		  k = 49
	       } else
                  k = ival
	       call calloc (EXT_PCOL(ext), k, TY_INT)
	       call calloc (EXT_PCW(ext), k, TY_INT)
	       call calloc (EXT_PZERO(ext), k, TY_REAL)
	       call malloc (EXT_PSCAL(ext), k, TY_REAL)
	       call amovkr (1.0, Memr[EXT_PSCAL(ext)], k)
	       call calloc (EXT_PNULL(ext), k*SZ_COLUNITS, TY_CHAR)
	    }
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    # Do not overwrite if
	    if (gkey != TO_MG)
	       call rft_get_fits_string (card, EXTNAME(ext), LEN_CARD)
	    k = fnextn (EXTNAME(ext), extn, SZ_EXTN)
	    call strlwr(extn)
	    if (strcmp (extn, "trl") == 0)
	       EXT_TTYPE(ext) = TRAILER_FILE
	    else if (strcmp (extn, "txt") == 0)
	       EXT_TTYPE(ext) = TXT_FILE
#	} else if (strmatch (card, "^DATE    ") != 0) {
#	    call rft_get_fits_string (card, DATE(fits), LEN_CARD)
	} else if (strmatch (card, "^TTYPE" ) != 0) {
	    k = strmatch (card, "^TTYPE")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       if (gkey != TO_MG) {
	          # define previous column
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else {
		  if (coln == 1) {
		    icol = 1
		    kcol = 1
	          } else
		    icol = icol + 1
		  if (wf2_49 != 49 )
	             call gi_pdes (im, colname, datat, lendata,coln )
		  else if (ncoln < 45 || mod(icol,2) == 1)
	             call gi_pdes (im, colname, datat, lendata, kcol)
#		  else if (ncoln < 45 || mod(icol,2) == 1) {
#	             call gi_pdes (im, colname, datat, lendata, kcol)
#		  } else {
#			call eprintf("\n")
#		  }
	       }
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, colname, SZ_COLNAME)
	} else if (strmatch (card, "^TBCOL" ) != 0) {
	    pp = EXT_PCOL(ext)
	    k = strmatch (card, "^TBCOL")
	    nchar = ctoi (card, k, jc)
#	    nchar = ctoi (card, icc, tbcol[jc])
	    nchar = ctoi (card, icc, Memi[pp+jc-1])
	} else if (strmatch (card, "^TFORM" ) != 0) {
	    k = strmatch (card, "^TFORM")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       if (gkey != TO_MG) {
	          # define previous column
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else
	          call gi_pdes (im, colname, datat, lendata, coln)
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, ftnfmt, SZ_COLFMT)
	    pc = EXT_PCW(ext)
	    if (ext_type == TABLE) {
#	       call tbgtyp (ftnfmt, datat, tbcw[coln])
	       call tbgtyp (ftnfmt, datat, Memi[pc+coln-1])
	       call tbbaln (datat, dtype, lendata)
	       call tbbftp (ftnfmt, colfmt)
	       if (datat < 0) {		# Change format to left justified text
		   call strcpy ("%-", pfmt, SZ_COLFMT)
		   call strcat (colfmt[2], pfmt, SZ_COLFMT)
		   call strcpy (pfmt, colfmt, SZ_COLFMT)
	       }
	    }else { # is BINTABLE
	       pp = EXT_PCOL(ext)  # Use this pointer (TBCOL) to indicate a
				   # BYTE datatype.
	       call tbg3dtyp (ftnfmt, datat, lendata)
#	       TBC_DTYPE[coln] = datat
	       Memi[pp+coln-1] = 0
	       if (datat == TY_UBYTE) {
		  datat = TY_SHORT 
		  Memi[pp+coln-1] = 1
	       }
	       Memi[pc+coln-1] = datat
	       if (lendata < 0) lendata = 1	  # One element per char col.
	       call tbbaln (datat, dtype, len)
	       colfmt[1] = EOS
	    }
	} else if (strmatch (card, "^TUNIT" ) != 0) {
	    k = strmatch (card, "^TUNIT")
	    nchar = ctoi (card, k, ncoln)	# possible new column number
	    if (ncoln != coln) {
	       # define previous column
	       if (gkey != TO_MG) {
	          call tbcdef (tp, colptr, colname, colunits, colfmt,
			    datat, lendata, 1)
	          colunits[1] = EOS
	       } else
	          call gi_pdes (im, colname, datat, lendata, coln)
	       coln = ncoln
	    }
	    call rft_get_fits_string (card, colunits, SZ_COLUNITS)
	    # Logical values has been encoded as character in the fits
	    # table. The values are 'T' and 'F'.
	    if (ext_type == TABLE && 
		 strncmp (colunits, "LOGICAL-", 8) == 0) {
	       lendata = -datat
	       colfmt[1] = '%'
	       call sprintf(colfmt[2], SZ_COLFMT, "%db")
		    call pargi(lendata)
	       datat = TY_BOOL
	       call strcpy (colunits[9], colunits, SZ_COLUNITS)
	    }
	} else if (strmatch (card, "^TNULL" ) != 0) {
	    pp = EXT_PNULL(ext)
	    k = strmatch (card, "^TNULL")
	    nchar = ctoi (card, k, jc)
#	    call rft_get_fits_string (card, tnull[1,jc], SZ_COLFMT)
	    call get_null_string (card, Memc[pp+(jc-1)*SZ_COLUNITS],
				 SZ_COLUNITS)
	} else if (strmatch (card, "^TZERO" ) != 0) {
	    pp = EXT_PZERO(ext)
	    k = strmatch (card, "^TZERO")
	    nchar = ctoi (card, k, jc)
#	    nchar = ctor (card, icc, tzero[jc])
	    nchar = ctor (card, icc, Memr[pp+jc-1])
	    # change datatype to real if 'datat' is int.
	    if (Memr[pp+jc-1] != 0.0 && datat == TY_INT) {
	       datat = TY_REAL
	       call strcpy ("%-15.7g", colfmt, SZ_COLFMT)
	    }
	} else if (strmatch (card, "^TSCAL" ) != 0) {
	    pp = EXT_PSCAL(ext)
	    k = strmatch (card, "^TSCAL")
	    nchar = ctoi (card, k, jc)
#	    nchar = ctor (card, icc, tscal[jc])
	    nchar = ctor (card, icc, Memr[pp+jc-1])
	    # change datatype to real if 'datat' is int.
	    if (Memr[pp+jc-1] != 1.0 && datat == TY_INT){
	       datat = TY_REAL
	       call strcpy ("%-15.7g", colfmt, SZ_COLFMT)
	    }
	} else {
	    # Allow storage for user parameters
	    if (upar == NO) {
	       upar = YES
	       if (ppar != NULL)
		  call mfree (ppar, TY_CHAR)
	       mtsize = (LEN_CARD+1)*MAX_UPARM
	       call malloc (ppar, mtsize, TY_CHAR)
	       ioff = 0
	       npar = 0
	    }
	    # Keep user parameters in a buffer until END
	    call amovc (card, Memc[ppar+ioff], LEN_CARD)	# copy EOS also
	    ioff = ioff + LEN_CARD + 1
	    Memc[ppar+ioff-1] = EOS
	    npar = npar + 1
	    if (npar >= mtsize/(LEN_CARD+1)) {    # increase no. of cards by 10
	       mtsize = mtsize + (LEN_CARD+1)*50
	       call realloc(ppar, mtsize, TY_CHAR)
	    }
	}
	return (NO)
end

# FTB_PUT_UPAR -- Procedure to write user parameters to the table 
# already created.

procedure ftb_put_upar (tp, npar, uparbuf)

pointer tp			     # i: table descriptor
char	uparbuf[LEN_CARD, npar]       # i: buffer with user pars
int	npar			     # I: number of parameters read

char	keyword[SZ_KEYWORD], sval[LEN_CARD]
char 	card[LEN_CARD], squo, cht, chn, dot, blkn
int	i, k, sscan(), nscan(), stridx(), stat, strmatch()
double  dval
int	bval, ival, iparn

begin
	blkn = ' '
	squo = '\''
	cht = 'T'
	chn = 'F'
	dot = '.'
	do i = 1, npar {
	    do k = 1, 8 {
	       if (uparbuf[k,i] == blkn) {
	          keyword[k] = EOS
	          break
	       }
	       keyword[k] = uparbuf[k,i]
	    }
	    keyword[SZ_KEYWORD+1] = EOS
	    call strcpy (uparbuf[10,i], card, LEN_CARD)
	    if (stridx (squo, uparbuf[1,i]) == 11) {           # is a string
	       call rft_get_fits_string (uparbuf[1,i], sval, LEN_CARD)
	       call tbhadt (tp, keyword, sval)
	    } else if (strmatch(keyword, "^HISTORY") != 0 ) {
	       call strcpy (uparbuf[10,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "HISTORY", sval)
	    } else if (strmatch(keyword, "^COMMENT") != 0 ) {
	       call strcpy (uparbuf[10,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "COMMENT", sval)
	    } else if (strmatch(card, "^                    T ") != 0 ) {
	       bval = YES	
	       call tbhadb (tp, keyword, bval)
	    } else if (strmatch(card, "^                    F ") != 0 ) {
	       bval = NO
	       call tbhadb (tp, keyword, bval)
	    } else {                   # is a number
	       stat = sscan(card)
		    call gargd(dval)
	       if (nscan() < 1) {
	          call strcpy (uparbuf[1,i], card, LEN_CARD)
		  # append card regardless of content or keyword
	          call tbhanp (tp, keyword, 't', card[9], iparn)
	       } else {
		  if (stridx(dot, card) == 0) {
	             stat = sscan(card)
		        call gargi(ival)
		     call tbhadi (tp, keyword, ival)
		  } else
	             call tbhadd (tp, keyword, dval)
	       }
	    }
	
	}
end

# TBG3dTYPE -- Get datatype  from 3d_table fits files

procedure tbg3dtyp (ftnfmt, datatyp, nelem)

char	ftnfmt[SZ_COLFMT]	# i: fortran format specification
int	datatyp			# o: data type expressed as an int
int	nelem			# 0: number of elements in a column
#--
int	ctoi(), nchar, len, ipos, strlen()

include "tab.com"

begin
	call strlwr (ftnfmt)
	len = strlen(ftnfmt)

	ipos = 1
	nchar = ctoi (ftnfmt, ipos, nelem)
        if (nelem == 0)
	   nelem = 1

	# At this release (NOV 91) there is no support
	# yet for binary table with nelem > 1
	# We will skip the table data if this happens.
	IF (nelem > 1)
	   BIN_DTYNSP = true
	if (ftnfmt[len] == 'l') {
	    datatyp = TY_BOOL
	    nchar = nelem
#	} else if (ftnfmt[len] == 'x') {
#	    datatyp = TY_BITARR
	} else if (ftnfmt[len] == 'i') {
	    datatyp = TY_SHORT
	    nchar = nelem*SZ_SHORT*SZB_CHAR
	} else if (ftnfmt[len] == 'j') {
	    datatyp = TY_INT
	    nchar = nelem*SZ_INT*SZB_CHAR
	} else if (ftnfmt[len] == 'e') {
	    datatyp = TY_REAL
	    nchar = nelem*SZ_REAL*SZB_CHAR
	} else if (ftnfmt[len] == 'b') {    # BYTE datatype
	    datatyp = TY_UBYTE
	    nchar = nelem
	} else if (ftnfmt[len] == 'd') {
	    datatyp = TY_DOUBLE
	    nchar = nelem*SZ_DOUBLE*SZB_CHAR
	} else if (ftnfmt[len] == 'a') {
	    nchar = nelem + 1                   # +1 for the EOS
	    datatyp = -nelem			# NOTE:  not an SPP data type
	    nelem = 1
	   BIN_DTYNSP = false    # except for characters, where is legal
				 # to have 12a for example.
	} else {
	    call eprintf ("\n** Bintable datatype not supported. ")
	    call eprintf ("Will skip file\n")
	}
	# Maxlen is the maximun number of characters in a column containing
	# an array. It is initialized in tab_wheader.
	maxlen = max (maxlen, nelem*nchar)
end


# TBGTYPE -- Get datatype and field width from the format specification. 
# Notice that datatype for character format is not spp standard.

procedure tbgtyp (ftnfmt, datatyp, width)

char	ftnfmt[SZ_COLFMT]	# i: fortran format specification
int	datatyp			# o: data type expressed as an int
int	width			# 0: field width in character (TBFORM value)
#--
int	ctoi(), nchar, ipos

begin
	call strlwr (ftnfmt)

	ipos = 2
	nchar = ctoi (ftnfmt, ipos, width)

	if (ftnfmt[1] == 'e') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'g') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'f') {
	    datatyp = TY_REAL
	} else if (ftnfmt[1] == 'd') {
	    datatyp = TY_DOUBLE
	} else if (ftnfmt[1] == 'i') {
	    datatyp = TY_INT
	} else if (ftnfmt[1] == 'b') {
	    datatyp = TY_BOOL
	} else if (ftnfmt[1] == 'a') {
	    datatyp = -width			# NOTE:  not an SPP data type
	} else {
	    call error (5,"table datatype not supported")
	}
end


procedure trimh (card)

char card[LEN_CARD]

int	i , strlen()

begin
	for (i=strlen(card); 
	     i > 1  && (card[i] == ' ' || card[i] == '\n');
	     i=i-1)
	     ;

	     card[i+1] = EOS
	     
end
# GET_NULL_STRING -- Get null string with all the significant 
#		     trailing blanks.
procedure  get_null_string (card, str, maxchar)

char    card[LEN_CARD]          # FITS card
char    str[LEN_CARD]           # FITS string
int     maxchar                 # maximum number of characters

int     j, istart, nchar

begin
	# Check for opening quote
	for (istart = COL_VALUE; istart <= LEN_CARD && card[istart] != '\'';
		istart = istart + 1)
		;
	istart = istart + 1
	# closing quote
	for (j = istart; (j<LEN_CARD)&&(card[j]!='\''); j = j + 1)
		;
	nchar = min (maxchar, j - istart)
	# copy string
	if (nchar <= 0)
	   str[1] = EOS
	else
	   call strcpy (card[istart], str, nchar)
end

