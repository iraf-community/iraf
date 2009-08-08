include <imhdr.h>
include <imio.h>
include <tbset.h>
include <mach.h>
include "rfits.h"

# TAB_READ_HEADER -- Read a FITS header for a table extension.
# EOT is detected by an EOF on the first read and EOF is returned to the 
# calling routine.  Any error is passed to the calling routine.

int procedure tab_read_header (fits_fd, im, ext, tp, fits)

int	fits_fd			# FITS file descriptor
pointer im			# Image descriptor
pointer	ext			# Extension data structure
pointer	tp			# IRAF table descriptor
pointer fits			# descriptor holding fits file info

size_t	sz_val, c_1
long	i, rec_count
int	ext_type, stat
char	card[LEN_CARD+1]

long	rft_init_read_pixels(), rft_read_pixels()
int	tab_decode_card(), strncmp(), strmatch()

errchk	rft_read_pixels
errchk	stropen, close

include "rfits.com"

begin
	c_1 = 1

	card[LEN_CARD + 1] = '\n'
	card[LEN_CARD + 2] = EOS

	# Initialization
	BIN_MAXLEN(ext) = 0
	if (gkey != TO_MG)
           EXTNAME(ext) = EOS
	TAB_TYPE(ext) = SDAS_TABLE
	
	BIN_DTYNSP(ext) = false	        # Temporary flag to note the presence
				# of input binary table with multiple
				# elements per table cell.
	rec_count = 0
	# Header is character data in FITS_BYTE form
	i = rft_init_read_pixels (len_record, FITS_BYTE, LSBF, TY_CHAR)

	sz_val = LEN_CARD
	i = rft_read_pixels (fits_fd, card, sz_val, rec_count, c_1)

	if (i == EOF) 		# At EOT
	   return (EOF)
	FITS_XTEN(fits) = TABLE
	if (strmatch (card, "^XTENSION") != 0) {
	   if (strncmp( card[12], "IMAGE", 5) == 0) {
	      FITS_XTEN(fits) = IMAGE
	      return (IMAGE)
	   } else if ( strncmp( card[12], "BINTABLE", 8) == 0)
	      FITS_XTEN(fits) = BINTABLE
	   else if (strncmp( card[12], "TABLE", 5) != 0) 
	        call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else {
	   return (EOF)
	}
	ext_type = FITS_XTEN(fits)
	# Loop until the END card is encountered
	repeat {
	    sz_val = LEN_CARD
	    i = rft_read_pixels (fits_fd, card, sz_val, rec_count, c_1)

	    if (i == EOF) {	# At EOT
		return (EOF)
	    } else if (i != LEN_CARD) {
	        call error (2, "RFT_READ_HEADER: Error reading FITS header")
	    }

	    # Print FITS card images if long_header option specified
	    if (long_header == YES ) {
		call printf ("%s")
		    call pargstr (card)
	    }
	    stat = tab_decode_card (im, fits, ext, tp, ext_type, card)

	} until (stat == YES)

	return (stat)
end

define MAX_UPARM 50      # define max number of user parameter for a buffer
define LEN_CARD1 81

# RTB_DECODE_CARD -- Decode a FITS card and return YES when the END
# card is encountered.  The keywords understood are given in fits.h.

int procedure tab_decode_card (im, fits, ext, tp, ext_type, card)

pointer im		# Image descriptor
pointer fits		# Descriptor holding fits file info
pointer	ext		# Extension data structure
pointer	tp		# IRAF table descriptor
int	ext_type	# FITS XTENSION type
char	card[LEN_CARD]	# FITS card

size_t	sz_val
pointer	ppar
int	nchar, ival, upar, ioff, mtsize, tindex
int	icc, j, k, tnaxis, npar
long	lval

int	tab_rkval(), tbpsta(), chk_ascname()
int	strmatch(), strncmp(), ctoi(), ctol()

include	"rfits.com"
data	upar /NO/
data    tindex /0/
data    ppar /NULL/

begin
	icc = COL_VALUE
	k = 0
	# See if the have one of the Table column descriptor keywords.
	if (card[1] == 'T')  # let's see if it is.
	   k = tab_rkval (card, ext, ext_type)
	if (k != 0) {
	   tindex = k
	   return(NO)
	}
	if (strmatch (card, "^END     ") != 0) {
	   #make sure we do not have a strange FITS file with TXT_FILE
	   # properties, if so convert it to sdas table.
	   if (TAB_TYPE(ext) == TXT_FILE)
	       if (gkey != TO_MG)
		   if (tbpsta (tp, TBL_MAXCOLS) != 1)
		       TAB_TYPE(ext) = SDAS_TABLE

	    if (TAB_TYPE(ext) == SDAS_TABLE)
	       call tab_crtab (im, tp, ext, upar, ppar, npar, ext_type)

	   tindex = 0
	   return (YES)  # Yes, the END card has been read.
	} else if (strmatch (card, "^XTENSION") != 0) {
	    if (strncmp( card[icc+1], "TABLE", 5) != 0)
	      call error (13, "RTB_DECODE_CARD: Fits extension not supported")
	} else if (strmatch (card, "^BITPIX  ") != 0) {
	    nchar = ctoi (card, icc, EXT_BITPIX(ext))
	    tindex = 0
	} else if (strmatch (card, "^NAXIS   ") != 0) {
	    nchar = ctoi (card, icc, tnaxis)
	    if (tnaxis > 2) 
		call error (5, "RTB_DECODE_CARD: FITS table NAXIS too large")
	} else if (strmatch (card, "^NAXIS") != 0) {
	    k = strmatch (card, "^NAXIS")
	    nchar = ctoi (card, k, j)
	    if ( j == 1 )
	       nchar = ctol (card, icc, EXT_ROWLEN(ext))
	    else
	       nchar = ctol (card, icc, EXT_NROWS(ext))
	} else if (strmatch (card, "^PCOUNT  ") != 0) {
	    nchar = ctol (card, icc, lval)
	    if (lval != 0) 
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
	       call gi_pstfvall (im, "PCOUNT", lval)
	       if (lval > 0)
		  # Realloc space needed for the stf descriptor
		  call gi_realloc (im)
	    }
	    if (ival > 0) {
	       sz_val = ival
	       call calloc (EXT_PBCOL(ext), sz_val, TY_LONG)
	       call calloc (EXT_PCW(ext), sz_val, TY_LONG)
	       call calloc (EXT_PZERO(ext), sz_val, TY_DOUBLE)
	       call malloc (EXT_PSCAL(ext), sz_val, TY_DOUBLE)
	       call amovkd (1.0d0, Memd[EXT_PSCAL(ext)], sz_val)
	       sz_val = ival*(SZ_COLUNITS+1)
	       call calloc (EXT_PNULL(ext), sz_val, TY_CHAR)
	       sz_val = ival*(SZ_COLNAME+1)
	       call calloc (EXT_PTYPE(ext), sz_val, TY_CHAR)
	       sz_val = ival
	       call calloc (EXT_PDTYPE(ext), sz_val, TY_INT)
	       call calloc (EXT_PDSIZE(ext), sz_val, TY_LONG)
	       sz_val = ival*(SZ_COLUNITS+1)
	       call calloc (EXT_PUNIT(ext), sz_val, TY_CHAR)
	       sz_val = ival*(SZ_COLFMT+1)
	       call calloc (EXT_PFORM(ext), sz_val, TY_CHAR)
	       call calloc (EXT_PDISP(ext), sz_val, TY_CHAR)
	    }
	} else if (strmatch (card, "^EXTNAME ") != 0) {
	    # Do not overwrite if
	    if (gkey != TO_MG)
	       call rft_get_fits_string (card, EXTNAME(ext), LEN_CARD)

	    if (chk_ascname (IRAFNAME(fits), EXTNAME(ext)) == YES)
	       TAB_TYPE(ext) = TXT_FILE

	} else {
	    # Allow storage for user parameters
	    if (gkey != TO_MG) {
	       if (card[1] == ' ') {
	          if (tindex == 0 || tindex < tbpsta(tp, TBL_MAXCOLS)) 
		     return(NO)
	       }
	       # Discard the 1st blank card after tindex tables parameters
	       # has been counted.
	       if (card[1] == ' ' && tindex == tbpsta(tp, TBL_MAXCOLS)) {
	          tindex = tindex + 1
	          return(NO)
	       }
	    } else {
	       if (card[1] == ' ')
		  return(NO)
	    }

	    if (upar == NO) {
	       upar = YES
	       if (ppar != NULL)
		  call mfree (ppar, TY_CHAR)
	       mtsize = (LEN_CARD+1)*MAX_UPARM
	       sz_val = mtsize
	       call calloc (ppar, sz_val, TY_CHAR)
	       ioff = 0
	       npar = 0
	    }
	    # Keep user parameters in a buffer until END
	    sz_val = LEN_CARD
	    call amovc (card, Memc[ppar+ioff], sz_val)	# copy EOS also
	    ioff = ioff + LEN_CARD + 1
	    Memc[ppar+ioff-1] = EOS
	    npar = npar + 1
	    if (npar >= mtsize/(LEN_CARD+1)) {    # increase no. of cards by 10
		mtsize = mtsize + (LEN_CARD+1)*50
		sz_val = mtsize
		call realloc(ppar, sz_val, TY_CHAR)
	    }
	}
	return (NO)
end

define	TTYPE 1
define	TBCOL 2
define	TFORM 3
define	TUNIT 4
define	TNULL 5
define	TZERO 6
define	TSCAL 7
define	TDISP 8

# TAB_RKVAL -- Accumulate table column descriptors in memory.
#
int procedure tab_rkval (card, ext, ext_type)

char 	card[LEN_CARD]
pointer ext
int	ext_type

char	key[5], ftnfmt[SZ_COLFMT], sppfmt[SZ_COLFMT]
char	colfmt[SZ_COLFMT], colunits[SZ_COLUNITS]
int	index, ip, icc, nchar, jc, col_dtype, width, junk, dlen
long	nelem
pointer pt, pp, pu, pd, pc, pb, pf, pz, poff
int	strdic(), strncmp(), ctoi(), ctol(), ctod()

string	tbkeys "|TTYPE|TBCOL|TFORM|TUNIT|TNULL|TZERO|TSCAL|TDISP|"

begin
	# This assumes that first non_blank character is in column 1
	call strcpy (card, key, 5)
	index = strdic (key, key, 5, tbkeys)
	if (index == 0) return(0)

	ip = 6    #Char position to start decoding integer number.
	icc = COL_VALUE
	pt = EXT_PTYPE(ext)
	pd = EXT_PDTYPE(ext)
	pc = EXT_PCW(ext)
	pb = EXT_PBCOL(ext)
	pf = EXT_PFORM(ext)
	pz = EXT_PDSIZE(ext)

	switch(index) {
	case TTYPE:
	    nchar = ctoi (card, ip, jc)
	    poff = pt + (jc-1)*SZ_COLNAME
	    call rft_get_fits_string (card, Memc[poff], SZ_COLNAME)

	case TBCOL:
	    nchar = ctoi (card, ip, jc)
	    nchar = ctol (card, icc, Meml[pb+jc-1])
	case TFORM:
	    # This keyword value will give us the table column datatype,
	    # the format and the size in chars of the value
	    nchar = ctoi (card, ip, jc)
	    call rft_get_fits_string (card, ftnfmt , SZ_COLFMT)
	    if (ext_type == TABLE) {
	       # Get datatype and format width.
	       call tab_gtyp (ftnfmt, col_dtype, width)
	       # Get dlen: Number of characters for storage in table.
	       call tbbaln (col_dtype, junk, dlen)
	       # Get spp format.
	       call tbbftp (ftnfmt, sppfmt)
	       Meml[pc+jc-1] = width
	       nelem = 1
	    }else { # is BINTABLE
	       # Use of the pointer (EXT_PBCOL) to indicate a BYTE datatype.
	       # Get datatype and number of elements per table cell.
	       call tab_gbtyp (ftnfmt, col_dtype, nelem, ext)
	       if (col_dtype == TY_UBYTE) {
		  col_dtype = TY_SHORT 
		  Meml[pb+jc-1] = BYTE2SHORT
	       }
	       Meml[pc+jc-1] = col_dtype
	       # See if One element per char col.
	       if (nelem < 0) nelem = 1	  
	       # This call is not needed.
#	       call tbbaln (datat, junk, len)
	       sppfmt[1] = EOS
	    }
	    Memi[pd+jc-1] = col_dtype
	    Meml[pz+jc-1] = nelem
	    call strcpy (sppfmt, Memc[pf+(jc-1)*SZ_COLFMT], SZ_COLFMT)
	case TUNIT:
	    pu = EXT_PUNIT(ext)
	    nchar = ctoi (card, ip, jc)
	    call rft_get_fits_string (card, colunits, SZ_COLUNITS)
	    # Logical values has been encoded as character in the fits
	    # table. The values are 'T' and 'F'.
	    if (strncmp ("INTEGER*2", colunits, 9) == 0)
	       Memi[pd+jc-1] = TY_SHORT
	    if (ext_type == TABLE && 
		 strncmp ("LOGICAL-", colunits, 8) == 0) {
	       Meml[pz+jc-1] = -Memi[pd+jc-1]
	       colfmt[1] = '%'
	       call sprintf(colfmt[2], SZ_COLFMT, "%db")
		    call pargl(Meml[pz+jc-1])
	       Memi[pd+jc-1] = TY_BOOL
	       call strcpy (colfmt, Memc[pf+(jc-1)*SZ_COLFMT], SZ_COLFMT)
	       # Now get rid of the ""LOGICAL-" string.
	       call strcpy (colunits[9], colunits, SZ_COLUNITS)
	    }
	    call strcpy (colunits, Memc[pu+(jc-1)*SZ_COLUNITS], SZ_COLUNITS)
	case TNULL:
	    pp = EXT_PNULL(ext)
	    nchar = ctoi (card, ip, jc)
	    call get_null_string (card, Memc[pp+(jc-1)*SZ_COLUNITS],
				 SZ_COLUNITS)
	case TZERO:
	    pp = EXT_PZERO(ext)
	    nchar = ctoi (card, ip, jc)
	    nchar = ctod (card, icc, Memd[pp+jc-1])
	case TSCAL:
	    pp = EXT_PSCAL(ext)
	    nchar = ctoi (card, ip, jc)
	    nchar = ctod (card, icc, Memd[pp+jc-1])
	case TDISP:
	    pd = EXT_PDISP(ext)
	    nchar = ctoi (card, ip, jc)
	    call rft_get_fits_string (card, ftnfmt , SZ_COLFMT)
	    # Get spp format.
	    call tbbftp (ftnfmt, Memc[pd+(jc-1)*SZ_COLFMT])
	}
	return(jc)
end

# TAB_CRTAB -- Procedure to create the output table or update the gpb.

procedure tab_crtab (im, tp, ext, upar, ppar, npar, ext_type)
pointer	im, tp, ext, ppar
int	upar, npar, ext_type, i_val

pointer pt, pd, ps, pu, pf, pk, pz, pp, pb, pdis, junk
int	ncols, k
int	tbpsta(), gi_gstfval()
include "rfits.com"

begin
	# Now define the columns or the gbp's.
	if (gkey != TO_MG) {
	   ncols = tbpsta(tp, TBL_MAXCOLS)
	} else {
	   ncols = gi_gstfval (im, "PCOUNT")
	}
	# Go over the TSCAL and TZERO's to see if we need to change
	# the column datatypes.
	if ((scale == YES) && gkey != TO_MG) {
	   pk = EXT_PSCAL(ext)    
	   pz = EXT_PZERO(ext)   
	   pf = EXT_PFORM(ext)
	   pd = EXT_PDTYPE(ext)
	   ps = EXT_PDSIZE(ext)
	   pb = EXT_PBCOL(ext)
	   do k = 0, ncols-1 {
	      if (Memd[pk+k] != 1.0d0 || Memd[pz+k] != 0.0d0) {
		 switch (Memi[pd+k]) {
		 case TY_SHORT:
		    if (Memd[pk+k] == 1.0d0) {
		       Memi[pd+k] = TY_INT
#		       Meml[ps+k] = SZ_INT
		       Meml[ps+k] = 1        # Per Phil's advice 10-13-95.
		       call strcpy("%12d", Memc[pf+k*SZ_COLFMT], SZ_COLFMT)
		       if (ext_type == BINTABLE)
			  Meml[pb+k] = SHORT2INT
	            } else {
		       Memi[pd+k] = TY_REAL
#		       Meml[ps+k] = SZ_REAL
		       Meml[ps+k] = 1        # Per Phil's advice 10-13-95.
		       call strcpy("%12.5g", Memc[pf+k*SZ_COLFMT], SZ_COLFMT)
		       if (ext_type == BINTABLE)
			  Meml[pb+k] = SHORT2REAL
	            }
	         case TY_INT:
		    if (Memd[pk+k] != 1.0d0) {
		       Memi[pd+k] = TY_REAL
#		       Meml[ps+k] = SZ_REAL
		       Meml[ps+k] = 1        # Per Phil's advice 10-13-95.
		       call strcpy("%12.5g", Memc[pf+k*SZ_COLFMT], SZ_COLFMT)
		       if (ext_type == BINTABLE)
			  Meml[pb+k] = INT2REAL
	            }		
	         case TY_LONG:
		    if (Memd[pk+k] != 1.0d0) {
		       Memi[pd+k] = TY_REAL
#		       Meml[ps+k] = SZ_REAL
		       Meml[ps+k] = 1        # Per Phil's advice 10-13-95.
		       call strcpy("%12.5g", Memc[pf+k*SZ_COLFMT], SZ_COLFMT)
		       if (ext_type == BINTABLE) {
			  if ( SZ_LONG == 2 ) {
			     Meml[pb+k] = INT2REAL
			  } else {
			     Meml[pb+k] = LONG2REAL
			  }
		       }
	            }		
	         } #end switch
	      } #end if
	   } # enddo
	} #end if
	# Create SDAS Table column descriptors.
	pt = EXT_PTYPE(ext)     # Pointer to col_name
	pd = EXT_PDTYPE(ext)    # Pointer to col_datatype
	ps = EXT_PDSIZE(ext)    # Pointer to col_len
	pu = EXT_PUNIT(ext)    # Pointer to col_units
	pf = EXT_PFORM(ext)    # Pointer to col_format
	pdis = EXT_PDISP(ext)    # Pointer to display format.

	#Create the table column descriptor now.
	if (gkey != TO_MG) {

	   do k = 0, ncols-1 {
	      # If there was a TDISP value for the column, use this
	      # for column display format.
	      pp = pf
	      if (Memc[pdis+k*SZ_COLFMT] != EOS)
		 pp = pdis
	      call tbcdef (tp, junk, Memc[pt+k*SZ_COLNAME],
		        Memc[pu+k*SZ_COLUNITS], 
	               Memc[pp+k*SZ_COLFMT], Memi[pd+k], Meml[ps+k], 1)
           }

	} else {
	    do k = 0, ncols-1 {
		call gi_pdes (im, Memc[pt+k*SZ_COLNAME], Memi[pd+k], i_val,
			      k+1)
		Meml[ps+k] = i_val
	    }
	}

	if (gkey != TO_MG) {
	   call tbpset (tp, TBL_MAXPAR, npar+5)
	   call tbtcre (tp)
	   if (upar == YES) {  
	      # now write the user parameters to the table
	      call ftb_put_upar (tp, npar, Memc[ppar])
	      upar = NO
	      call mfree(ppar, TY_CHAR)
	   }
	} else {
	   if (upar == YES) {
	      call gi_gcomm (im, npar, Memc[ppar])
	      upar = NO
	      call mfree(ppar, TY_CHAR)
	   }
	}
end

include <lexnum.h>
include <ctype.h>

# FTB_PUT_UPAR -- Procedure to write user parameters to the table 
# already created.

procedure ftb_put_upar (tp, npar, uparbuf)

pointer tp			     # i: table descriptor
int	npar			     # i: number of parameters read
char	uparbuf[LEN_CARD, npar]      # i: buffer with user pars

char	keyword[SZ_KEYWORD], sval[LEN_CARD], blkn
int	i, k, stat, j, ltype
int	ival, iparn, ip, type, junk
double  dval
real    rval
bool    bval, bstring
char 	comment[LEN_CARD]
int	strncmp(), strmatch(), stridx(), sscan(), lexnum()

begin
	blkn = ' '
	do i = 1, npar {
	   # Extract keyword name
	   do k = 1, 8 {
	       if (uparbuf[k,i] == blkn) {
	          keyword[k] = EOS
	          break
	       }
	       keyword[k] = uparbuf[k,i]
	   }
	   keyword[SZ_KEYWORD+1] = EOS

  	   call get_val_comm (uparbuf[9,i], sval, comment, bstring)

           if (bstring) {                # is a string
	       call tbhadt (tp, keyword, sval)
 	       #Add comment
 	       call tbhpcm (tp, keyword, comment)
	   # Take care of HISTORY, COMMENT and boolean parameters
	   } else if (strmatch(keyword, "^HISTORY") != 0 ) {
	       call strcpy (uparbuf[9,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "HISTORY", sval)
	   } else if (strmatch(keyword, "^COMMENT") != 0 ) {
	       call strcpy (uparbuf[9,i], sval, LEN_CARD)
	       call trimh (sval)
	       call tbhadt (tp, "COMMENT", sval)
	   } else if (strncmp(uparbuf[10,i],
		      "                    T ", 22) == 0 ) {
	       bval = true	
	       call tbhadb (tp, keyword, bval)
 	       #Add comment
 	       call tbhpcm (tp, keyword, comment)
	   } else if (strncmp(uparbuf[10,i],
                      "                    F ", 22) == 0 ) {
	       bval = false
	       call tbhadb (tp, keyword, bval)
 	       #Add comment
 	       call tbhpcm (tp, keyword, comment)
	   } else if (keyword[1] == EOS ) {     		# Keyword is empty
               call strcpy (uparbuf[9,i], sval, LEN_CARD)
               call trimh (sval)
               call tbhadt (tp, keyword, sval)
           } else {                   				# is a number
	       ip = 1
	       # Determine the kind of number.
	       type = lexnum (sval, ip, junk)
	       if (type == LEX_REAL) {
	          stat = sscan(sval)
		    # Count the number of digits of precision,
		    # anything larger than 5 is double.
		    ip = 1
		    if (sval[1] == '-' || sval[1] == '+')
		      ip = ip + 1
		    while (sval[ip] == '0')
		      ip = ip + 1
	            for (j=1;  j <= MAX_DIGITS && IS_DIGIT(sval[ip]); j=j+1)
			ip = ip + 1
		    if (sval[ip] == '.' ) {
		       ip = ip + 1
                       if (j == 1)         # skip leading zeros
			  while (sval[ip] == '0')    # if str = "0.00ddd"
			     ip = ip + 1
		       for (;  j <= MAX_DIGITS && IS_DIGIT(sval[ip]); j=j+1)
			   ip = ip + 1
	            }
		    ltype = TY_REAL
	 	    if (stridx(sval[ip], "dD") > 0)  
		       ltype = TY_DOUBLE
		    if (j > 6 || ltype == TY_DOUBLE) {
		       call gargd(dval)
		       call tbhanp (tp, keyword, TY_DOUBLE, sval, iparn)
		       #Add a comment to a numeric value.
 		       call tbhpcm (tp, keyword, comment)
		    } else {
		       call gargr(rval)
	               call tbhadr (tp, keyword, rval)
		       #Add a comment to a numeric value.
 		       call tbhpcm (tp, keyword, comment)
		    }
	       } else if (type == LEX_DECIMAL) {
	             stat = sscan(sval)
		        call gargi(ival)
		     call tbhadi (tp, keyword, ival)
		     #Add a comment to a numeric value.
 		     call tbhpcm (tp, keyword, comment)
	       } else {
	          call tbhanp (tp, keyword, 't', sval, iparn)
		  #Add a comment to a numeric value.
 		  call tbhpcm (tp, keyword, comment)
               }	
	   }
	
	}
end

# TBG3dTYPE -- Get datatype  from bin_table fits files

procedure tab_gbtyp (ftnfmt, dtype, nelem, ext)

char	ftnfmt[SZ_COLFMT]	# i: fortran format specification
int	dtype		        # o: data type expressed as an int
long	nelem			# 0: number of elements in a column
pointer ext
#--
long	nchar
int	len, ipos
int	ctol(), strlen()

begin
	call strlwr (ftnfmt)
	len = strlen(ftnfmt)

	ipos = 1
	nchar = ctol (ftnfmt, ipos, nelem)
        if (nelem == 0)
	   nelem = 1

	# At this release (NOV 91) there is no support
	# yet for binary table with nelem > 1
	# We will skip the table data if this happens.
	IF (nelem > 1)
	   BIN_DTYNSP(ext) = true
	if (ftnfmt[len] == 'l') {
	    dtype = TY_BOOL
	    nchar = nelem
#	} else if (ftnfmt[len] == 'x') {
#	    dtype = TY_BITARR
	} else if (ftnfmt[len] == 'i') {
	    dtype = TY_SHORT
	    nchar = nelem*SZ_SHORT*SZB_CHAR
	} else if (ftnfmt[len] == 'j') {
	    dtype = TY_INT
	    nchar = nelem*SZ_INT*SZB_CHAR
	} else if (ftnfmt[len] == 'k') {
	    if ( SZ_LONG == 2 ) {
		call error (0, "TAB_GBTYP: cannot handle 64-bit integer.")
	    }
	    dtype = TY_LONG
	    nchar = nelem*SZ_LONG*SZB_CHAR
	} else if (ftnfmt[len] == 'e') {
	    dtype = TY_REAL
	    nchar = nelem*SZ_REAL*SZB_CHAR
	} else if (ftnfmt[len] == 'b') {    # BYTE datatype
	    dtype = TY_UBYTE
	    nchar = nelem
	} else if (ftnfmt[len] == 'd') {
	    dtype = TY_DOUBLE
	    nchar = nelem*SZ_DOUBLE*SZB_CHAR
	} else if (ftnfmt[len] == 'a') {
	    nchar = nelem + 1                   # +1 for the EOS
	    dtype = -nelem			# NOTE:  not an SPP data type
	    nelem = 1
	   BIN_DTYNSP(ext) = false    # except for characters, where is legal
				 # to have 12a for example.
	} else {
	    call eprintf ("\n** Bintable datatype not supported. ")
	    call eprintf ("Will skip file\n")
	}
	# Maxlen is the maximun number of characters in a column containing
	# an array. It is initialized in tab_wheader.
	BIN_MAXLEN(ext) = max (BIN_MAXLEN(ext), nelem*nchar)
end


# TBGTYPE -- Get datatype and field width from the format specification. 
# Notice that datatype for character format is not spp standard.

procedure tab_gtyp (ftnfmt, dtype, width)

char	ftnfmt[SZ_COLFMT]	# i: fortran format specification
int	dtype			# o: data type expressed as an int
int	width			# 0: field width in character (TBFORM value)
#--
int	nchar, ipos
int	ctoi()

begin
	call strlwr (ftnfmt)

	ipos = 2
	nchar = ctoi (ftnfmt, ipos, width)

	if (ftnfmt[1] == 'e') {
	    dtype = TY_REAL
	} else if (ftnfmt[1] == 'g') {
	    dtype = TY_REAL
	} else if (ftnfmt[1] == 'f') {
	    dtype = TY_REAL
	} else if (ftnfmt[1] == 'd') {
	    dtype = TY_DOUBLE
	} else if (ftnfmt[1] == 'i') {
	    dtype = TY_INT
	} else if (ftnfmt[1] == 'b') {
	    dtype = TY_BOOL
	} else if (ftnfmt[1] == 'a') {
	    dtype = -width			# NOTE:  not an SPP data type
	} else {
	    call error (5,"table datatype not supported")
	}
end

# Copy a string with no leading ot trailing blanks.
#
procedure get_string (instr, outstr, maxch)

char	instr[ARB]
char	outstr[ARB]
int	maxch

int	ip, k, nchar
int	strlen()

begin
	       ip = 1
	       while (instr[ip] == ' ')
		  ip = ip + 1
	       if (instr[ip] == '\'')
		  ip = ip + 1
               k = min (maxch, strlen(instr))
	       while (instr[k] == ' ')
	          k = k -1
	       if (instr[k] == '\'')
	          k = k -1
	       while (instr[k] == ' ')
	          k = k -1
	       if (k >= ip) {
		  nchar = min (maxch, k-ip+1)
		  for (k=1; k<=nchar; k=k+1)
		     outstr[k] = instr[ip+k-1]
		  outstr[k] = EOS
	       } else 
		  outstr[1] = EOS
	       
end

procedure trimh (card)

char card[LEN_CARD]

int	i
int	strlen()

begin
	for (i=strlen(card); 
	     i > 1  && (card[i] == ' ' || card[i] == '\n');
	     i=i-1)
	     ;

	     card[i+1] = EOS
	     
end

# GET_NULL_STRING -- Get null string with all the significant 
#		     trailing blanks.
procedure get_null_string (card, str, maxchar)

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

procedure get_val_comm (buf, sval, comment, bstring)

char	buf[ARB]        # Input buffer
char	sval[ARB]       # Output with keyword value
char	comment[ARB]    # Output with comment
bool	bstring		# True is 'sval' is a string.

int     ip, nch
char	sl
int	stridx(), ctowrd()

begin
        bstring = false
        if (buf[1] == '=') {   # Is a regular keyword
           ip = 2
           while(buf[ip] == ' ')
              ip = ip + 1
 
           if (buf[ip] == '\'')                         # Is a string
              bstring = true
           nch = ctowrd (buf, ip, sval, LEN_CARD)
	   sl = '/'
           nch = stridx(sl, buf[ip+1])
           if (nch > 0)
              call get_string (buf[ip+nch+1], comment, LEN_CARD)
        } else {
           call strcpy (buf, sval, LEN_CARD)
           comment[1] = EOS
        }
end
