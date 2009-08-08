include <mach.h>
include <tbset.h>
include <imhdr.h>
include <mii.h>
include "wfits.h"

# TAB_WRITE_DATA -- Procedure to convert IRAF table data to FITS format
# line by line.

procedure tab_write_data (tp, ext, fits_fd)

pointer	tp			# IRAF table descriptor
pointer	ext			# Extension data structure
int	fits_fd			# FITS file descriptor

size_t	sz_val
pointer bf, cf, sp
long	nlines, i
int	ncols
size_t	rowlen, npix_record, nrecords

int	tbpsta()
long	tbpstl()
errchk	malloc, mfree, tab_get_data_line, wft_scale_line, wft_long_line
errchk	wft_init_write_pixels, wft_write_pixels, wft_write_last_record

include "wfits.com"

begin
	nlines = tbpstl (tp, TBL_NROWS)
	if (nlines == 0)
	    return
	
	if (ext_type == BINTABLE) {
	   ieee = YES     # binary tables is ieee format
	   call wft_init_write_pixels(len_record, TY_CHAR, MII_BYTE)
	   call ieemapr (YES, YES)
	   call ieemapd (YES, YES)
	}

	rowlen = EXT_LENAXIS(ext,1)

	call smark(sp)
	sz_val = maxlen
	call salloc (cf, sz_val, TY_CHAR)
	call salloc (bf, rowlen, TY_CHAR)

	npix_record = len_record * FITS_BYTE / EXT_BITPIX(ext)

	ncols = tbpsta (tp, TBL_NCOLS)

	do i = 1, nlines {

	    call amovkc (" " , Memc[bf], rowlen)
	    # Get an image line.
	    call tab_get_data_line (tp, COLPTR(ext), Memc[bf], 
			Memc[cf], ncols, i)

	    # write the pixels
	    call wft_write_pixels (fits_fd, Memc[bf], rowlen)
	}

	# write the final record
	call wft_write_last_record (fits_fd, nrecords)
	if (long_header == YES) {
	    call printf ("%d  Data records(s) written\n")
	        call pargz (nrecords)
	}
	call  sfree(sp)
	
end


# TAB_GET_DATA_LINE -- Procedure to fetch the next table data line.

procedure tab_get_data_line (tp, colptr, buf, cbuf, numcols, rownum)

pointer tp			# Pointer to table descriptor
pointer	colptr[numcols]		# Array of pointers to column descriptors
char	buf[ARB]		# pointer to table data line
char	cbuf[ARB]		# pointer temporary char buffer
int	numcols			# Number of columns from which to get values
long	rownum			# Row number

pointer colp
bool	nullflag		# flag:  true ==> element is undefined
int	lenstring		# Length of each string in array buffer,
				# in chars.
char	sppfmt[SZ_COLFMT], forfmt[SZ_COLFMT]
bool    bbuf
pointer sp, pp, pb, sp2
int	datatype, i, k, col, iof, nchar
size_t	inoff, outoff, nbytes, npcell
long	j
int	tbcigi(), strlen()
long	tbcigl()

include "wfits.com"

begin
	iof = 1
	col = 1
	outoff = 1
	nbytes = 0
	do i = 1, numcols {
	   colp = colptr[i]
	   datatype = tbcigi (colp, TBL_COL_DATATYPE)
	   # Nelem_per_cell is the number of elements per cell (could be 
	   # >1 in a binary table)
	   npcell = tbcigl (colp, TBL_COL_LENDATA)
	   call smark(sp)
	   call salloc(pb, npcell, TY_BOOL)
	   if (datatype < 0) {
	      nchar = -datatype
	      datatype = TY_CHAR
	   }
	   if (ext_type == TABLE) {
	      call tbcigt (colp, TBL_COL_FMT, sppfmt, SZ_COLFMT)
	      if (def_fmt == YES) {
		 if (datatype == TY_CHAR)
		    k = -nchar  # We want to pass the real datatype (e.g. -72)
		 else
		   k = datatype
	         call chgfmt (sppfmt, k, forfmt, lenstring)
	      } else
	         #change only H,Z,L,M to F.
	         call chgtyp (sppfmt, datatype, forfmt, lenstring)
	      # change back to spp format with the new spec
	      call tbbftp (forfmt, sppfmt)
	   }
	   switch (datatype) {
	   case TY_SHORT:
	      call tbrgts (tp, colp, cbuf, Memb[pb], col, rownum)
	      if (ext_type == BINTABLE) {
		 call miipak (cbuf, cbuf, npcell, TY_SHORT, MII_SHORT)
		 inoff = 1       # first byte from input buffer to be move
		 outoff = outoff + nbytes
		 nbytes = npcell*SZ_SHORT*SZB_CHAR
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      } else {
		 nullflag = Memb[pb]
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		     call pargs (cbuf)
	      }
	   case TY_INT:
	      call tbrgti (tp, colp, cbuf, Memb[pb], col, rownum)
	      if (ext_type == BINTABLE) {
		 call miipak (cbuf, cbuf, npcell, TY_INT, MII_LONG)
		 inoff = 1       # first byte from input buffer to be move
		 outoff = outoff + nbytes
		 nbytes = npcell*SZ_INT*SZB_CHAR
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      } else {
		 nullflag = Memb[pb]
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		     call pargi (cbuf)
	      }
	   case TY_LONG:
	      call tbrgtl (tp, colp, cbuf, Memb[pb], col, rownum)
	      if (ext_type == BINTABLE) {
		 if ( SZ_LONG == 2 ) {
		    call miipak (cbuf, cbuf, npcell, TY_LONG, MII_LONG)
		 } else {
		    call miipak (cbuf, cbuf, npcell, TY_LONG, MII_LONGLONG)
		 }
		 inoff = 1       # first byte from input buffer to be move
		 outoff = outoff + nbytes
		 nbytes = npcell*SZ_LONG*SZB_CHAR
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      } else {
		 nullflag = Memb[pb]
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		     call pargl (cbuf)
	      }
	   case TY_REAL:
	      call tbrgtr (tp, colp, cbuf, Memb[pb], col, rownum)
	      if (ext_type == BINTABLE) {
		 # Set the mapping from INDEFR to NaN.
		 call ieesnanr (INDEFR) 
		 call miipak (cbuf, cbuf, npcell, TY_REAL, MII_REAL)
		 inoff = 1
		 outoff = outoff + nbytes
		 nbytes = npcell*SZ_REAL*SZB_CHAR
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      } else {
		 nullflag = Memb[pb]
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		   call pargr (cbuf)
	      }
	   case TY_DOUBLE:
	      call tbrgtd (tp, colp, cbuf, Memb[pb], col, rownum)
	      if (ext_type == BINTABLE) {
		 # Set the mapping from INDEFD to NaN.
		 call ieesnand (INDEFD) 
		 call miipak (cbuf, cbuf, npcell, TY_DOUBLE, MII_DOUBLE)
		 inoff = 1
		 outoff = outoff + nbytes
		 nbytes = npcell*SZ_DOUBLE*SZB_CHAR
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      } else {
		 nullflag = Memb[pb]
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		   call pargd (cbuf)
	      }
	   case TY_CHAR:
	      if (ext_type == BINTABLE) {
	         call tbrgtt (tp, colp, cbuf, Memb[pb], maxlen,
			      col, rownum)
	         do k = strlen(cbuf)+1, maxlen    # Pad with blanks
		    cbuf[k] = ' '
		 inoff = 1       # first byte from input buffer to be move
		 outoff = outoff + nbytes
		 nbytes = nchar
		 call miipak (cbuf, cbuf, nbytes, TY_CHAR, MII_BYTE)
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
	      }else
	         call tbrgtt (tp, colp, buf[iof], nullflag, lenstring, 
			      col, rownum)
	   case TY_BOOL:
	      if (ext_type == BINTABLE) {
		 call smark(sp2)
		 call salloc (pp, npcell, TY_BOOL)
	         call tbrgtb (tp, colp, Memb[pp], Memb[pb], col, rownum)
		 do j = 0, npcell-1
		    if (Memb[pp+j]) cbuf[j+1] = 'T' 
		    else cbuf[j+1] = 'F'
		 inoff = 1       # first byte from input buffer to be move
		 outoff = outoff + nbytes
		 nbytes = npcell
		 call miipak (cbuf, cbuf, npcell, TY_CHAR, MII_BYTE)
		 call bytmov (cbuf, inoff, buf, outoff, nbytes)
		 call sfree(sp2)
	      }else {
	         call tbrgtb (tp, colp, bbuf, nullflag, col, rownum)
		 # Aug 1991 NZ. Add support for LOGICAL on FITS, for 
		 # extension = TABLE
	         call sprintf (buf[iof], lenstring, sppfmt)	    
		 if (bbuf)
		   call pargstr("T")
		 else
		   call pargstr("F")
	      }
	   default:
		call flush(STDOUT)
		call error (3, "Input column datatype not supported")
	   }
	   # Blank the NULL terminator
	   do k = strlen(buf[iof]), lenstring-1
	      buf[iof+k] = ' '
	   # Replace the string 'INDEF' with '*'s for a non character column
	   # else put blanks.

	   if ((ext_type == TABLE) && nullflag) {
	      buf[iof] = '*'
	      do k = 1, lenstring-1
	         buf[iof+k] = ' '
	   }
	iof = iof + lenstring
#	buf[iof-1] = ' '
	call sfree(sp)
	}
end

# CHGTYP -- Convert spp format to those Fortran format acceptable by
#	    the FITS table standard (i.e. I,E,D,A only)
#	    Spp's H,M,L,Z to those above.
#
# NZ AUG 1991. Since LOGICAL datatype is not accepted in TABLE fits 
#              extension, put the values 'T' and 'F' as characters.
define	LOG10_16	1.204	# base 10 log of 16

procedure chgtyp (sppfmt, datatype, ftnfmt, lenfmt)

char	sppfmt[ARB]		# i: Print format in SPP style
int	datatype		# i: Column datatype
char	ftnfmt[ARB]		# o: The corresponding Fortran format
int	lenfmt			# o: Lenght of display format ('w' field)
#--
char	p_ftnfmt[SZ_COLFMT]	# pseudo-Fortran format (incl SPP extensions)
char	dot			# '.'
int	w_num, d_num		# field width and number of decimals (as in w.d)
int	nchar, ip		# for reading w and d using ctoi, and for itoc
int	dot_loc			# location of "." in format
int	stridx(), ctoi(), itoc()

begin
	# Convert to pseudo-Fortran print format, which may not be a valid
	# Fortran format.
	call strlwr(sppfmt)     # To take care of %12.5H for example.
	call tbbptf (sppfmt, p_ftnfmt)

	if (p_ftnfmt[2] == '-') {			# get rid of it
	    do ip = 3, SZ_COLFMT
		p_ftnfmt[ip-1] = p_ftnfmt[ip]
	    p_ftnfmt[SZ_COLFMT] = EOS
	}

	# We may not need this stuff; see below.
	dot = '.'
	dot_loc = stridx (dot, p_ftnfmt)
	ip = 2
	if (ctoi (p_ftnfmt, ip, w_num) > 0) {		# field width
	    if (dot_loc > 0) {
		ip = dot_loc + 1
		if (ctoi (p_ftnfmt, ip, d_num) <= 0)	# number of decimals
		    d_num = 0
	    } else {
		d_num = 0
	    }
	} else {
	    w_num = 0
	}
	if ((w_num < 0) || (d_num < 0)) {
	    call flush (STDOUT)
	    call error (1, "chgtyp:  invalid format")
	}
	if (w_num == 0)
	    w_num = 6

	# There are only a few formats that need fixing.
	ip = 2
	if (p_ftnfmt[1] == 'H') {			# hours:min:sec format
	    if (datatype == TY_DOUBLE) {
	       ftnfmt[1] = 'F'
	       w_num = 21
	       d_num = 16
	    } else {	# assume single float
	       ftnfmt[1] = 'F'
	       d_num = 5
	       w_num = 9
	    }
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    ftnfmt[ip+nchar] = dot
	    ip = ip + nchar + 1
	    nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else if (p_ftnfmt[1] == 'M') 	{		# min:sec format
	    if (datatype == TY_DOUBLE) {
	       ftnfmt[1] = 'F'
	       w_num = 20
	       d_num = 16
	    } else {	# assume single float
	       ftnfmt[1] = 'F'
	       d_num = 5
	       w_num = 9
	    }
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    ftnfmt[ip+nchar] = dot
	    ip = ip + nchar + 1
	    nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else if (p_ftnfmt[1] == 'Z') {		# hexadecimal
	    ftnfmt[1] = 'I'
	    w_num = w_num * LOG10_16 + 1		# need more room
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else if (p_ftnfmt[1] == 'L') {		# Logical
	    call strcpy (p_ftnfmt, ftnfmt, SZ_COLFMT)
#	    ftnfmt[1] = 'I'				# change to I (1 or 0)
	    ftnfmt[1] = 'A'		# july 1991 NZ	
	} else if (p_ftnfmt[1] == 'I' && (datatype != TY_SHORT) &&
					 (datatype != TY_INT) &&
					 (datatype != TY_LONG)) { # Logical
	    if (datatype == TY_DOUBLE) {
	       ftnfmt[1] = 'F'
	       w_num = 25
	       d_num = 17
	    } else {	# assume single float
	       ftnfmt[1] = 'F'
	       d_num = 7
	       w_num = 15
	    }
	    nchar = itoc (w_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	    ftnfmt[ip+nchar] = dot
	    ip = ip + nchar + 1
	    nchar = itoc (d_num, ftnfmt[ip], SZ_COLFMT-ip+1)
	} else
	    call strcpy (p_ftnfmt, ftnfmt, SZ_COLFMT)

	lenfmt = w_num + 1		# separate each field by a blank
end
