include	<tbset.h>
include <mii.h>
include "rfits.h"


# RFT_PUT_TABLE_ROW -- Procedure to fill each column buffer with blanks
# from the last non_character to the buffer length. See also if there are
# null values defined or a scaled column has been found; then copy to a double
# dimension buffer.
#
# AUG 1991 NZ. Add support to read LOGICAL values for a table column. This
#              values are 'T' or 'F' and are encoded as a character string.

procedure rft_put_table_row (tp, ext, colptr, buf, rowlen, ncols, rownum)

pointer tp
pointer ext		# i: extension data structure
int	colptr[ARB]     # i: column pointer descriptor
char	buf[ARB]	# i: input string buffer
int	rowlen		# i: number of chars in buffer
int	ncols		# i: number of columns
int	rownum		# i: actual row number

pointer	sp, pp,pz,ps,pc,pn,pl, pt
int	i, j, nch, ctor(), tbcigi()
int	biof, len, ip, cmp_null()
real	rval, value

include	"tab.com"
include "rfits.com"


begin
	call smark (sp)
	call salloc (pp, rowlen+1, TY_CHAR)
	call salloc (pt, rowlen+1, TY_CHAR)
	call amovkc( " ", Memc[pp], rowlen+1)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)
	pc = EXT_PCOL(ext)
	pl = EXT_PCW(ext)
	pn = EXT_PNULL(ext)
	do i = 1, ncols {
	   # get position of first character and length of column
	   biof = Memi[pc+i-1]
	   len = Memi[pl+i-1]

	   if (Memc[pn+(i-1)*SZ_COLUNITS] != EOS) {
	      if (cmp_null (buf[biof], Memc[pn+(i-1)*SZ_COLUNITS], len) != 0) {
		 # if the input buffer has a null value just skip the column,
		 # since the output buffer already has UNDEF on it.
	         next
	      }
	   }
	   # copy the column element to a NULL terminated string

	   # If the column datatype is Boolean then change the value
	   # to the character boolean equivalent 'Y' or 'N'.
	   # The table values are 'T' or 'F'.
	   # TODO: If there is a floating point field with no period, then
	   #       is it not interpreted correctly by this code. We need to
	   #       look for a period and then applied the scaling according to
	   #       the 'd' field in the input TFORM value. (Nov 1993)
	   if (tbcigi(colptr[i], TBL_COL_DATATYPE) == TY_BOOL) {
	      if (buf[biof+len-1] == 'T')
		 Memc[pp] = 'Y'
	      else
		 Memc[pp] = 'N'
	      Memc[pp+1] = EOS
	   } else { 
	      call strcpy (buf[biof], Memc[pp], len)
	      # Strip trailing blanks
	      do j = len-1,0,-1
		 if (Memc[pp+j] != ' ') {
		    break
		 }
	      Memc[pp+j+1] = EOS
	   }
	   # scale data if necessary
	   if (scale == YES)
	      if (Memr[pz+i-1] != 0.0 || Memr[ps+i-1] != 1.0) {
	         ip = 1
	         nch = ctor (Memc[pp], ip, rval)
	         value = rval*Memr[ps+i-1] + Memr[pz+i-1]
	         call tbeptr (tp, colptr[i], rownum, value)
	         next
	      } 
	   call tbeptt (tp, colptr[i], rownum, Memc[pp])
	}

	call sfree (sp)
end

# CMP_NULL -- See if string 'str' of length 'len' contains the
#	      pattern 'pattern'. Return zero if not.
int procedure cmp_null (str, pattern, len)

char	str[ARB]         # String of len characters (with no EOS)
char    pattern[SZ_COLUNITS],ch
int	len, ind, strmatch(),k, strlen(),pl

pointer sp, st, pt
begin
	call smark (sp)
	call salloc (st, len+1, TY_CHAR)
	call salloc (pt, len+1, TY_CHAR)

	# string does not have EOS delimiter
	call strcpy (str, Memc[st], len)
	call strcpy (pattern, Memc[pt], len)
	# Extend the pattern to 'len' character by filling with 
	# its last character.
	pl = strlen (pattern)
	if (pl < len) {
	   ch = pattern[pl]
	   do k = pl, len-1
	      Memc[pt+k] = ch
	}
	ind  = strmatch(Memc[st], Memc[pt])
	call sfree(sp)
	return (ind)

end
include <mach.h>

procedure rft_p3d_table_row (tp, ext, colptr, inbbuf, wcbuf, 
				icbuf, ncols, rownum)

pointer tp
pointer ext
int	colptr[ARB]     # i: column pointer descriptor
char	inbbuf[ARB]	# i: input buffer with byte information alignment
char    wcbuf[ARB]	# Char buffer with char alignment
char    icbuf[ARB]	# Char buffer with possible int alignment
int	ncols		# i: number of columns
int	rownum		# i: actual row number

bool    scale_datai(), scale_datar(), scale_datad(), scale_datas()
pointer spp, pb,pp, pc
int	biof, inoff, outoff, nbytes, i 
int	dtype, nelem, tbcigi(), k

include	"tab.com"
include "rfits.com"

begin
	biof = 1
	inoff = 1
	nbytes = 0

	pp = EXT_PCW(ext)
	pc = EXT_PCOL(ext)
	do i = 1, ncols {
	   dtype = Memi[pp+i-1]
	   nelem = tbcigi (colptr[i], TBL_COL_LENDATA)
	   if (dtype < 0) {
	      nelem = -dtype
	      dtype = TY_CHAR
	   }
	   switch (dtype) {
	   case TY_SHORT:
	       inoff =  inoff + nbytes
	       outoff = 1
	       # See if data comes from a BYTE FITS column.
	       if (Memi[pc+i-1] == 1) {    # Yes, is byte.
		  nbytes = nelem
		  call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
                  call miiupk (wcbuf, icbuf, nelem, MII_BYTE, TY_SHORT)
	       } else {
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_SHORT)
	       }
	       # These data items (ty_short) has not been promoted to 
	       # ty_real when scaled, because the TSCAL and TZERO can be
	       # located after the column definition in the FITS
	       # header. It is not easy to redefine the column datatype
	       # after is defined. This is a draw back since data truncation
	       # can occur.
	       if (scale_datas (ext, i, icbuf, wcbuf, nelem))
	          call tbrpts (tp, colptr[i], wcbuf, 1, rownum)
	       else
	          call tbrpts (tp, colptr[i], icbuf, 1, rownum)
	   case TY_INT:
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem*SZ_INT*SZB_CHAR
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_INT, TY_INT)
	       if (scale_datai (ext, i, wcbuf, icbuf, nelem))
	          call tbrptr (tp, colptr[i], icbuf, 1, rownum)
	       else
	          call tbrpti (tp, colptr[i], wcbuf, 1, rownum)
	   case TY_REAL:
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem*SZ_REAL*SZB_CHAR
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_REAL, TY_REAL)
	       if (scale_datar (ext, i, wcbuf, icbuf, nelem))
	          call tbrptr (tp, colptr[i], icbuf, 1, rownum)
	       else
	          call tbrptr (tp, colptr[i], wcbuf, 1, rownum)
	   case TY_CHAR:
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem
	       call bytmov (inbbuf, inoff, icbuf, outoff, nbytes)
	       call strupk (icbuf, wcbuf, nelem)
	       wcbuf[nelem+1] = EOS
	       # Strip trailing blanks
	       do k = nelem,1,-1
		  if (wcbuf[k] != ' ') {
		    break
		  }
	       wcbuf[k+1] = EOS
	       call tbrptt (tp, colptr[i], wcbuf, nelem, 1, rownum)
	   case TY_BOOL:
	       # Boolean elements in 3d tables occupy 1 byte.
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_BYTE, TY_CHAR)
	       call smark(spp)
	       call salloc(pb, nelem, TY_BOOL)
	       do k = 0, nelem-1
		  Memb[pb+k] = (wcbuf[k+1] == 'T')
	       call tbrptb (tp, colptr[i], Memb[pb], 1, rownum)
	       call sfree(spp)
	   case TY_DOUBLE:
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem*SZ_DOUBLE*SZB_CHAR
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_DOUBLE, TY_DOUBLE)
	       if (scale_datad (ext, i, wcbuf, icbuf, nelem))
	          call tbrptd (tp, colptr[i], icbuf, 1, rownum)
	       else
	          call tbrptd (tp, colptr[i], wcbuf, 1, rownum)
#	   case TY_BITARR:
#	   ;
	   default:
	       call eprintf("rft_put_table_row: datatype not supported\n")
	   }
	}
end

bool procedure scale_datas (ext, col, isbuf, osbuf, nelem)
pointer ext
int	col
short	isbuf[ARB]
short	osbuf[ARB]
int	nelem

pointer ps,pz
int	k
include "rfits.com"
begin
	if (scale == NO) return(false)
	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if (Memr[pz+col-1] != 0.0 || Memr[ps+col-1] != 1.0) {
	   do k = 1, nelem
	      osbuf[k] = isbuf[k]*Memr[ps+col-1] + Memr[pz+col-1]
	   return(true)
	} else
	   return(false)
end
bool procedure scale_datai (ext, col, ibuf, rbuf, nelem)
pointer ext
int	col
int	ibuf[ARB]
real	rbuf[ARB]
int	nelem

pointer ps,pz
int	k
include "rfits.com"
begin
	if (scale == NO) return(false)
	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if (Memr[pz+col-1] != 0.0 || Memr[ps+col-1] != 1.0) {
	   do k = 1, nelem
	      rbuf[k] = ibuf[k]*Memr[ps+col-1] + Memr[pz+col-1]
	   return(true)
	} else
	   return(false)
end
bool procedure scale_datar (ext, col, irbuf, orbuf, nelem)
pointer ext
int	col
real	irbuf[ARB]
real	orbuf[ARB]
int	nelem

pointer ps,pz
int	k
include "rfits.com"
begin
	if (scale == NO) return(false)
	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if (Memr[pz+col-1] != 0.0 || Memr[ps+col-1] != 1.0) {
	   do k = 1, nelem
	      orbuf[k] = irbuf[k]*Memr[ps+col-1] + Memr[pz+col-1]
	   return(true)
	} else
	   return(false)
end
bool procedure scale_datad (ext, col, idbuf, odbuf, nelem)
pointer ext
int	col,nelem
double  idbuf[ARB]
double  odbuf[ARB]

pointer ps,pz
int	k
include "rfits.com"
begin
	if (scale == NO) return(false)
	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if (Memr[pz+col-1] != 0.0 || Memr[ps+col-1] != 1.0) {
	   do k = 1, nelem
	      odbuf[k] = idbuf[k]*Memr[ps+col-1] + Memr[pz+col-1]
	   return(true)
	} else
	   return(false)
end
