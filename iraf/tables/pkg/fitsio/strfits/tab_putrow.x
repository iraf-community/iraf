include <mach.h>
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
# Phil Hodge	28-Sept-2005	Change procedures scale_datai, scale_datar,
#			scale_datad to replace the ibuf, rbuf, dbuf arguments
#			with icbuf, to be consistent with the way these
#			functions are called.  Call new functions cpy_xxi,
#			cpy_xxr, cpy_xxd to copy the contents of icbuf to
#			local variables of the correct type.

procedure rft_put_table_row (tp, ext, colptr, buf, rowlen, ncols, rownum)

pointer tp
pointer ext		# i: extension data structure
long	colptr[ARB]     # i: column pointer descriptor
char	buf[ARB]	# i: input string buffer
long	rowlen		# i: number of chars in buffer
int	ncols		# i: number of columns
long	rownum		# i: actual row number

size_t	sz_val
pointer	sp, pp, pz, ps, pc, pn, pl, pd, cptr
int	nch, ip, cn, j, ival
long	biof, len, lval
real	rval
double  dval

int	cmp_null(), tbcigi(), ctor(), ctoi(), ctol(), ctod()

include "rfits.com"

begin
	call smark (sp)
	sz_val = rowlen+1
	call salloc (pp, sz_val, TY_CHAR)
	call amovkc( " ", Memc[pp], sz_val)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)
	pc = EXT_PBCOL(ext)
	pl = EXT_PCW(ext)
	pn = EXT_PNULL(ext)
	pd = EXT_PDTYPE(ext)
	do cn = 0, ncols-1 {
	   cptr = colptr[cn+1]
	   # get position of first character and length of column
	   biof = Meml[pc+cn]
	   len = Meml[pl+cn]
	   if (Memc[pn+(cn)*SZ_COLUNITS] != EOS) {
	      if (cmp_null (buf[biof], Memc[pn+(cn)*SZ_COLUNITS], len) != 0) {
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
	   if (tbcigi(cptr, TBL_COL_DATATYPE) == TY_BOOL) {
	      if (buf[biof+len-1] == 'T')
		 Memc[pp] = 'Y'
	      else
		 Memc[pp] = 'N'
	      Memc[pp+1] = EOS
	   } else {
	      call rft_strcpy (buf[biof], Memc[pp], len)
	      # Strip trailing blanks
	      do j = len-1,0,-1
		 if (Memc[pp+j] != ' ') {
		    break
		 }
	      Memc[pp+j+1] = EOS
	   }
	   # scale data if necessary
	   if (scale == YES) {
	      if (Memd[pz+cn] != 0.0d0 || Memd[ps+cn] != 1.0d0) {
		 switch (Memi[pd+cn]) {
		 case TY_SHORT:
		    if (Memd[ps+cn] == 1.0d0) {
	               ip = 1
	               nch = ctoi (Memc[pp], ip, ival)
	               ival = ival*Memd[ps+cn] + Memd[pz+cn]
	               call tbepti (tp, cptr, rownum, ival)
		    } else {
	               ip = 1
	               nch = ctor (Memc[pp], ip, rval)
	               rval = rval*Memd[ps+cn] + Memd[pz+cn]
	               call tbeptr (tp, cptr, rownum, rval)
		    }
		 case TY_INT:
		    if (Memd[ps+cn] == 1.0d0) {
	               ip = 1
	               nch = ctoi (Memc[pp], ip, ival)
	               ival = ival*Memd[ps+cn] + Memd[pz+cn]
	               call tbepti (tp, cptr, rownum, ival)
		    } else {
	               ip = 1
	               nch = ctor (Memc[pp], ip, rval)
	               rval = rval*Memd[ps+cn] + Memd[pz+cn]
	               call tbeptr (tp, cptr, rownum, rval)
		    }
		 case TY_LONG:
		    if (Memd[ps+cn] == 1.0d0) {
	               ip = 1
	               nch = ctol (Memc[pp], ip, lval)
	               lval = lval*Memd[ps+cn] + Memd[pz+cn]
	               call tbeptl (tp, cptr, rownum, lval)
		    } else {
	               ip = 1
	               nch = ctor (Memc[pp], ip, rval)
	               rval = rval*Memd[ps+cn] + Memd[pz+cn]
	               call tbeptr (tp, cptr, rownum, rval)
		    }
		 case TY_REAL:
	            ip = 1
	            nch = ctor (Memc[pp], ip, rval)
	            rval = rval*Memd[ps+cn] + Memd[pz+cn]
	            call tbeptr (tp, cptr, rownum, rval)
		 case TY_DOUBLE:
	            ip = 1
	            nch = ctod (Memc[pp], ip, dval)
	            dval = dval*Memd[ps+cn] + Memd[pz+cn]
	            call tbeptd (tp, cptr, rownum, dval)
	         } #end switch
	         next
	      } #end if
	   } #end if scale
	   call tbeptt (tp, cptr, rownum, Memc[pp])
	}

	call sfree (sp)
end

# CMP_NULL -- See if string 'str' of length 'len' contains the
#	      pattern 'pattern'. Return zero if not.
int procedure cmp_null (str, pattern, len)

char	str[ARB]         # String of len characters (with no EOS)
char    pattern[SZ_COLUNITS]
long	len

size_t	sz_val
int	ind
long	pl, k
pointer sp, st, pt
int	rft_strcmp()
long	rft_strlen()

begin
	call smark (sp)
	sz_val = len+1
	call salloc (st, sz_val, TY_CHAR)
	call salloc (pt, sz_val, TY_CHAR)

	# string does not have EOS delimiter
	call rft_strcpy (str, Memc[st], len)
	call rft_strcpy (pattern, Memc[pt], len)
	# Extend the pattern to 'len' character by filling with
	# blanks.
	pl = rft_strlen (pattern)
	if (pl < len) {
	   do k = pl, len-1
	      Memc[pt+k] = ' '
	}
	Memc[pt+len] = EOS
	ind = 99
	if (rft_strcmp(Memc[st], Memc[pt]) != 0)
	   ind = 0
	call sfree(sp)
	return (ind)

end

procedure rft_strcpy (s1, s2, maxch)

char	s1[ARB]
char	s2[ARB]
long	maxch

long	i

begin
	do i = 1, maxch {
	    s2[i] = s1[i]
	    if (s2[i] == EOS)
		return
	}

	s2[maxch+1] = EOS
end

int procedure rft_strcmp (s1, s2)

char	s1[ARB]                         # strings to be compared
char	s2[ARB]                         # strings to be compared

long	i

begin
	do i = 1, MAX_LONG
	    if (s1[i] != s2[i])
		return (s1[i] - s2[i])
	    else if (s1[i] == EOS)
		return (0)
end

long procedure rft_strlen (str)

char	str[ARB]
long	ip

begin
	do ip = 1, MAX_LONG
	    if (str[ip] == EOS)
		return (ip - 1)
end

procedure rft_p3d_table_row (tp, ext, colptr, inbbuf, wcbuf,
				icbuf, ncols, rownum)

pointer tp
pointer ext
pointer	colptr[ARB]     # i: column pointer descriptor
char	inbbuf[ARB]	# i: input buffer with byte information alignment
char    wcbuf[ARB]	# Char buffer with char alignment
char    icbuf[ARB]	# Char buffer with possible int alignment
int	ncols		# i: number of columns
long	rownum		# i: actual row number

size_t	sz_val
pointer spp, pb,pd, pc, cptr
int	cn, i_val
long	biof, nelem, k
size_t	inoff, outoff, nbytes
int	dtype
long	tbcigl()

include "rfits.com"

begin
	biof = 1
	inoff = 1
	nbytes = 0

	pd = EXT_PDTYPE(ext)
	pc = EXT_PBCOL(ext)
	do cn = 0, ncols-1 {
	   cptr = colptr[cn+1]
	   dtype = Memi[pd+cn]
	   nelem = tbcigl (cptr, TBL_COL_LENDATA)
	   if (dtype < 0) {
	      nelem = -dtype
	      dtype = TY_CHAR
	   }
	   switch (dtype) {
	   case TY_SHORT:
	       inoff =  inoff + nbytes
	       outoff = 1
	       # See if data comes from a BYTE FITS column.
	       if (Meml[pc+cn] == BYTE2SHORT) {    # Yes, is byte.
		  nbytes = nelem
		  call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
                  call miiupk (wcbuf, icbuf, nelem, MII_BYTE, TY_SHORT)
	       } else {
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_SHORT)
	       }
	       call scale_datas (ext, tp, cn+1, cptr, icbuf, nelem, rownum)
	   case TY_INT:
	       inoff =  inoff + nbytes
	       outoff = 1
	       if (Meml[pc+cn] == SHORT2INT) {  # Is short --> int convertion.
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_INT)
	       } else {
	          nbytes = nelem*SZ_INT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_LONG, TY_INT)
	       }
	       call scale_datai (ext, tp, cn+1, cptr, icbuf, nelem, rownum)
	   case TY_REAL:
	       inoff =  inoff + nbytes
	       outoff = 1
	       if (Meml[pc+cn] == SHORT2REAL) {
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_REAL)
	       } else if (Meml[pc+cn] == INT2REAL) {
	          nbytes = nelem*SZ_INT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_LONG, TY_REAL)
	       } else if (Meml[pc+cn] == LONG2REAL) {
	          nbytes = nelem*SZ_LONG*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
		  if ( SZ_LONG == 2 ) {
		    call miiupk (wcbuf, icbuf, nelem, MII_LONG, TY_REAL)
		  } else {
		    call miiupk (wcbuf, icbuf, nelem, MII_LONGLONG, TY_REAL)
		  }
	       } else {
	          nbytes = nelem*SZ_REAL*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_REAL, TY_REAL)
	       }
	       call scale_datar (ext, tp, cn+1, cptr, icbuf, nelem, rownum)
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
	       i_val = nelem
	       call tbrptt (tp, cptr, wcbuf, i_val, 1, rownum)
	   case TY_BOOL:
	       # Boolean elements in 3d tables occupy 1 byte.
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_BYTE, TY_CHAR)
	       call smark(spp)
	       sz_val = nelem
	       call salloc(pb, sz_val, TY_BOOL)
	       do k = 0, nelem-1
		  Memb[pb+k] = (wcbuf[k+1] == 'T')
	       call tbrptb (tp, cptr, Memb[pb], 1, rownum)
	       call sfree(spp)
	   case TY_DOUBLE:
	       inoff =  inoff + nbytes
	       outoff = 1
	       nbytes = nelem*SZ_DOUBLE*SZB_CHAR
	       call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	       call miiupk (wcbuf, wcbuf, nelem, MII_DOUBLE, TY_DOUBLE)
	       call scale_datad (ext, tp, cn+1, cptr, wcbuf, nelem, rownum)
#	   case TY_BITARR:
#	   ;
	   default:
	       call eprintf("rft_put_table_row: datatype not supported\n")
	   }
	}
end

procedure scale_datas (ext, tp, col, colptr, ibuf, nelem, rownum)

pointer ext
pointer	tp
int	col
pointer	colptr
short	ibuf[ARB]
long	nelem
long	rownum

pointer ps, pz
include "rfits.com"

begin

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if ((scale == YES) &&
	   (Memd[pz+col-1] != 0.0d0 || Memd[ps+col-1] != 1.0d0)) {
	#
	# No scaling of type short is done, since the datatype
	# has been promoted to int or float.
	#
 call eprintf("Scale_datas: TY_SHORT scaling, needs to be INT\n")
	}
	call tbrpts (tp, colptr, ibuf, 1, rownum)
end

procedure scale_datai (ext, tp, col, colptr, icbuf, nelem, rownum)

pointer ext
pointer	tp
int	col
pointer	colptr
char    icbuf[ARB]
long	nelem
long	rownum

size_t	sz_val
pointer ps, pz, sp, rb
long	k
pointer ibuf			# a copy of icbuf, but declared to be int
include "rfits.com"

begin
	call smark(sp)
	sz_val = nelem
	call salloc (ibuf, sz_val, TY_INT)
	call cpy_xxi (icbuf, Memi[ibuf], nelem)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if ((scale == YES) &&
	   (Memd[pz+col-1] != 0.0d0 || Memd[ps+col-1] != 1.0d0)) {
           if (Memd[ps+col-1] != 1.0d0) {
	      # Columns datatype has been converted to real only
	      # if the TSCAL was different from 1.0. (see tab_rheader)
	      sz_val = nelem
	      call salloc(rb, sz_val, TY_REAL)
	      do k = 1, nelem
	         Memr[rb+k-1] = Memi[ibuf+k-1]*Memd[ps+col-1] + Memd[pz+col-1]
	      call tbrptr (tp, colptr, Memr[rb], 1, rownum)
	      call sfree(sp)
	      return
	   } else {
	      do k = 1, nelem
	         Memi[ibuf+k-1] = Memi[ibuf+k]*Memd[ps+col-1] + Memd[pz+col-1]
	   }
	}
	call tbrpti (tp, colptr, Memi[ibuf], 1, rownum)
	call sfree(sp)
end

procedure scale_datar (ext, tp, col, colptr, icbuf, nelem, rownum)

pointer ext
pointer	tp
int	col
pointer	colptr
char    icbuf[ARB]
long	nelem
long	rownum

size_t	sz_val
pointer ps, pz
long	k
pointer sp
pointer rbuf			# a copy of icbuf, but declared to be real
include "rfits.com"

begin
	call smark(sp)
	sz_val = nelem
	call salloc (rbuf, sz_val, TY_REAL)
	call cpy_xxr (icbuf, Memr[rbuf], nelem)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if ((scale == YES) &&
	   (Memd[pz+col-1] != 0.0d0 || Memd[ps+col-1] != 1.0d0)) {
	   do k = 1, nelem
	      Memr[rbuf+k-1] = Memr[rbuf+k-1]*Memd[ps+col-1] + Memd[pz+col-1]
	}
	call tbrptr (tp, colptr, Memr[rbuf], 1, rownum)
	call sfree (sp)
end

procedure scale_datad (ext, tp, col, colptr, icbuf, nelem, rownum)

pointer ext
pointer	tp
int	col
pointer	colptr
char    icbuf[ARB]
long	nelem
long	rownum

size_t	sz_val
pointer ps,pz
long	k
pointer sp
pointer dbuf			# a copy of icbuf, but declared to be double
include "rfits.com"

begin
	call smark(sp)
	sz_val = nelem
	call salloc (dbuf, sz_val, TY_DOUBLE)
	call cpy_xxd (icbuf, Memd[dbuf], nelem)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if ((scale == YES) &&
	   (Memd[pz+col-1] != 0.0d0 || Memd[ps+col-1] != 1.0d0)) {
	   do k = 1, nelem
	      Memd[dbuf+k-1] = Memd[dbuf+k-1]*Memd[ps+col-1] + Memd[pz+col-1]
	}
	call tbrptd (tp, colptr, Memd[dbuf], 1, rownum)
	call sfree (sp)
end

procedure cpy_xxi (input, output, nelem)

char	input[ARB]
int	output[ARB]
long	nelem
#--
long	i, j
int	ratio, k
char	cccbuf[SZ_INT/SZ_CHAR]
int	iiibuf[1]
equivalence (iiibuf[1], cccbuf[1])

begin
	ratio = SZ_INT / SZ_CHAR
	i = 1
	do j = 1, nelem {
	    do k = 1, ratio {
		cccbuf[k] = input[i]
		i = i + 1
	    }
	    output[j] = iiibuf[1]
	}
end

procedure cpy_xxr (input, output, nelem)

char	input[ARB]
real	output[ARB]
long	nelem
#--
long	i, j
int	ratio, k
char	cccbuf[SZ_REAL/SZ_CHAR]
real	rrrbuf[1]
equivalence (rrrbuf[1], cccbuf[1])

begin
	ratio = SZ_REAL / SZ_CHAR
	i = 1
	do j = 1, nelem {
	    do k = 1, ratio {
		cccbuf[k] = input[i]
		i = i + 1
	    }
	    output[j] = rrrbuf[1]
	}
end

procedure cpy_xxd (input, output, nelem)

char	input[ARB]
double	output[ARB]
long	nelem
#--
long	i, j
int	ratio, k
char	cccbuf[SZ_DOUBLE/SZ_CHAR]
double	dddbuf[1]
equivalence (dddbuf[1], cccbuf[1])

begin
	ratio = SZ_DOUBLE / SZ_CHAR
	i = 1
	do j = 1, nelem {
	    do k = 1, ratio {
		cccbuf[k] = input[i]
		i = i + 1
	    }
	    output[j] = dddbuf[1]
	}
end
