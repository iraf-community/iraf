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
int	colptr[ARB]     # i: column pointer descriptor
char	buf[ARB]	# i: input string buffer
int	rowlen		# i: number of chars in buffer
int	ncols		# i: number of columns
int	rownum		# i: actual row number

pointer	sp, pp,pz,ps,pc,pn,pl, pd
int	j, nch, ctor(), tbcigi(), ival, ctoi(), ctod()
int	biof, len, ip, cmp_null(), cn, cptr
real	rval
double  dval

include "rfits.com"


begin
	call smark (sp)
	call salloc (pp, rowlen+1, TY_CHAR)
	call amovkc( " ", Memc[pp], rowlen+1)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)
	pc = EXT_PBCOL(ext)
	pl = EXT_PCW(ext)
	pn = EXT_PNULL(ext)
	pd = EXT_PDTYPE(ext)
	do cn = 0, ncols-1 {
	   cptr = colptr[cn+1]
	   # get position of first character and length of column
	   biof = Memi[pc+cn]
	   len = Memi[pl+cn]
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
	      call strcpy (buf[biof], Memc[pp], len)
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
		 case TY_INT,TY_LONG:
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
int	len, ind, strcmp()
int     pl, strlen(),k

pointer sp, st, pt
begin
	call smark (sp)
	call salloc (st, len+1, TY_CHAR)
	call salloc (pt, len+1, TY_CHAR)

	# string does not have EOS delimiter
	call strcpy (str, Memc[st], len)
	call strcpy (pattern, Memc[pt], len)
	# Extend the pattern to 'len' character by filling with
	# blanks.
	pl = strlen (pattern)
	if (pl < len) {
	   do k = pl, len-1
	      Memc[pt+k] = ' '
	}
	Memc[pt+len] = EOS
	ind = 99
	if (strcmp(Memc[st], Memc[pt]) != 0)
	   ind = 0
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

pointer spp, pb,pd, pc
int	biof, inoff, outoff, nbytes, cptr, cn
int	dtype, nelem, tbcigi(), k

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
	   nelem = tbcigi (cptr, TBL_COL_LENDATA)
	   if (dtype < 0) {
	      nelem = -dtype
	      dtype = TY_CHAR
	   }
	   switch (dtype) {
	   case TY_SHORT:
	       inoff =  inoff + nbytes
	       outoff = 1
	       # See if data comes from a BYTE FITS column.
	       if (Memi[pc+cn] == BYTE2SHORT) {    # Yes, is byte.
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
	       if (Memi[pc+cn] == SHORT2INT) {  # Is short --> int convertion.
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_INT)
	       } else {
	          nbytes = nelem*SZ_INT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_INT, TY_INT)
	       }
	       call scale_datai (ext, tp, cn+1, cptr, icbuf, nelem, rownum)
	   case TY_REAL:
	       inoff =  inoff + nbytes
	       outoff = 1
	       if (Memi[pc+cn] == SHORT2REAL) {
	          nbytes = nelem*SZ_SHORT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_SHORT, TY_REAL)
	       } else if (Memi[pc+cn] == INT2REAL) {
	          nbytes = nelem*SZ_INT*SZB_CHAR
	          call bytmov (inbbuf, inoff, wcbuf, outoff, nbytes)
	          call miiupk (wcbuf, icbuf, nelem, MII_INT, TY_REAL)
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
	       call tbrptt (tp, cptr, wcbuf, nelem, 1, rownum)
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
pointer ext,tp
int	col, colptr
short	ibuf[ARB]
int	nelem, rownum

pointer ps,pz
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
pointer ext, tp
int	col, colptr
char    icbuf[ARB]
int	nelem, rownum

pointer ps,pz, sp, rb
int	k
pointer ibuf			# a copy of icbuf, but declared to be int
include "rfits.com"
begin
	call smark(sp)
	call salloc (ibuf, nelem, TY_INT)
	call cpy_xxi (icbuf, Memi[ibuf], nelem)

	pz = EXT_PZERO(ext)
	ps = EXT_PSCAL(ext)

  	if ((scale == YES) &&
	   (Memd[pz+col-1] != 0.0d0 || Memd[ps+col-1] != 1.0d0)) {
           if (Memd[ps+col-1] != 1.0d0) {
	      # Columns datatype has been converted to real only
	      # if the TSCAL was different from 1.0. (see tab_rheader)
	      call salloc(rb , nelem, TY_REAL)
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
pointer ext, tp
int	col, colptr
char    icbuf[ARB]
int	nelem, rownum

pointer ps,pz
int	k
pointer sp
pointer rbuf			# a copy of icbuf, but declared to be real
include "rfits.com"
begin
	call smark(sp)
	call salloc (rbuf, nelem, TY_REAL)
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
pointer ext,tp
int	col,nelem, rownum, colptr
char    icbuf[ARB]

pointer ps,pz
int	k
pointer sp
pointer dbuf			# a copy of icbuf, but declared to be double
include "rfits.com"
begin
	call smark(sp)
	call salloc (dbuf, nelem, TY_DOUBLE)
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
int	nelem
#--
int	i, j, k
int	ratio
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
int	nelem
#--
int	i, j, k
int	ratio
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
int	nelem
#--
int	i, j, k
int	ratio
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
