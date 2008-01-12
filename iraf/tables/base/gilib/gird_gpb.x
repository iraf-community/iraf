include <imio.h>
include <tbset.h>
include "gi.h"

# GIRD_GPB -- Procedure to read gpb values from the input table columns.
# The resulting values then replace those in the imuserarea for the 
# current group number

procedure gird_gpb (tp, gn, im)

pointer	tp		# i: table descriptor
int	gn		# i: group number, or table line number
pointer	im		# i: output image descriptor

pointer	pp
int	stf, i, ncols, tbpsta(), tbcnum(), colp

bool	bbuf
short	sbuf
int	ibuf
long	lbuf
real	rbuf
double	dbuf
char	cbuf[SZ_LINE]
string	badtype "illegal group data parameter datatype"

begin
	   stf = IM_KDES (im)
	   ncols = tbpsta (tp, TBL_NCOLS)

	   do i = 1, ncols {
	      colp = tbcnum (tp, i)
	      pp = STF_PDES(stf,i)
	      switch (P_SPPTYPE(pp)) {
	      case TY_BOOL:
		  call tbegtb (tp, colp, gn, bbuf)
		  call imaddb (im, P_PTYPE(pp), bbuf)
	      case TY_SHORT:
		  call tbegti (tp, colp, gn, ibuf)
		  sbuf = ibuf
		  call imadds (im, P_PTYPE(pp), sbuf)
	      case TY_LONG:
		  call tbegti (tp, colp, gn, ibuf)
		  lbuf = ibuf
		  call imaddl (im, P_PTYPE(pp), lbuf)
	      case TY_REAL:
		  call tbegtr (tp, colp, gn, rbuf)
		  call imaddr (im, P_PTYPE(pp), rbuf)
	      case TY_DOUBLE:
		  call tbegtd (tp, colp, gn, dbuf)
		  call imaddd (im, P_PTYPE(pp), dbuf)
	      case TY_CHAR:
		  call tbegtt (tp, colp, gn, cbuf, P_LEN(pp))
		  call imastr (im, P_PTYPE(pp), cbuf)
	      default:
		  call error (1, badtype)
	      }
	   }
end
