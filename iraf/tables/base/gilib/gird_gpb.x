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

long	l_gn
int	i, ncols
pointer	stf, pp, colp

bool	bbuf
short	sbuf
int	ibuf
long	lbuf
real	rbuf
double	dbuf
char	cbuf[SZ_LINE]

int	tbpsta()
pointer	tbcnum()

string	badtype "illegal group data parameter datatype"

begin
	   l_gn = gn

	   stf = IM_KDES (im)
	   ncols = tbpsta (tp, TBL_NCOLS)

	   do i = 1, ncols {
	      colp = tbcnum (tp, i)
	      pp = STF_PDES(stf,i)
	      switch (P_SPPTYPE(pp)) {
	      case TY_BOOL:
		  call tbegtb (tp, colp, l_gn, bbuf)
		  call imaddb (im, P_PTYPE(pp), bbuf)
	      case TY_SHORT:
		  call tbegti (tp, colp, l_gn, ibuf)
		  sbuf = ibuf
		  call imadds (im, P_PTYPE(pp), sbuf)
	      case TY_INT:
		  call tbegti (tp, colp, l_gn, ibuf)
		  call imaddi (im, P_PTYPE(pp), ibuf)
	      case TY_LONG:
		  call tbegtl (tp, colp, l_gn, lbuf)
		  call imaddl (im, P_PTYPE(pp), lbuf)
	      case TY_REAL:
		  call tbegtr (tp, colp, l_gn, rbuf)
		  call imaddr (im, P_PTYPE(pp), rbuf)
	      case TY_DOUBLE:
		  call tbegtd (tp, colp, l_gn, dbuf)
		  call imaddd (im, P_PTYPE(pp), dbuf)
	      case TY_CHAR:
		  call tbegtt (tp, colp, l_gn, cbuf, P_LEN(pp))
		  call imastr (im, P_PTYPE(pp), cbuf)
	      default:
		  call error (1, badtype)
	      }
	   }
end
