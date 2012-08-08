# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include "stf.h"

# STF_ADDPAR -- Encode a parameter in FITS format and add it to the FITS format
# IMIO user area; initialize the entry for the parameter in the GPB descriptor
# as well.

procedure stf_addpar (im, pname, dtype, plen, pval, pno)

pointer	im			#I image descriptor
char	pname[ARB]		#I parameter name
int	dtype			#I SPP datatype of parameter
int	plen			#I length (> 1 if array)
char	pval[ARB]		#I string encoded initial parameter value
int	pno			#U parameter number

bool	bval
real	rval
double	dval
short	sval
long	lval
pointer	pp, stf

bool	initparam
int	ival, ip, junk
int	ctoi(), ctor(), ctod(), imaccf()
errchk	imadds, imaddl, imaddr, imaddd, imastr

begin
	stf = IM_KDES(im)
	pp  = STF_PDES(stf,pno)
	ip  = 1

	call strcpy (pname, P_PTYPE(pp), SZ_PTYPE)

	# Initialize the parameter only if not already defined in header.
	initparam = (imaccf (im, pname) == NO)

	switch (dtype) {
	case TY_BOOL:
	    call strcpy ("LOGICAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_BOOL * SZB_CHAR * NBITS_BYTE
	    if (initparam) {
		bval = (pval[1] == 'T')
		call imaddb (im, P_PTYPE(pp), bval)
	    }
	case TY_SHORT:
	    call strcpy ("INTEGER*2", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_SHORT * SZB_CHAR * NBITS_BYTE
	    if (initparam) {
		junk = ctoi (pval, ip, ival)
		sval = ival
		call imadds (im, P_PTYPE(pp), sval)
	    }
	case TY_LONG:
	    call strcpy ("INTEGER*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_LONG * SZB_CHAR * NBITS_BYTE
	    if (initparam) {
		junk = ctoi (pval, ip, ival)
		lval = ival
		call imaddl (im, P_PTYPE(pp), lval)
	    }
	case TY_REAL:
	    call strcpy ("REAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_REAL * SZB_CHAR * NBITS_BYTE
	    if (initparam) {
		junk = ctor (pval, ip, rval)
		call imaddr (im, P_PTYPE(pp), rval)
	    }
	case TY_DOUBLE:
	    call strcpy ("REAL*8", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_DOUBLE * SZB_CHAR * NBITS_BYTE
	    if (initparam) {
		junk = ctod (pval, ip, dval)
		call imaddd (im, P_PTYPE(pp), dval)
	    }
	default:
	    call sprintf (P_PDTYPE(pp), SZ_PDTYPE, "CHARACTER*%d")
		call pargi (plen)
	    P_PSIZE(pp) = plen * NBITS_BYTE
	    if (initparam)
		call imastr (im, P_PTYPE(pp), pval)
	}

	P_OFFSET(pp) = 0
	P_SPPTYPE(pp) = dtype
	P_LEN(pp) = plen

	pno = pno + 1
end
