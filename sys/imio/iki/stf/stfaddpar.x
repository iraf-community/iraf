# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include "stf.h"

# STF_APAR -- Encode a parameter in FITS format and add it to the FITS format
# IMIO user area; initialize the entry for the parameter in the GPB descriptor
# as well.

procedure stf_addpar (im, pname, dtype, plen, pval, pno)

pointer	im			# image descriptor
char	pname[ARB]		# parameter name
int	dtype			# SPP datatype of parameter
int	plen			# length (> 1 if array)
char	pval[ARB]		# string encoded parameter value
int	pno			# [in/out] parameter number

real	rval
double	dval
short	sval			# added to allow i*2 values from RSDP
long	lval			# added to allow i*2 values from RSDP
pointer	pp, stf
int	ival, ip, junk
int	ctoi(), ctor(), ctod()
errchk	imadds, imaddl, imaddr, imaddd, imastr

begin
	stf = IM_KDES(im)
	pp  = STF_PDES(stf,pno)
	ip  = 1

	call strcpy (pname, P_PTYPE(pp), SZ_PTYPE)

	switch (dtype) {
	case TY_SHORT:
	    call strcpy ("INT*2", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_SHORT * SZB_CHAR * NBITS_BYTE
	    junk = ctoi (pval, ip, ival)
	    sval = ival
	    call imadds (im, P_PTYPE(pp), sval)
	case TY_LONG:
	    call strcpy ("INT*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_LONG * SZB_CHAR * NBITS_BYTE
	    junk = ctoi (pval, ip, ival)
	    lval = ival
	    call imaddl (im, P_PTYPE(pp), lval)
	case TY_REAL:
	    call strcpy ("REAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_REAL * SZB_CHAR * NBITS_BYTE
	    junk = ctor (pval, ip, rval)
	    call imaddr (im, P_PTYPE(pp), rval)
	case TY_DOUBLE:
	    call strcpy ("REAL*8", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_DOUBLE * SZB_CHAR * NBITS_BYTE
	    junk = ctod (pval, ip, dval)
	    call imaddd (im, P_PTYPE(pp), dval)
	default:
	    call sprintf (P_PDTYPE(pp), SZ_PDTYPE, "CH*%d")
		call pargi (plen)
	    P_PSIZE(pp) = plen * NBITS_BYTE
	    call imastr (im, P_PTYPE(pp), pval)
	}

	P_OFFSET(pp) = 0
	P_SPPTYPE(pp) = dtype
	P_LEN(pp) = plen

	pno = pno + 1
end
