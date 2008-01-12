include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include "gi.h"

define  SZ_KEYWORD  8
# GI_PDES -- Procedure to setup the stf parameter descriptor PDES
# with parameter name, datatype and parameter number.

procedure gi_pdes (im, pname, dtype, plen, pno)

pointer	im		# Image descriptor
char    pname[SZ_KEYWORD]	# Parameter name
int	dtype		# Parameter value datatype
int	plen		# value len
int	pno		# Parameter number.

pointer	pp, stf
int	ip
errchk	imadds, imaddl, imaddr, imaddd, imastr


begin

	if (dtype < 0) {
	   plen = -dtype
	   dtype = TY_CHAR
	} else {
	   plen = 1
	   if (dtype == TY_INT)
	      dtype = TY_LONG
	}

#	call stf_addpar (im, pname, dtype, len, " ", pno)

	stf = IM_KDES(im)
	pp  = STF_PDES(stf,pno)
	ip  = 1

	call strcpy (pname, P_PTYPE(pp), SZ_PTYPE)

	switch (dtype) {
	case TY_BOOL:
	    call strcpy ("LOGICAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_BOOL * SZB_CHAR * NBITS_BYTE
	    call imaddb (im, P_PTYPE(pp), 'F')
	case TY_SHORT:
	    call strcpy ("INTEGER*2", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_SHORT * SZB_CHAR * NBITS_BYTE
	    call imadds (im, P_PTYPE(pp), " ")
	case TY_LONG:
	    call strcpy ("INTEGER*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_LONG * SZB_CHAR * NBITS_BYTE
	    call imaddl (im, P_PTYPE(pp), 0)
	case TY_REAL:
	    call strcpy ("REAL*4", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_REAL * SZB_CHAR * NBITS_BYTE
	    call imaddr (im, P_PTYPE(pp), 0.0)
	case TY_DOUBLE:
	    call strcpy ("REAL*8", P_PDTYPE(pp), SZ_PDTYPE)
	    P_PSIZE(pp) = plen * SZ_DOUBLE * SZB_CHAR * NBITS_BYTE
	    call imaddd (im, P_PTYPE(pp), 0.0d0)
	default:
	    call sprintf (P_PDTYPE(pp), SZ_PDTYPE, "CHARACTER*%d")
		call pargi (plen)
	    P_PSIZE(pp) = plen * NBITS_BYTE
	    call imastr (im, P_PTYPE(pp), " ")
	}

	P_OFFSET(pp) = 0
	P_SPPTYPE(pp) = dtype
	P_LEN(pp) = plen

	pno = pno + 1
end
