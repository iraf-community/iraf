include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include "gi.h"

# GI_ADDPAR -- Add a parameter in FITS format and add it to the FITS format
# IMIO user area; initialize the entry for the parameter in the GPB descriptor
# as well.

procedure gi_addpar (im, pname, dtype, plen, pval, pcomm)

pointer	im			#I image descriptor
char	pname[ARB]		#I parameter name
int	dtype			#I SPP datatype of parameter
int	plen			#I length (> 1 if array)
char	pval[ARB]		#I string encoded initial parameter value
char	pcomm[ARB]		#I string comment to the new parameter

bool	bval
real	rval
double	dval
short	sval
long	lval
pointer	pp, stf

bool	initparam
int	ival, ip, junk, pnum, i, strncmp()
int	ctoi(), ctor(), ctod(), imaccf()
errchk	imadds, imaddl, imaddr, imaddd, imastr

begin
	stf = IM_KDES(im)
	pnum = STF_PCOUNT(stf) + 1
	
	# See if the parameter already exist
	for (i=1; i < pnum; i=i+1) {
	    pp = STF_PDES(stf,i)
	    if (strncmp(pname, P_PTYPE(pp), SZ_PTYPE) == 0)
	       return
        }	   
	# add 1 more gpb parameter
	#
	STF_PCOUNT(stf) = pnum

	# Get memory for it
	if (STF_PCOUNT(stf) > 0) {
	   call realloc (stf,
	        LEN_STFBASE + STF_PCOUNT(stf) * LEN_PDES, TY_STRUCT)
	   IM_KDES(im) = stf
	}

	pp  = STF_PDES(stf,pnum)
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
	case TY_LONG, TY_INT:
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

	STF_PSIZE(stf) =  STF_PSIZE(stf) + P_PSIZE(pp)
	STF_SZGROUP(stf) = STF_SZGROUP(stf) + 
			P_PSIZE(pp) / (SZB_CHAR * NBITS_BYTE)

	P_OFFSET(pp) = 0
	if (dtype == TY_INT) dtype = TY_LONG
	P_SPPTYPE(pp) = dtype
	P_LEN(pp) = plen
	call strcpy(pcomm, P_COMMENT(pp), FITS_SZCOMMENT)
end
