# Copyright restrictions apply - see tables$copyright.tables 
# 
include <imio.h>
include <mach.h>
include <tbset.h>
include	"gf.h"

# GF_CRGPB  -- Procedure to extract values from table columns
# convert them to binary values into character buffer; this buffer will
# eventually be written to the end of the data portion as a gpb.
# No provision is made to detect undefined values in the character
# string.

procedure gi_crgpb (im, buf, tbcol, pw, group)

pointer im
char	buf[ARB]	# i: input string buffer
int	tbcol[ARB]	# i: table column starting positions.
int	pw[ARB]		# i: table column width
int	group		# i: current group number

long	offset
pointer	sp, stf, gpb, lbuf, pp, op, ip, ival
int	biof, pfd, pn, sz_param, sz_gpb, i, nch

int	ctol(), ctor(), ctod(), ctoi(), strncmp(),pcount
bool	bval
# changed to short and long for short integers in gpb
short	sval
long	lval
#
real	rval
double	dval
include "tab.com"
int	pc
errchk	seek

string	writerr "cannot update group parameter block"
string	badtype "illegal group data parameter datatype"

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	stf = IM_KDES(im)
	pfd = STF_PFD(stf)

	pcount = STF_PCOUNT(stf)

	# Allocate a buffer for the GPB.
	sz_gpb = STF_PSIZE(stf) / NBITS_BYTE / SZB_CHAR
	call salloc (gpb, sz_gpb, TY_CHAR)

	# Extract the binary value of each parameter in the GPB and encode it
	# in FITS format in the IMIO user area.
	
	if (wf2_49 == 49)     # Interim: this is for wfpc2 data with 49 gp.
	   pcount = pcount + 3
	offset = 0
	pc = 1
#	for (pn=1;  pn <= STF_PCOUNT(stf);  pn=pn+1) {
	for (pn=1;  pn <= pcount;  pn=pn+1) {
	    pp = STF_PDES(stf,pc)
	    op = gpb + offset

	    if (wf2_49 == 49 && (pn == 44 || pn == 46 || pn == 48))
	       next
	    pc = pc + 1
	    # get position of first character and length of column
	    biof = tbcol[pn]

	    ip = 1
	    switch (P_SPPTYPE(pp)) {
	    case TY_BOOL:
		bval = false
		if (strncmp (buf[biof+pw[pn]-1], "T", 1) == 0)
		   bval = true
		call amovc (bval, Memc[op], SZ_BOOL)

	    case TY_SHORT:
		nch = ctoi (buf[biof], ip, ival)
		sval = ival
		call amovc (sval, Memc[op], SZ_SHORT)

	    case TY_INT,TY_LONG:
		nch = ctol (buf[biof], ip, lval)
		call amovc (lval, Memc[op], SZ_LONG)

	    case TY_REAL:
		nch = ctor (buf[biof], ip, rval)
		# Memr[(op-1)/SZ_REAL+1] = rval
		call amovc (rval, Memc[op], SZ_REAL)

	    case TY_DOUBLE:
		nch = ctod (buf[biof], ip, dval)
		# Memd[(op-1)/SZ_DOUBLE+1] = dval
		call amovc (dval, Memc[op], SZ_DOUBLE)

	    case TY_CHAR:
		# Blank fill the string buffer.
		do i = 1, P_LEN(pp)
		    Memc[lbuf+i-1] = ' '

		call strcpy (buf[biof], Memc[lbuf], P_LEN(pp))
		# Pack the blank filled array into the GPB.
		call chrpak (Memc[lbuf], 1, Memc[gpb+offset], 1, P_LEN(pp))

	    default:
		call error (1, badtype)
	    }

	    sz_param = P_PSIZE(pp) / NBITS_BYTE / SZB_CHAR
	    offset = offset + sz_param
	}

	# Write the GPB into the pixfile.  The GPB is located at the very end
	# of the data storage area for the group .

	offset = (group * STF_SZGROUP(stf) + 1) - sz_gpb
	call seek (pfd, offset)
	iferr (call write (pfd, Memc[gpb], sz_gpb))
	    call error (5, writerr)

	call sfree (sp)
end
