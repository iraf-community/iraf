include <imio.h>
include <tbset.h>
include "gi.h"

define	SZ_KEYWORD	8
# GIOPN_TABLE -- Procedure to open a table and define the columns
# with the group parameter information from the input geis image
# descriptor. 

procedure giopn_table (tname, im, tp, colptr)

char	tname[SZ_FNAME]		# i: Table name
pointer	im			# i: image descriptor (stf format)
pointer	tp			# o: Table descriptor
int	colptr[ARB]			# o: pointer to each column

pointer	pp
int	i, pcount, tbtopn(), stf

begin

	stf = IM_KDES(im)
	pcount = STF_PCOUNT(stf)

	tp = tbtopn (tname, NEW_FILE, 0)
	call tbpset (tp, TBL_MAXCOLS, pcount)

	# setup maximun number of user parameter
	call tbpset (tp, TBL_MAXPAR, pcount+3)
	call tbtcre (tp)

	do i = 1, pcount {
	   pp = STF_PDES(stf,i)
	   switch (P_SPPTYPE(pp)) {
	   case TY_BOOL:
		# Declare 1 byte for a value T or F.
		call tbcdef (tp, colptr[i], P_PTYPE(pp), "", "%1b", 
			   TY_BOOL, 1, 1)
	   case TY_SHORT:	
		# Put "INTEGER*2" string in TUNIT that is in P_PDTYPE(pp).
		# Strfits (tab_rheader) will look for this string and
		# reset the column type from I4 (tform=i11) to I2.
		call tbcdef (tp, colptr[i], P_PTYPE(pp), P_PDTYPE(pp), "", 
			     TY_SHORT, 1, 1)
	   case TY_INT:
		call tbcdef (tp, colptr[i], P_PTYPE(pp), "", "", TY_INT, 1, 1)
	   case TY_LONG:
		call tbcdef (tp, colptr[i], P_PTYPE(pp), "", "", TY_INT, 1, 1)
	   case TY_REAL:
		call tbcdef (tp, colptr[i], P_PTYPE(pp), "", "", TY_REAL, 1, 1)
	   case TY_DOUBLE:
		call tbcdef (tp, colptr[i], P_PTYPE(pp), "", "",
			     TY_DOUBLE, 1, 1)
	   case TY_CHAR:
		call tbcdef (tp, colptr[i], P_PTYPE(pp), P_PDTYPE(pp), "",
			     -P_LEN(pp), 1, 1)
	   default:
		call error (1, "Illegal group data parameter datatype")
	   }
	   # Add user parameter with the PTYPE comment string
	   call tbhadt (tp, P_PTYPE(pp), P_COMMENT(pp))
	}
end
