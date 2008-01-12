include	<tbset.h>

#  TICH  --  Copy data from input header into scalar cell in output.
#
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)


procedure ticht  (itp, ihc, otp, ocp, orow, maxch)

pointer	itp		# i: input table descriptor
int	ihc		# i: header keyword index
pointer	otp		# i: output table descriptor
pointer	ocp		# i: output column descriptor
int	orow		# i: row where to insert
int	maxch
#--
pointer	buf
pointer	sp, kwname, kwval
int	datatype, parnum

string	corrupt  "Corrupted header in input table."

int	nscan()

begin
	call smark (sp)
	call salloc (buf, maxch + 1, TY_CHAR)
	call salloc (kwname, SZ_LINE, TY_CHAR)
	call salloc (kwval,  SZ_PARREC, TY_CHAR)

	# Build keyword name and look for it.
	call sprintf (Memc[kwname], SZ_LINE, "TCV_%03d")
	    call pargi (ihc)
	call tbhfkr (itp, Memc[kwname], datatype, Memc[kwval], parnum)

	# Parse and read value. We assume that the keyword existence 
	# was confirmed by previously finding the paired TCD_ keyword.
	if (parnum > 0) {
	    call sscan (Memc[kwval])
	    call gargwrd (buf, maxch) 
	    if (nscan() < 1) call error (1, corrupt)
	} else
	    call error (1, corrupt)

	# Write value into scalar cell.
	call tbcptt  (otp, ocp, buf, maxch, orow, orow)

	call sfree (sp)
end
