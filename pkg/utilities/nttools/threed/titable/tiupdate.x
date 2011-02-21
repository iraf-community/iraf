include	<tbset.h>

#  TIUPDATE  --  Opens an already existing output table for update.
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Replaced code by tbcnum call (IB)


procedure tiupdate (output, otp, cpo, ncpo)

char	output[ARB]	# i: table name
pointer	otp		# o: table descriptor
pointer	cpo		# o: column descriptor
int	ncpo		# o: number of columns
#--
int	i

errchk	tbtopn

pointer	tbtopn(), tbcnum()
int	tbpsta()

begin
	# Open table and get its size.
	otp  = tbtopn (output, READ_WRITE, NULL)
	ncpo = tbpsta (otp, TBL_NCOLS)

	# Alloc column descriptor array. This
	# must be freed by caller.
	call malloc (cpo, ncpo, TY_INT)

	# Fill array with column info.
	do i = 1, ncpo
	    Memi[cpo+i-1] = tbcnum (otp, i)
end
