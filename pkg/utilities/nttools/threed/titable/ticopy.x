include	<tbset.h>

#  TICOPY  --  Copy input table into row of output table
#
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Revised after code review (IB)


procedure ticopy (itp, cpi, ncpi, otp, cpo, ncpo, rowsel, row, nrows,
                  coln, colu, colf)

pointer	itp		# i: input table descriptor
pointer	cpi		# i: input column descriptor array
int	ncpi		# i: input number of columns
pointer	otp		# i: output table descriptor
pointer	cpo		# i: output column descriptor array
int	ncpo		# i: output number of columns
char	rowsel[ARB]	# i: work string for row selector
int	row		# i: row where to begin insertion
int	nrows		# i: number of selected rows
char	coln[ARB]	# i: work string for column names
char	colu[ARB]	# i: work string for column units
char	colf[ARB]	# i: work string for column formats
#--
pointer	sp, coln2, colu2, colf2, icp, ocp
int	icpi, icpo, dum, dtypi, dtypo, maxlen
int	ihc, maxhc
bool	found

errchk	ticc

pointer	tcs_column()
int	tbalen(), tihmax()
bool	streq(), tihdec()

begin
	call smark (sp)
	call salloc (coln2, SZ_COLNAME,  TY_CHAR)
	call salloc (colu2, SZ_COLUNITS, TY_CHAR)
	call salloc (colf2, SZ_COLFMT,   TY_CHAR)

	# Loop over output table column pointers.
	do icpo = 1, ncpo {

	    # Get column name and data type from output table.
	    ocp = Memi[cpo+icpo-1]
	    call tbcinf (ocp, dum, coln, colu, colf, dtypo, dum, dum)

	    # Array length must be the minimum in between table array 
            # size and the number of rows selected from input table. 
	    maxlen = min (tbalen(ocp), nrows)

	    # If there are matched columns, loop over 
	    # input table column pointers.
	    found = false
	    do icpi = 1, ncpi {

	        # Get column name and data type from input table.
	        icp = tcs_column (Memi[cpi+icpi-1])
	        call tbcinf (icp,dum,Memc[coln2],colu,colf,dtypi,dum,dum)

	        # If column names match, copy from table to table.
	        if (streq (coln, Memc[coln2])) {
	            # For now, abort if datatypes do not match.
	            if (dtypo != dtypi)
	                call error (1, "Data types do not match.")
	            call ticc (itp,icp,otp,ocp,dtypo,maxlen,rowsel,row)
	            found = true
	        }
	    }

	    # If column was not found, look into header.
	    if (!found) {
	        maxhc = tihmax (itp)
	        do ihc = 1, maxhc {
	            if (tihdec (itp, ihc, Memc[coln2], Memc[colu2], 
                                Memc[colf2], dtypi, dum)) {
	                if (streq (coln, Memc[coln2])) {

	                    # For now, abort if datatypes do not match.
	                    if (dtypo != dtypi)
	                        call error (1, "Data types do not match.")
	                    if (dtypo < 0)
	                        dtypo = TY_CHAR

	                    switch (dtypo) {
	                    case TY_CHAR:
	                        call ticht (itp, ihc, otp, ocp, row, -dtypi)
	                    case TY_BOOL:
	                        call tichb (itp, ihc, otp, ocp, row)
	                    case TY_SHORT:
	                        call tichs (itp, ihc, otp, ocp, row)
	                    case TY_INT,TY_LONG:
	                        call tichi (itp, ihc, otp, ocp, row)
	                    case TY_REAL:
	                        call tichr (itp, ihc, otp, ocp, row)
	                    case TY_DOUBLE:
	                        call tichd (itp, ihc, otp, ocp, row)
	                    default:
	                        call error (1, "Non-supported data type.")
	                    }
	                }
	            }
	        }
	    }
	}

	call sfree (sp)
end


