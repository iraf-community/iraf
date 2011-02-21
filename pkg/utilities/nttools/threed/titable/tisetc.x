
#  TISETC  --  Set column info in new output table.
#
#
#
#
#
#  Revision history:
#  ----------------
#  20-Jan-97  -  Task created (I.Busko)
#  17-Mar-97  -  Revised after code review (IB)


procedure tisetc (cpo, newcpo, ncpo, nscalar, itp, otp, colname, colunits, 
                  colfmt, csize, template)

pointer	cpo		# i:  array of column descriptors 
pointer	newcpo		# io: new array of column descriptors 
int	ncpo		# i:  number of columns matched by selector
int	nscalar		# i:  number of scalar columns
char	colname[ARB]	# i:  work array for column names
char	colunits[ARB]	# i:  work array for column units
char	colfmt[ARB]	# i:  work array for column format
pointer	itp,otp		# io: template and output table descriptors
int	csize		# i:  cell size in output table
bool	template	# i:  is there a template ?
#--
pointer	ocp
int	i, j, colnum, ntot
int	datatype, lendata, lenfmt

errchk	tihdec

pointer	tcs_column()
int	tihmax()
bool	tihdec()

begin
	# First copy column information from template/input 
	# table into output table.
	do i = 1, ncpo {
	    ocp = tcs_column (Memi[cpo+i-1])
	    if (!template) {

	        # Template wasn't supplied; copy column info from 2-D 
	        # input table into 3-D output table, taking care of 
	        # resetting the array size. 
	        call tbcinf (ocp, colnum, colname, colunits, colfmt, 
                            datatype, lendata, lenfmt)
	        if (lendata > 1)
	            call error (1, "Input table has array element !")
	        call tbcdef (otp, ocp, colname, colunits, colfmt, 
                             datatype, csize, 1)
	    } else {

	        # Copy with same array  size configuration, since
	        # template is supposedly a 3-D table.
	        call tbcinf (ocp, colnum, colname, colunits, colfmt, 
                             datatype, lendata, lenfmt)
	        call tbcdef (otp, ocp, colname, colunits, colfmt, 
                             datatype, lendata, 1)
	    }

	    # Save column pointer.
	    Memi[newcpo+i-1] = ocp
	}

	# If header-stored scalars exist, define new columns for them.
	if (nscalar > 0) {
	    ntot = tihmax (itp)
	    i = ncpo
	    do j = 1, ntot {
	        if (tihdec (itp, j, colname, colunits, colfmt, datatype,
                            lenfmt)) {
	            call tbcdef (otp, ocp, colname, colunits, colfmt,
                                datatype, 1, 1)
	            Memi[newcpo+i] = ocp
	            i = i + 1
	        }
	    }
	}
end

