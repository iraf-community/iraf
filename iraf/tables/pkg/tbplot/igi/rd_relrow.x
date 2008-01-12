include <tbset.h>
include <imhdr.h>

# RD_RELROW -- Read array data from a table row within the bounds of 
#		the row selector
#	This function is based on the OM_RDARRAY function written
#	by B. Simon
#	Author: WJH, 8 May 1997

#procedure rd_relrow (tp, col, rcode, relrow, dtype, data, nelem, ncol)
procedure rd_relrow (tp, colselect, rowselect, relrow, data, nelem)
			     
pointer	tp		# i: table descriptor
#pointer col[ARB]	# i: column selectors
char	colselect[ARB]	# i: column selector
char	rowselect[ARB]	# i: row selector
int	dtype		# i: data type of output columns
int	relrow		# i: relative row number within selector bounds
#pointer	data		# o: pointers to output columns
int	nelem		# o: length of each output column
pointer data		# o: pointers to output columns
#--

int	irow, nrow, coltype, ncopy
#int 	ncol, size
int 	numcols
int	chkrow
int	first
pointer	sp, length, file
pointer	rcode		# Pointer to QPOE code for row selector
pointer cdp, x
bool	done

string	ambiguous  "More than one row matches in file"
string	badtype    "Unrecognized input datatype"
string	badsize    "All arrays are not the same length"

bool	trseval()
int	tbpsta()
#int	tcs_totsize()
errchk	trseval, om_error, tcs_rdarys, tcs_rdaryi, tcs_rdaryr, tcsrdaryd
pointer	trsopen()
int	tbcigi()
int	tbagtr(), tbagtd(), tbagti(), tbagts()

begin

	# Allocate temporary arrays

	call smark (sp)
	call salloc (length, IM_MAXDIM, TY_INT)
	call salloc (file, SZ_PATHNAME, TY_CHAR)
	
	dtype = TY_REAL
	chkrow = 1
	first = 1
	done = FALSE

	# Get table name for error messages

	call tbtnam (tp, Memc[file], SZ_PATHNAME)

	nrow = tbpsta (tp, TBL_NROWS)
	
	# Create QPOE code for row selector
	rcode = trsopen (tp, rowselect)

	do irow = 1, nrow {

	  if (trseval (tp, irow, rcode) ){

	    if (chkrow == relrow && !done) {

		numcols = tbpsta (tp, TBL_NCOLS)

		# Check if the column exists
		call tbcfnd (tp, colselect, cdp, numcols)

		# Determine which datatype is use to read the array
		coltype = tbcigi (cdp, TBL_COL_DATATYPE)

		# Read the array from the table
		ncopy  = tbcigi (cdp, TBL_COL_LENDATA)

		switch (coltype) {
		  case TY_REAL:		
		    if (tbagtr (tp, cdp, irow, Memr[data], first, ncopy) < ncopy)
			call igta_disaster (tp, "error reading input")
	
		  case TY_DOUBLE:
		    call salloc (x, ncopy, TY_DOUBLE)

		    if (tbagtd (tp, cdp, irow, Memd[x], first, ncopy) < ncopy) {
			call igta_disaster (tp, "error reading input")
		    }
		        call achtdr(Memd[x],Memr[data], ncopy)
	
		  case TY_INT:
		    call salloc (x, ncopy, TY_INT)

		    if (tbagti (tp, cdp, irow, Memi[x], first, ncopy) < ncopy)
			call igta_disaster (tp, "error reading input")
		    call achtir(Memi[x],Memr[data],ncopy)

		  case TY_SHORT:
		    call salloc (x, ncopy, TY_SHORT)

		    if (tbagts (tp, cdp, irow, Mems[x], first, ncopy) < ncopy)
			call igta_disaster (tp, "error reading input")
		    call achtsr(Mems[x],Memr[data],ncopy)

		  case TY_BOOL:
			call igta_disaster (tp, "error reading input: Data type wrong (BOOL)")			
		  case -1,-2 :		# character string
			call igta_disaster (tp, "error reading input: Data type wrong (CHAR)")	
		  default:
		    call igta_disaster (tp, "unknown data type")
		}
	
		# Check array lengths to make sure they are equal
		nelem = ncopy
		done = TRUE
	    } 
		chkrow = chkrow + 1
	  }
	}
	call trsclose(rcode)
	call sfree (sp)

# Check to see if a row has been read and 'done' was set to TRUE...
	if (!done) {
		call eprintf ("WARNING: No row within range of selection criteria. NO DATA READ IN...\n")
	}

end
