#* HISTORY *
#* 17-Mar-97	I.Busko		created
#* 15-Jan-97	B.Simon		modified to call trsrows

# SELROWS -- Count how many rows are selected by an expression

int procedure selrows (tp, expr)

pointer	tp		# i: table descriptor
char	expr[ARB]	# i: expression to be evaluated
#--
int	nrow
pointer	set

int	rst_nelem()
pointer	trsrows()
errchk	trsrows

begin
	# Compute set of rows matching expression

	set = trsrows (tp, expr)

	# Count number of rows in set

	nrow = rst_nelem (set)

	call rst_free (set)
	return (nrow)
end
