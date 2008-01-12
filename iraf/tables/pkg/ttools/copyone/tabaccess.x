# TABACCESS -- Test to see if an table is accessible with the given access
# mode. Return the result of the test as YES or NO.
#
# B.Simon	12-Aug-87	First Code
# B.Simon	19-Jun-95	Revised to use tbtacc

int procedure tabaccess (tablename, acmode)

char	tablename[ARB]	# i: table file name
int	acmode		# i: access mode
#--
int	tbtacc()

begin
	if (acmode == NEW_FILE || acmode == NEW_COPY)
	    return (YES)

	return (tbtacc (tablename))
end
