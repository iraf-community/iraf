# TBCALC  -- Perform arithmetic operations on a column column of an
# APPHOT/DAOPHOT STSDAS table database.

procedure tbcalc (tables, column, value)

file	tables {prompt="Input apphot/daophot tables databases to be renumbered"}
string	column {prompt="Column to be edited"}
string	value  {prompt="New value or expression for column"}

struct	*inlist

begin
	# Declare local variables.
	file ttables, tcolumn, tvalue
	string tmpin, inname

	# Get the positional parameters.
	ttables = tables
	tcolumn = column
	tvalue = value

	tmpin = mktemp ("tmp$")
	files (ttables, sort=no, > tmpin)

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    if (defpar ("tcalc.verbose") || defpar ("tcalc.harmless")) {
	        tcalc (inname, tcolumn, tvalue, datatype="real", colunits="",
	            colfmt="", verbose=no, harmless=0.1)
	    } else {
	        tcalc (inname, tcolumn, tvalue, datatype="real", colunits="",
	            colfmt="")
	    }
	}
	delete (tmpin, ver-, >& "dev$null")
	inlist = ""
end
