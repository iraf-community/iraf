# TBRENUMBER -- Renumber the ID column of an APPHOT/DAOPHOT STSDAS table
# database.

procedure tbrenumber (tables)

file	tables {prompt="Input apphot/daophot tables databases to be renumbered"}
int	idoffset	{0, min=0, prompt="Id number offset"}
string	id     		{"ID", prompt="Id name keyword"}

struct	*inlist

begin
	# Declare local variables.
	file ttables
	string tmpin, inname, expr

	# Get the positional parameters.
	ttables = tables
	expr = "rownum + " // idoffset

	tmpin = mktemp ("tmp$")
	files (ttables, sort=no, > tmpin)

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    if (defpar ("tcalc.verbose") || defpar ("tcalc.harmless")) {
	        tcalc (inname, id, expr, datatype="real", colunits="",
	            colfmt="", verbose=no, harmless=0.1)
	    } else {
	        tcalc (inname, id, expr, datatype="real", colunits="",
	            colfmt="")
	    }
	}
	delete (tmpin, ver-, >& "dev$null")
	inlist = ""
end
