# TBRENUMBER -- Renumber the ID column of an APPHOT/DAOPHOT STSDAS table
# database.

procedure tbrenumber (tables)

file	tables {prompt="Input apphot/daophot tables databases to be renumbered"}
string	id     {"ID", prompt="Id name keyword"}

struct	*inlist

begin
	# Declare local variables.
	file ttables
	string tmpin, inname

	# Get the positional parameters.
	ttables = tables

	tmpin = mktemp ("tmp$")
	files (ttables, sort=no, > tmpin)

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    if (defpar ("tcalc.verbose") || defpar ("tcalc.harmless")) {
	        tcalc (inname, id, "rownum", datatype="real", colunits="",
	            colfmt="", verbose=no, harmless=0.1)
	    } else {
	        tcalc (inname, id, "rownum", datatype="real", colunits="",
	            colfmt="")
	    }
	}
	inlist = ""
end
