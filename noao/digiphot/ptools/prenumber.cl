# PRENUMBER - Renumber the ID column of an APPHOT/DAOPHOT database from 1 to
# N where N is the number of objects in the database. The renumbering is
# done in place.

procedure prenumber (infile)

string	infile     {prompt="Input apphot/daophot databases(s) to be renumbered"}
string	id	   {"ID", prompt="Id name keyword"}

struct	*inlist

begin
	# Local variable declarations.
	file	tmpin
	string	in, inname

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infile

	# Expand the file list names.
	tmpin = mktemp ("tmp$")
	files (in, sort=no, > tmpin)

	# Loop over each file in the input and output lists selecting records.
	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    istable (inname)
	    if (istable.table) {
		if (defpar ("tcalc.verbose") || defpar ("tcalc.harmless")) {
		    tcalc (inname, id, "rownum", datatype="real", colunits="",
		        colfmt="", verbose=no, harmless=0.1)
		} else {
		    tcalc (inname, id, "rownum", datatype="real", colunits="",
		        colfmt="")
		}
	    } else if (istable.text) {
		txrenumber (inname, id=id)
	    } else {
		print ("Cannot run RENUMBER on file: " // inname)
	    }
	}
	inlist = ""

	delete (tmpin, ver-, >& "dev$null")
end
