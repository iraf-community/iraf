# PCALC - Recompute a column of an APPHOT/DAOPHOT database using an
# arithmetic expression.

procedure pcalc (infile, field, value)

string	infile     {prompt="Input apphot/daophot databases(s)"}
string	field	   {prompt="Field to be edited"}
string	value	   {prompt="New value or expression for field"}

struct	*inlist

begin
	# Local variable declarations.
	file	tmpin
	string	in, tfield, tvalue, inname

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infile
	tfield = field
	tvalue = value

	# Expand the file list names.
	tmpin = mktemp ("tmp$")
	files (in, sort=no, > tmpin)

	# Loop over each file in the input and output lists selecting records.
	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    istable (inname)
	    if (istable.table) {
		if (defpar ("tcalc.verbose") || defpar ("tcalc.harmless")) {
		    tcalc (inname, tfield, tvalue, datatype="real",
		        colunits="", colfmt="", verbose=no, harmless=0.1)
		} else {
		    tcalc (inname, tfield, tvalue, datatype="real",
		        colunits="", colfmt="")
		}
	    } else if (istable.text) {
		txcalc (inname, tfield, tvalue)
	    } else {
		print ("Cannot run PCALC on file: " // inname)
	    }
	}
	inlist = ""

	delete (tmpin, ver-, >& "dev$null")
end
