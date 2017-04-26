# PSORT - Sort an ST or APPHOT/DAOPHOT file based on a single column.

procedure psort (infiles, field)

string	infiles       {prompt="Input Apphot/daophot database(s) to be sorted"}
string	field	      {prompt="Field to be sorted on"}
bool	ascend        {yes, prompt="Sort in increasing value order?"}

struct	*inlist

begin
	# Local variable declarations.
	file	tmpin
	string	in, col, inname

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infiles
	col = field

	# Expand the file list names.
	tmpin = mktemp ("tmp$")
	files (in, sort=no, > tmpin)

	# Loop over each file in the input and output lists selecting records.
	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    istable (inname)
	    if (istable.table) {
		tsort (inname, col, ascend=ascend, casesens=yes)
	    } else if (istable.text) {
		txsort (inname, col, ascend=ascend)
	    } else {
		print ("Cannot run PSORT on file: " // inname)
	    }
	}
	inlist = ""

	delete (tmpin, ver-, >& "dev$null")
end
