# PSELECT - Select records from an ST table of an APPHOT/DAOPHOT text file
# based on the value of a boolean expression.

procedure pselect (infiles, outfiles, expr)

string	infiles		{prompt="Input apphot/daophot database(s)"}
string	outfiles	{prompt="Output apphot/daophot database(s)"}
string	expr		{prompt="Boolean expression for record selection"}

struct	*inlist, *outlist

begin
	# Local variable declarations.
	file	tmpin, tmpout
	int	nin, nout
	string	in, out, ex, inname, outname

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infiles
	out = outfiles
	ex = expr

	# Make temporary names.
	tmpin = mktemp ("tmp$")
	tmpout = mktemp ("tmp$")

	# Expand the file list names.
	files (in, sort=no, > tmpin)
	files (out, sort=no, > tmpout)

	# Compute the lengths of the input and output lists.
	inlist = tmpin
	for (nin = 0; fscan (inlist, inname) != EOF; nin = nin + 1)
	    ;
	inlist = ""
	outlist = tmpout
	for (nout = 0; fscan (outlist, outname) != EOF; nout = nout + 1)
	    ;
	outlist = ""

	# Delete the temporary files.
	delete (tmpin, ver-, >& "dev$null")
	delete (tmpout, ver-, >& "dev$null")

	# Quit if the number of input and output files is different.
	if (nin != nout) {
	    print ("ERROR: Input and output file lists are different lengths")
	    return
	}

	# Expand the file list names.
	files (in, sort=no, > tmpin)
	files (out, sort=no, > tmpout)

	# Loop over each file in the input and output lists selecting records.
	inlist = tmpin
	outlist = tmpout
	while (fscan (inlist, inname) != EOF && fscan (outlist, outname) !=
	    EOF) {
	    istable (inname)
	    if (istable.table) {
		tselect (inname, outname, ex)
	    } else if (istable.text) {
		txselect (inname, outname, ex)
	    } else {
		print ("ERROR: Cannot run PSELECT on file: " // inname)
	    }
	}

	delete (tmpin, ver-, >& "dev$null")
	delete (tmpout, ver-, >& "dev$null")
	inlist = ""
	outlist = ""
end
