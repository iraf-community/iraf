# PDUMP - Select fields from an ST table or an APPHOT/DAOPHOT text file
# and dump to a text file.

procedure pdump (infiles, fields, expr)

string	infiles		{prompt="Input apphot/daophot databases(s)"}
string	fields		{prompt="Fields to be extracted"}
string	expr		{"yes", prompt="Boolean expression"}
bool	headers		{no, prompt="Print field headers?"}
bool	parameters	{yes, prompt="Print parameters?"}

struct	*inlist

begin
	# Local variable declarations.
	file	tmpin, texpr
	int	nin, tpagwidth
	string	in, col, inname, hfile, pfile

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infiles
	col = fields
	texpr = expr

	# Make temporary names.
	tmpin = mktemp ("tmp$")

	# Expand the file list names.
	files (in, sort=no, > tmpin)

	# Compute the length of the input list.
	inlist = tmpin
	for (nin = 0; fscan (inlist, inname) != EOF; nin = nin + 1)
	    ;
	inlist = ""

	# Delete the temporary files.
	delete (tmpin, ver-, >& "dev$null")

	# Expand the file list names.
	files (in, sort=no, > tmpin)

	if (headers) {
	    hfile = "STDOUT"
	    if (parameters)
		pfile = "STDOUT"
	    else
		pfile = ""
	} else {
	    hfile = ""
	    pfile = ""
	}

	# Loop over each file in the input list selecting records.
	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    istable (inname)
	    if (istable.table) {

	        # Adjust pagination for TDUMP.
	        #tpagwidth = tdump.pwidth.p_max
	        tdump.pwidth.p_max = 1023

		tbdump (inname, col, texpr, cdfile=hfile, pfile=pfile,
		    datafile="STDOUT", rows="-", pagwidth=1023)

	        # Reset pagination for TDUMP.
		#tdump.pwidth.p_max = tpagwidth

	    } else if (istable.text) {
		txdump (inname, col, texpr, headers=headers,
		    parameters=parameters)
	    } else {
		print ("ERROR: Cannot run PDUMP on file: " // inname)
	    }
	}

	delete (tmpin, ver-, >& "dev$null")
	inlist = ""
end
