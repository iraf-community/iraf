# PCONCAT -- Concatenate a list of apphot/daophot databases into a single
# output database. The input files must either be all text files or all data
# files. PCONCAT checks that the input files were all created by the same
# task.

procedure pconcat (infiles, outfile)

file	infiles {prompt = "Input apphot/daophot database(s) to be concatenated"}
file	outfile {prompt = "Output apphot/daophot database"}
string	task	{"TASK", prompt="Task name keyword"}

struct	*inlist

begin
	# Declare local variables.
	bool	table, text, other
	file	in, out
	string	tmpin, inname

	# Cache the istable parameters.
	cache ("istable")

	# Get the positional parameters.
	in = infiles
	out = outfile

	# Make a file lists.
	tmpin = mktemp ("tmp$")
	files (in, sort=no, > tmpin)

	# Check to see whether the first file is a text database or a table.

	inlist = tmpin
	if (fscan (inlist, inname) != EOF) {
	    istable (inname)
	    table = istable.table
	    text = istable.text
	    other = istable.other
	} else {
	    table = no
	    text = no
	    other = no
	}
	delete (tmpin, ver-, >& "dev$null")
	inlist = ""

	# Do the actual appending.
	if (table)
	    tbconcat (in, out, task=task)
	else if (text)
	    txconcat (in, out, task=task)
	else if (access (inname))
	    print ("File " // inname // " is not an APPHOT/DAOPHOT database")
	else
	    print ("File " // inname // " does not exist")
end
