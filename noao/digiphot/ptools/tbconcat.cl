# TBCONCAT -- Concatenate a list of apphot/daophot STSDAS table databases
# into a single output database. All the input files must of been written
# by the same task.

procedure tbconcat (tables, outtable)

file  tables {prompt="Input apphot/daophot tables databases to be concatenated"}
file	outtable  {prompt="Output apphot/daophot STSDAS table database"}
string	task      {"TASK", prompt="Task name keyword"}

struct	*inlist

begin
	# Declare local variables.
	bool	stat, first_tab
	file	in, out
	int	npars, first_npars, ncols, first_ncols
	string	tmpin, inname, first_inname, tkname, first_tkname

	# Cache the parameters.
	cache ("istable", "keypar", "tinfo")

	# Get the positional parameters.
	in = tables
	out = outtable

	# Make a file lists.
	tmpin = mktemp ("tmp$")
	files (in, sort=no, > tmpin)

	# Loop through the list checking that all the files are 
	# tables that they were created with the same task and
	# that they have the same number of parameters and columns

	stat = yes
	first_tab = yes

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {

	    # Check that the input file is an STSDAS table.
	    istable (inname)
	    if (! istable.table) {
		print ("ERROR: File " // inname // " is not an ST table")
		stat = no
		break
	    }


	    # Check that all the input files were written by the same task.
	    if (defpar ("keypar.silent")) {
	        keypar (inname, task, silent=no)
	    } else {
	        keypar (inname, task)
	    }
	    if (first_tab) {
		first_inname = inname
		first_tkname = keypar.value
	    } else {
		tkname = keypar.value
		if (tkname != first_tkname) {
		    print ("ERROR:")
		    print ("    File" // first_inname // " written by task " //
                        first_tkname)
		    print ("    File" // inname // " written by task " //
		        tkname)
		    stat = no
		    break
		}
	    }

	    # Check that the number of parameters and columns is the same.
	    tinfo (inname, ttout=no)
	    if (first_tab) {
		first_npars = tinfo.npar
		first_ncols = tinfo.ncols
	    } else {
		npars = tinfo.npar
		ncols = tinfo.ncols
		if (npars != first_npars) {
		    print ("ERROR:")
		    print ("    File " // first_inname // " has " //
		        first_npars // "parameters")
		    print ("    File " // inname // " has " // npars //
		        "parameters")
		    stat = no
		    break
		}
		if (ncols != first_ncols) {
		    print ("ERROR:")
		    print ("    File " // first_inname // " has " //
		        first_ncols // "columns")
		    print ("    File " // inname // " has " // ncols //
		        "columns")
		    stat = no
		    break
		}
	    }

	    first_tab = no
	}

	delete (tmpin, ver-, >& "dev$null")
	inlist = ""

	# Return if status is not ok.
	if (! stat)
	    return

	# Do the actual append.
	tmerge (in, out, option="append", allcols=no, tbltype="row",
		allrows=100, extracol=0)
end
