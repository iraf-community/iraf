# TBDUMP -- Dump selected columns of an APPHOT/DAOPHOT STSDAS table database.

procedure tbdump (tables, fields, expr)

file	tables	    {prompt = "Input apphot/daophot tables database(s)"}
string	fields      {"", prompt = "Fields to be extracted"}
string	expr	    {"yes", prompt = "Boolean expression for record selection"}
file	datafile    {"STDOUT", prompt = "Output file for table data"}
file	cdfile	    {"", prompt = "Output file for table column definitions"}
file	pfile       {"", prompt = "Output file for table header parameters"}
string	rows	    {"-", prompt = "Range of rows to dump"}
int	pagwidth    {158, prompt = "Output page width"}

struct  *inlist

begin
	# Declare local variables
	file	ttables
	string	tfields, texpr, tmpin, tmpout, inname

	# Get the positional parameters.
	ttables = tables
	tfields = fields
	texpr = expr

	tmpin = mktemp ("tmp$")
	files (ttables, sort=no, > tmpin)

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    if (texpr != "yes") {
	        tmpout = mktemp ("tmp$")
	        tselect (inname, tmpout, texpr)
	        tdump (tmpout, cdfile=cdfile, pfile=pfile, datafile=datafile,
	            columns=tfields, rows=rows, pwidth=pagwidth)
	        tdelete (tmpout, ver-, >& "dev$null")
	    } else {
	        tdump (inname, cdfile=cdfile, pfile=pfile, datafile=datafile,
	            columns=tfields, rows=rows, pwidth=pagwidth)
	    }
	}
	delete (tmpin, ver-, >& "dev$null")
	inlist = ""
end
