# TBDUMP -- Dump selected columns of an APPHOT/DAOPHOT STSDAS table database.

procedure tbdump (tables, fields)

file	tables	    {prompt = "Input apphot/daophot tables database(s)"}
string	fields      {"", prompt = "Fields to be extracted"}
file	datafile    {"STDOUT", prompt = "Output file for table data"}
file	cdfile	    {"", prompt = "Output file for table column definitions"}
file	pfile       {"", prompt = "Output file for table header parameters"}
string	rows	    {"-", prompt = "Range of rows to dump"}
int	pagwidth    {158, prompt = "Output page width"}

struct  *inlist

begin
	# Declare local variables
	file	ttables
	string	tfields, tmpin, inname

	# Get the positional parameters.
	ttables = tables
	tfields = fields

	tmpin = mktemp ("tmp$")
	files (ttables, sort=no, > tmpin)

	inlist = tmpin
	while (fscan (inlist, inname) != EOF) {
	    tdump (inname, cdfile=cdfile, pfile=pfile, datafile=datafile,
	        columns=tfields, rows=rows, pwidth=pagwidth)
	    print ("")
	}
	inlist = ""
end
