# TBSORT --  Sort an APPHOT/DAOPHOT STSDAS table using the values in the
# selected columns.

procedure tbsort (table, fields)

file table    {"", prompt="Input apphot/daophot tables database(s)to be sorted"}
string fields {"", prompt = "Fields to be sorted on"}
bool ascend   {yes, prompt = "Sort in ascending value order ?"}
bool casesens {yes, prompt = "Case sensitive sort ?"}

begin
	# Decalre the local variables.
	string ttable, tfields

	# Get the positional parameters.
	ttable = table
	tfields = fields

	# Sort the table.
	tsort (ttable, tfields, ascend=ascend, casesens=casesens)
end
