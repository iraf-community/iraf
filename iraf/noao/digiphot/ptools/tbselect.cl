# TBSELECT --  Select records from an APPHOT/DAOPHOT STSDAS table based
# on the value of an expression.

procedure tbselect (intable, outtable, expr)

file intable	{"", prompt = "Input apphot/daophot tables database(s)"}
file outtable	{"", prompt = "Output apphot/daophot tables database(s)"}
string expr	{"", prompt = "Boolean expression for record selection"}

begin
	# Declare the local variables.
	string tintable, touttable, texpr

	# Get the positional parameters.
	tintable = intable
	touttable = outtable
	texpr = expr

	tselect (tintable, touttable, texpr)
end
