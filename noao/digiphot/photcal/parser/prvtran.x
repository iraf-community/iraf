# PR_VTRAN -- Translate the internal parser symbol for an error or weight
# column definition into a compact user readable definition. This code is
# intended primarily for interfacing to the inlfit or other package with
# the goal of making more pleasing variable names.

procedure pr_vtran (invname, outvname, maxch)

char	invname[ARB]	# the input variable name
char	outvname[ARB]	# the output variable name
int	maxch		# maximum number of characters

int	first, last
int	gstrmatch()

begin
	if (gstrmatch (invname, "@E_", first, last) != 0) {
	    call sprintf (outvname, maxch, "er(%s)")
		call pargstr (invname[last+1])
	} else if (gstrmatch (invname, "@W_", first, last) != 0) {
	    call sprintf (outvname, maxch, "wt(%s)")
		call pargstr (invname[last+1])
	} else {
	    call strcpy (invname, outvname, maxch)
	}
end
