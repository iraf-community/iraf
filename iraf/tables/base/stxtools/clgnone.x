# CLGNONE -- Get a string parameter whose value may be "none"
#
# B.Simon	20-Jun-94	original
# B.Simon	30-Jan-95	moved to stxtools

procedure clgnone (param, value, maxch)

char	param[ARB]	# i: parameter name
char	value[ARB]	# o: parameter value
int	maxch		# i: maximum length of value
#--
pointer	sp, temp1, temp2
bool	streq()

begin
	# Allocate memory for temporary strings

	call smark (sp)
	call salloc (temp1, maxch, TY_CHAR)
	call salloc (temp2, maxch, TY_CHAR)

	# Read parameter and convert to lower case for simpler comparison

	call clgstr (param, Memc[temp1], maxch)
	call strcpy (Memc[temp1], Memc[temp2], maxch)
	call strjust (Memc[temp2])

	# If value is none, set to null string

	if (Memc[temp2] == EOS || streq (Memc[temp2], "none")) {
	    value[1] = EOS
	} else {
	    call strcpy (Memc[temp1], value, maxch)
	}

	call sfree (sp)
end
