# ADDSLASH -- Convert a string by prefixing quote marks with backslashes
#
# B.Simon	30-Sep-87	First Code

procedure addslash (str, maxch)

char	str[ARB]		# String to be converted
int	maxch			# Maximum length of string

int	i, j
pointer	sp, aux

begin
	call smark (sp)
	call salloc (aux, maxch, TY_CHAR)

	j = 1
	for (i = 1; (str[i] != EOS) && (j <= maxch); i = i + 1) {
	    if (str[i] == '"') {
		if (j == maxch)
		    break
	     	Memc[aux+j-1] = '\\'
		j = j + 1
	    }
	    Memc[aux+j-1] = str[i]
	    j = j + 1
	}

	Memc[aux+j-1] = EOS
	call strcpy (Memc[aux], str, maxch)
	call sfree (sp)
end
