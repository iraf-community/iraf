#  TKAPST -- Append the token value to the macro text string.  If the 
#  text buffer is not large enough, allocate more space.

procedure tkapst (token, cline, maxch)

char	token[ARB]
pointer	cline
int	maxch

int	nch

int	strlen()

begin
	if (cline == NULL)
	    nch = 0
	else
	    nch = strlen (Memc[cline])

	if (nch + strlen (token) > maxch) {
	    maxch = maxch + SZ_LINE
	    call realloc (cline, maxch, TY_CHAR)
	}

	if (nch == 0)
	    call strcpy (token, Memc[cline], maxch)
	else
	    call strcat (token, Memc[cline], maxch)
end
