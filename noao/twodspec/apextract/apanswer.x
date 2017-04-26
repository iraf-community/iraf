define	ANSWERS	"|no|yes|NO|YES|"


# AP_ANSWER -- Prompt the user (if needed) and return bool based
# on 4-valued response

bool procedure ap_answer (param, prompt)

char	param[ARB]		# Parameter name
char	prompt[ARB]		# Prompt to be issued

char	word[3]
int	i, apgwrd()
pointer	pmode

begin
	i = apgwrd (param, word, 3, ANSWERS)
	switch (i) {
	case 3:
	    return (false)
	case 4:
	    return (true)
	default:
	    call malloc (pmode, SZ_LINE, TY_CHAR)
	    call sprintf (Memc[pmode], SZ_LINE, "%s.p_mode")
		call pargstr (param)
	    call appstr (Memc[pmode], "q")
	    repeat {
	        call eprintf (prompt)
	        call flush (STDERR)
		ifnoerr (i = apgwrd (param, word, 3, ANSWERS))
		    break
	    }
	    call appstr (param, word)
	    call appstr (Memc[pmode], "h")
	    call mfree (pmode, TY_CHAR)
	}

	switch (i) {
	case 1, 3:
	    return (false)
	case 2, 4:
	    return (true)
	}
end


# APGANSB -- Convert 4-valued parameter to bool

bool procedure apgansb (param)

char	param[ARB]		# Parameter name

char	word[3]
int	apgwrd()

begin
	switch (apgwrd (param, word, 3, ANSWERS)) {
	case 1, 3:
	    return (false)
	default:
	    return (true)
	}
end


# APGANS -- Convert 4-value parameter to bool except "no" is true.

bool procedure apgans (param)

char	param[ARB]		# Parameter name

char	word[3]
pointer	pmode
bool	streq()

begin
	call malloc (pmode, SZ_LINE, TY_CHAR)
	call sprintf (Memc[pmode], SZ_LINE, "%s.p_mode")
	    call pargstr (param)
	call apgstr (Memc[pmode], word, 3)
	if (word[1] != 'h')
	    call appstr (Memc[pmode], "h")
	call mfree (pmode, TY_CHAR)
	call apgstr (param, word, 3)
	return (!streq (word, "NO"))
end


# APPANS -- Put 4-valued parameter based on interactive parameter.

procedure appans (param, ival, nival)

char	param[ARB]		# Parameter
bool	ival			# Interactive value
bool	nival			# Noninteractive value

char	word[3]
pointer	pmode
bool	clgetb()

begin
	call malloc (pmode, SZ_LINE, TY_CHAR)
	call sprintf (Memc[pmode], SZ_LINE, "%s.p_mode")
	    call pargstr (param)
	call apgstr (Memc[pmode], word, 3)
	if (word[1] != 'h')
	    call appstr (Memc[pmode], "h")
	call mfree (pmode, TY_CHAR)
	if (clgetb ("interactive")) {
	    if (ival)
		call appstr (param, "yes")
	    else
		call appstr (param, "NO")
	} else {
	    if (nival)
		call appstr (param, "YES")
	    else
		call appstr (param, "NO")
	}
end
