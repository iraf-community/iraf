define	SZ_QUERY	2048


# T_CGIPARSE -- Parse the CGI QUERY_STRING environment variable.
# The string is expected to be a set of task.param=value pairs.
# The task parameter values are set.

procedure t_cgiparse ()

char	c, hex[2]
int	i
long	hexval
pointer	sp, str, par, val
pointer	ip, op

int	envfind(), gctol()

begin
	call smark (sp)
	call salloc (str, SZ_QUERY, TY_CHAR)
	call salloc (par, SZ_LINE, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)

	# Get the query string.  If there isn't one then do nothing.
	if (envfind ("QUERY_STRING", Memc[str], SZ_QUERY) <= 0)
	    return
	
	# Parse the query string into parameters and values.
	# For each pair set the task parameter.

	op = par
	for (ip=str;; ip=ip+1) {
	    c = Memc[ip]
	    switch (c) {
	    case '&', EOS:		# End of parameter=value
		Memc[op] = EOS
		call cgi_setpar (Memc[par], Memc[val])
		if (c == EOS)
		    break
		Memc[par] = EOS
		Memc[val] = EOS
		op = par
		next
	    case '=':			# Separator between parameters and value
		Memc[op] = EOS
		op = val
		next
	    case '+':			# Space character
		c = ' '
	    case '%':			# Special characters in hex
		call strcpy (Memc[ip+1], hex, 2)
		i = 1
		if (gctol (hex, i, hexval, 16) > 0) {
		    c = hexval
		    ip = ip + 2
		}
	    }

	    Memc[op] = c
	    op = op + 1
	}

	call sfree (sp)
end


# CGI_SETPAR -- Set parameter value.
# There is no error check for undefined tasks or parameters.
# Values of the wrong type are ignored.

procedure cgi_setpar (param, val)

char	param[ARB]		#I Task parameter
char	val[ARB]		#I Value string

bool	bval, streq()
int	ival, ip, ctoi(), ctod()
double	dval
pointer	sp, str, type

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call salloc (type, 10, TY_CHAR)

	# Determine if parameter type.
	call sprintf (Memc[str], SZ_LINE, "%s.p_type")
	    call pargstr (param)
	call clgstr (Memc[str], Memc[type], 10)

	# Set parameter in the approriate type.
	ip = 1
	switch (Memc[type]) {
	case 'i':
	    if (ctoi (val, ip, ival) > 0)
		call clputi (param, ival)
	case 'r':
	    if (ctod (val, ip, dval) > 0)
		call clputd (param, dval)
	case 'b':
	    if (streq (val, "no") || streq (val, "yes")) {
		bval = streq (val, "yes")
		call clputb (param, bval)
	    }
	default:
	    call clpstr (param, val)
	}

	call sfree (sp)
end
