# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ENVGETB -- Check whether the named option is set in the environment.
# Return true only if the option is defined in the environment and either has
# no value string (i.e., the existence of the variable is what is significant)
# or a value string which begins with the character 'y' or 'Y'.

bool procedure envgetb (varname)

char	varname[ARB]
bool	answer
pointer	sp, def
int	envfind()

begin
	call smark (sp)
	call salloc (def, SZ_LINE, TY_CHAR)

	if (envfind (varname, Memc[def], SZ_LINE) < 0)
	    answer = false
	else {
	    switch (Memc[def]) {
	    case 'y', 'Y', EOS:
		answer = true
	    default:				# abort not justified
		answer = false
	    }
	}

	call sfree (sp)
	return (answer)
end
