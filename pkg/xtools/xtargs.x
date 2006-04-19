include	<ctotok.h>


# XTARGS -- Parse strings consisting of a list keyword=value pairs.
# 
# This is a limited interface to parse strings containing a list of
# whitespace separate keyword=value pairs and then provide get procedures
# for the value as a string or double.  Other datatypes must be coerced or
# parsed from the string or the double values.
# 
# The keyword=value pairs may contain whitespace between around the equal
# sign but the value must be quoted if it contains blanks.  A keyword must
# be an "identifier" begining with a letter and consist of letters, numbers,
# underscore, dollar, and period.
# 
# The get procedure posts an error if the requested key is not present.
# Note that the full power of the symbol table package may be used.
# The values of the symbols is a single integer having the offset into
# the string buffer of the symbol table which provides the value as as
# string.
#
# 	stp = xtargs_open (argstr)		# parse string and return symtab
# 	xtargs_s (stp, key, val, maxchar)	# Get value as a string
# 	dval = xtargs_d (stp, key)		# Get value as a double
#
# Note that there is no close method and instead stclose should be used.



# XTARGS_OPEN -- Parse an argument string and return a symbol table.
#
# Note that this interface does not include a close because the user
# inherits the symbol table interface.

pointer procedure xtargs_open (argstr)

char	argstr[ARB]			#I Argument string

int	i, tok
pointer	sp, key, val, stp, sym

bool	strne()
int	nscan(), stpstr()
pointer	stopen(), stfind(), stenter()

begin
	call smark (sp)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (val, SZ_LINE, TY_CHAR)

	# Open symbol table.
	stp = stopen ("xtargs", 10, 100, 1000)

	# Scan the argument string.
	call sscan (argstr)
	repeat {
	    # Get keyword.
	    call gargtok (tok, Memc[key], SZ_FNAME)
	    if (tok == TOK_EOS)
	        break

	    # Get required delimiter.
	    call gargtok (i, Memc[val], SZ_LINE)
	    if (i != TOK_OPERATOR || strne (Memc[val], "="))
	        break

	    # Get value.
	    call gargwrd (Memc[val], SZ_LINE)

	    # Check for error.
	    if (tok != TOK_IDENTIFIER || mod (nscan(), 3) != 0)
	        break

	    # Ignore case.
	    call strlwr (Memc[key])

	    # Enter in symbol table.
	    sym = stfind (stp, Memc[key])
	    if (sym == NULL) {
	        sym = stenter (stp, Memc[key], 1)
		Memi[sym] = stpstr (stp, Memc[val], 1)
	    }
	}

	call sfree (sp)

	# Check for error.
	if (mod (nscan(), 3) != 1) {
	   call stclose (stp)
	   call error (1, "Syntax error")
	}
	
	return (stp)
end


# XTARGS_S -- Get string valued parameter.
# An error is triggered if the key is not in the symbol table.

procedure xtargs_s (stp, key, val, maxchar)

pointer	stp				#I Symbol table
char	key[ARB]			#I Key to find
char	val[maxchar]			#O String value
int	maxchar				#I Maximum number of characters

pointer	sym, stfind(), strefsbuf()

begin
	sym = stfind (stp, key)
	if (sym == NULL)
	    call error (1, "Key not found")

	call strcpy (Memc[strefsbuf(stp,Memi[sym])], val, maxchar)
end


# XTARGS_D -- Get double valued parameter.
# An error is triggered if the key is not in the symbol table.

double procedure xtargs_d (stp, key)

pointer	stp				#I Symbol table
char	key[ARB]			#I Key to find
double	dval				#R Integer value

int	i, j, ctod(), strlen()
pointer	sym, stfind(), strefsbuf()

begin
	sym = stfind (stp, key)
	if (sym == NULL)
	    call error (1, "Key not found")

	i = 1
	j = ctod (Memc[strefsbuf(stp,Memi[sym])], i, dval)
	if (j != strlen(Memc[strefsbuf(stp,Memi[sym])]))
	    call error (2, "Value not a number")

	return (dval)
end
