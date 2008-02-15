include	"inlfitdef.h"
include	<pkg/inlfit.h>

# Abort label
define	abort		9999


# ING_DEFKEY - Define graph keys

procedure ing_defkey (in, nvars, newgraph)

pointer	in			# INLFIT descriptor
int	nvars			# number of variables
int	newgraph		# update graph ?

char	ch
int	key			# graph key
int	axis			# axis number
int	type[2], num[2]		# key types and numbers
int	n, ip
pointer	line, word, vlabels, str, sp

int	scan()
int	ctoi()
int	strdic(), strlen()
int	inlstrext(), inlstrwrd()
int	in_geti()

begin
	# Allocate string space.
	call smark  (sp)
	call salloc (line,    SZ_LINE + 1, TY_CHAR)
	call salloc (word,    SZ_LINE + 1, TY_CHAR)
	call salloc (vlabels, SZ_LINE + 1, TY_CHAR)
	call salloc (str,     SZ_LINE + 1, TY_CHAR)

	# Get graph key to define.
	call printf ("Graph key to be defined: ")
	call flush (STDOUT)
	if (scan() == EOF)
	    goto abort
        call gargc (ch)

	# Convert key type into key number.
	switch (ch) {
	case '\n':
	    goto abort
	case 'h', 'i', 'j', 'k', 'l':
	    switch (ch) {
	    case 'h':
		key = 1
	    case 'i':
		key = 2
	    case 'j':
		key = 3
	    case 'k':
		key = 4
	    case 'l':
		key = 5
	    }
	default:
	    call eprintf ("Not a graph key, choose: [h, i, j, k, l]\n")
	    goto abort
	}

	# Get variable label pointer.
	call in_gstr (in, INLVLABELS, Memc[vlabels], SZ_LINE)

	# Print current settings for the axis types.
	call printf ("Set graph axis types (")
	do axis = 1, 2 {
	    call in_gkey (in, key, axis, type[axis], num[axis])
	    switch (type[axis]) {
	    case KEY_FUNCTION:
		call printf ("function")
	    case KEY_FIT:
		call printf ("fit")
	    case KEY_RESIDUALS:
		call printf ("residuals")
	    case KEY_RATIO:
		call printf ("ratio")
	    case KEY_NONLINEAR:
		call printf ("nonlinear")
	    case KEY_UAXIS:
		call sprintf (Memc[str], SZ_LINE, "user%d")
		    call pargi (num[axis])
		call printf (Memc[str])
	    case KEY_VARIABLE:
		if (inlstrwrd (num[axis], Memc[str], SZ_LINE,
		    Memc[vlabels]) != 0)
		    call printf (Memc[str])
		else {
		    call sprintf (Memc[str], SZ_LINE, "var%d")
		        call pargi (num[axis])
		    call printf (Memc[str])
		}
	    default:
		call error (0, "ing_defkey: Illegal key type")
	    }
	    if (axis == 1)
	        call printf (", ")
	}
	call printf (") : ")
	call flush (STDOUT)

	# Get line from the input stream.
	if (scan() == EOF)
	    goto abort
	call gargstr (Memc[line], SZ_LINE)

	# Get new axis types from input line.
	ip = 1
	axis = 1
	call sscan (Memc[line])
	while (axis <= 2) {

	    # Get word from line.
	    if (inlstrext (Memc[line], ip, ", ", YES, Memc[word],
	        SZ_LINE) == 0) {
		if (axis == 2)
		    call eprintf ("Incomplete definition, usage: X,Y\n")
		goto abort
	    }

	    # Search for word in the type dictionary. Keywords can
	    # be abreviated up to three characters to avoid conflicts
	    # with user variables.
	    if (strlen (Memc[word]) >= 3)
	        type[axis] = strdic (Memc[word], Memc[str], SZ_LINE, KEY_TYPES)
	    else
		type[axis] = 0

	    # Check type.
	    if (type[axis] == 0) {
		type[axis] = KEY_VARIABLE
	        num[axis]  = strdic (Memc[word], Memc[str], SZ_LINE,
		    Memc[vlabels])
	        if (num[axis] == 0) {
		    call eprintf ("Not a defined key type (%s), choose: [%s]\n")
			call pargstr (Memc[word])
			call pargstr (Memc[vlabels])
		    goto abort
	        }
	    } else if (type[axis] == KEY_VARIABLE || type[axis] ==
	        KEY_UAXIS) {
	        if (inlstrext (Memc[line], ip, ", ", YES, Memc[word],
		    SZ_LINE) == 0) {
		    call eprintf ("Incomplete definition, usage: X,Y\n")
		    goto abort
	        }
		n = 1
		if (ctoi (Memc[word], n, num[axis]) == 0) {
		    call eprintf ( "Not a valid var/user number (%s)\n")
			call pargstr (Memc[word])
		    goto abort
		}
		if (type[axis] == KEY_VARIABLE && num[axis] > nvars) {
		    call eprintf ( "Variable number does not exist (%s)\n")
			call pargstr (Memc[word])
		    goto abort
		}
	    } else
		num[axis] = INDEFI

	    # Count axis
	    axis = axis + 1
	}

	# Update axis types.
	call in_pkey (in, key, 1, type[1], num[1])
	call in_pkey (in, key, 2, type[2], num[2])

	# Test if screen needs to be refreshed.
	if (in_geti (in, INLGKEY) == key)
	    newgraph = YES
	else
	    newgraph = NO

abort
	# Free memory.
	call sfree (sp)
end
