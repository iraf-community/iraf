# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"environ.h"

# ENV_FIRST -- Return a pointer to the first (most recently entered) entry
# in the environment list.  A pointer to the string definition of the entry
# is returned as the output argument.

pointer procedure env_first (valp)

pointer	valp			# pointer to environment string
pointer	el
include	"environ.com"

begin
	el = envbuf + last
	if (el > envbuf) {
	    valp = E_SETP(el)
	    return (el)
	} else
	    return (NULL)
end


# ENV_NEXT -- Return a pointer to the next element in the environment list.
# A pointer to the string value of the element is returned as the output
# argument.

pointer procedure env_next (last_el, valp, show_redefines)

pointer	last_el			# pointer to last element returned
pointer	valp			# receives charp of next element define string
int	show_redefines		# do not skip redefined elements

pointer	el
include	"environ.com"

begin
	el = envbuf + E_LASTELEM(last_el)

	while (el > envbuf) {
	    if (E_REDEF(el) == NO || show_redefines == YES)
		break
	    else
		el = envbuf + E_LASTELEM(el)
	}

	if (el > envbuf) {
	    valp = E_SETP(el)
	    return (el)
	} else
	    return (NULL)
end
