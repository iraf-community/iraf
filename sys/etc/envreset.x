# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	"environ.h"

# ENVRESET -- Update the value of the named environment variable in place.
# This is used to permanently change the value of an environment variable,
# unlike ENVPUTS which will create a temporary redefinition which can later
# be discarded via ENVFREE.  A fixed amount of string storage is allocated
# for the value string when the environment variable is first defined; if
# the new value won't fit we simply call ENVPUTS to redefine the variable
# at the top of the environment stack.  A more sophisticated storage
# mechanism could be devised which could dynamically allocate more storage,
# but the simpler scheme seems adequate at present.

procedure envreset (key, value)

char	key[ARB]		# environment variable name
char	value[ARB]		# new string value

long	sum
pointer	el, ep
int	head, ip, junk, maxch
int	envputs(), strlen()
include	"environ.com"

begin
	# Get index into envbuf of the first element of the thread.
	if (key[1] == EOS)
	    head = NULL
	else {
	    sum = 0
	    do ip = 1, MAX_HASHCHARS {
		if (key[ip] == EOS)
		    break
		sum = sum + (sum + key[ip])
	    }
	    head = threads[mod(sum,NTHREADS)+1]
	}

	# If thread is not empty search down it for the named key; if the key
	# is redefined the most recent entry is updated.

	el = NULL
	if (head != NULL)
	    for (el = envbuf + head;  el > envbuf;  el = envbuf + E_NEXT(el)) {
		ep = E_SETP(el)
		for (ip=1;  key[ip] == Memc[ep];  ip=ip+1)
		    ep = ep + 1
		if (key[ip] == EOS && Memc[ep] == '=')
		    break
	    }

	# If the named key is not found or the new value won't fit add or
	# redefine the variable, otherwise set the new value.

	if (el <= envbuf)
	    junk = envputs (key, value)
	else if (strlen(value) > E_LEN(el))
	    junk = envputs (key, value)
	else {
	    maxch = E_LEN(el)
	    call strcpy (value, Memc[ep+1], maxch)
	    call ki_envreset (key, value)
	}
end
