# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>
include	"environ.h"

.help environ
.nf ___________________________________________________________________________
ENVIRON -- Routines for managing the environment list.  The environment list
is global in scope.  A process, e.g., the CL, builds up the environment list
and passes it on to a child process when the process is spawned.

       nchars = envgets (name, value, maxch)	# get value of envvar
	redef =	envputs (name, value)		# set value of envvar
       nchars = envfind (name, value, maxch)	# get value of envvar if def
		envmark (sp)			# mark stack pointer
      nredefs = envfree (sp, userfcn)		# free back to marked posn

	 bool = envgetb (name)			# get boolean value of envvar
	  int = envgeti (name)			# get integer value of envvar
		envlist (fd, prefix, show_redefs)	# print envlist on file
	nscan =	envscan (input_source)		# read SET stmts from a file

	 el = env_first (valp)			# head of envlist
	  el = env_next (el, valp, show_redefs) # next element of envlist

The environment list is maintained as a multi-threaded linked list.  This
provides the searching efficiency of a hash table plus stack like semantics
for redefinitions and for freeing blocks of variables.  There are two primary
data structures internally, an array of pointers to the heads of the threads,
and a buffer containing the list elements.  These data structures are
dynamically allocated and will be automatically reallocated at runtime if
overflow occurs.  The number of threads determines the hashing efficiency and
is a compile time parameter.

The ENVMARK and ENVFREE procedures mark and free storage on the environment
list stack.  All environment variables defined or redefined after a call to
ENVMARK will be deleted and storage freed by a call to ENVFREE.  If a redef
is freed the next most recent definition becomes current.  ENVFREE returns
as its function value the number of redefined variables uncovered by the free
operation.  The calling program must mark and free in the correct order or the
environment list may be trashed.

The ENVLIST procedure prints the environment list on a file.  Redefined values
will only be printed if desired.  The environment list is printed as a list of
SET statements in most recent first order, i.e.,

	set nameN=valueN
	set nameM=valueM
	    ...
	set name1=value1

The ENVLIST function is used both to inspect the environment list and to pass
the list on to a child process.  Redefined variables are omitted when passing
the list on to a child process, hence the order of definition does not matter.
The output format is "prefix name=value", where the prefix string is supplied
by the user.

The ENVSCAN function parses one or more SET statements, calling ENVPUTS to
enter the SET declarations into the environment list.  The argument is either
a SET declaration or a string of the form "set @filename", where "filename" is
the name of a file containing set declarations.
.endhelp ______________________________________________________________________


# ENVFIND -- Search the environment list for the named environment variable
# and return the string value if found.

int procedure envfind (key, value, maxch)

char	key[ARB]		# environment variable name
char	value[maxch]		# string value (output)
int	maxch

long	sum
pointer	el, ep
int	head, ip, nchars
int	envputs(), gstrcpy()
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

	# If thread is not empty search down it for the named key and return
	# the value string if found.  Note that the value of the E_NEXT pointer
	# is given as an integer offset into envbuf to facilitate reallocation
	# upon overflow.

	if (head != NULL)
	    for (el = envbuf + head;  el > envbuf;  el = envbuf + E_NEXT(el)) {
		ep = E_SETP(el)
		for (ip=1;  key[ip] == Memc[ep];  ip=ip+1)
		    ep = ep + 1
		if (key[ip] == EOS && Memc[ep] == '=')
		    return (gstrcpy (Memc[ep+1], value, maxch))
	    }

	# Key not found.  Ask the host system for the value of the environment
	# variable.

	call strpak (key, value, maxch)
	call zgtenv (value, value, maxch, nchars)

	if (nchars >= 0)  {
	    call strupk (value, value, maxch)
	    ip = envputs (key, value)
	    return (nchars)
	} else {
	    value[1] = EOS
	    return (ERR)
	}
end


# ENVPUTS -- Add a new SET definition to the environment list.  A SET operation
# is allowed to redefine a previously defined environment variable, but if the
# new definition is a redef we return YES as the function value.  If the set
# is a no-op (null key, or redef with the same value as previously) the envlist
# is not modified and NO is returned as the function value.

int procedure envputs (key, value)

char	key[ARB]		# environment variable name
char	value[ARB]		# string value

long	sum
int	head, thread_index, redef, ip
pointer	el, op, ep

bool	streq()
pointer	coerce()
int	gstrcpy(), krealloc()
include	"environ.com"

begin
	if (key[1] == EOS)
	    return (NO)

	# Get index into envbuf of the first element of the thread.
	sum = 0
	do ip = 1, MAX_HASHCHARS {
	    if (key[ip] == EOS)
		break
	    sum = sum + (sum + key[ip])
	}

	thread_index = mod (sum, NTHREADS) + 1
	head = threads[thread_index]

	# If thread is not empty search down it for the named key to see if we
	# have a redefinition.  If we have a redef but the new value is the
	# same as the old, do nothing.  Otherwise flag the element being
	# redefined as a redefinition (so that ENVLIST can ignore it).

	redef = NO
	if (head != NULL)
	    for (el = envbuf + head;  el > envbuf;  el = envbuf + E_NEXT(el)) {
		ep = E_SETP(el)
		for (ip=1;  key[ip] == Memc[ep];  ip=ip+1)
		    ep = ep + 1
		if (key[ip] == EOS && Memc[ep] == '=')
		    if (streq (Memc[ep+1], value))
			return (NO)
		    else {
			E_REDEF(el) = YES
			redef = YES
			break
		    }
	    }

	# Append the new list element to the end of ENVBUF, increasing the size
	# of the buffer if overflow occurs.  The list structure must be aligned
	# on a short integer boundary.  Set the back link pointers for searches.

	if (top + MAX_LENLISTELEM >= len_envbuf) {
	    len_envbuf = len_envbuf + INC_ENVBUF
	    if (krealloc (envbuf, len_envbuf, TY_SHORT) == ERR)
		call sys_panic (SYS_MFULL, "Out of memory")
	}

	el = envbuf + top
	E_NEXT(el)     = head
	E_LASTELEM(el) = last
	E_REDEF(el)    = NO

	# Deposit the string "key=value" in the E_SET field.  At least
	# MIN_SZVALUE chars are allocated for the value string, to permit
	# the value to be updated via ENVRESET (possibly changing size in
	# the process).

	op = E_SETP(el)
	op = op + gstrcpy (key, Memc[op], MAX_SZKEY)
	Memc[op] = '='
	op = op + 1
	E_LEN(el) = max (MIN_SZVALUE, gstrcpy(value,Memc[op],MAX_SZVALUE))
	op = op + E_LEN(el) + 1

	last = top
	threads[thread_index] = last
	top = coerce (op, TY_CHAR, TY_SHORT) - envbuf

	# Update the environment in any connected kernel servers.
	call ki_envreset (key, value)

	return (redef)
end


# ENVMARK -- Mark the position in the environment list.  A subsequent call
# to ENVFREE with the marked position as argument will unset all elements
# set after the marked position.

procedure envmark (old_top)

int	old_top			# top of envbuf stack
include	"environ.com"

begin
	old_top = top
end


# ENVFREE -- Free all environment list entries set since the matching call
# to ENVMARK.  Return as the function value the number of redefined environment
# variables uncovered by the free operation.  If the ZLOCPR integer entry point
# address of the user supplied function USERFCN is nonnull the function will
# be called with the name and value of each uncovered redefinition.  The calling
# sequence is as follows:  subroutine userfcn (name, value)

int procedure envfree (old_top, userfcn)

int	old_top			# top of envbuf stack
int	userfcn			# epa of function called for uncovered redefs

int	nredefs, head, i, j, t
pointer	sp, start, namep, el1, el2, ep1, ep2
include	"environ.com"

begin
	if (old_top < 1 || old_top >= top)
	    return (0)

	call smark (sp)
	call salloc (namep, SZ_FNAME, TY_CHAR)

	nredefs = 0

	# Clear the redef flags for all list elements that are redefined by
	# elements above the new top, and count the number of uncovered redefs.
	# Examine only non-empty threads.

	for (t=1;  t <= NTHREADS;  t=t+1) {
	    head = threads[t]
	    if (head != NULL) {
		# Examine only list elements in the thread which lie above the
		# top we are reverting to.

		for (j = head;  j >= old_top;  j = E_NEXT(el1)) {
		    el1 = envbuf + j

		    # Scan down the thread to see if this is a redefinition,
		    # and clear the redef flag if so.

		    for (i = j;  i != NULL;  i = E_NEXT(el2)) {
			el2 = envbuf + i
			if (E_REDEF(el2) == YES) {
			    ep1 = E_SETP(el1)
			    ep2 = E_SETP(el2)
			    start = ep2
			    while (Memc[ep1] == Memc[ep2] && Memc[ep1] != '=') {
				ep1 = ep1 + 1
				ep2 = ep2 + 1
			    }
			    if (Memc[ep1] == '=') {
				E_REDEF(el2) = NO
				nredefs = nredefs + 1
				if (userfcn != NULL) {
				    call strcpy (Memc[start], Memc[namep],
					ep2 - start)
				    call zcall2 (userfcn,
					Memc[namep], Memc[ep2+1])
				}
				break
			    }
			}
		    }
		}

		# Set the head of the thread to the first element below
		# the new top.

		threads[t] = j
	    }
	}

	# The variable OLD_TOP is the index of a list element.  Make it the
	# new top.

	top = old_top
	last = E_LASTELEM(envbuf+top)

	call sfree (sp)
	return (nredefs)
end
