# CLSET.H -- CLIO parameters.  Here "CL" refers to the parent process, which
# is not necessarily the IRAF Command Language.

define	CL_PRTYPE	1		# parent process type (see below)
define	CL_PCACHE	2		# symtab descriptor of param cache

# Process type codes.

define	PR_CONNECTED	1		# connected subprocess
define	PR_DETACHED	2		# detached subprocess
define	PR_HOST		3		# subprocess spawned by host

# Process interpreter mode codes (used by ONENTRY and the Iraf Main).

define	PR_NOEXIT	0		# run interpreter in Iraf Main
define	PR_EXIT		1		# skip interpreter, shutdown process
