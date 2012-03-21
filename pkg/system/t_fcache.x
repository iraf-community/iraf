# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>

define	FC_CMDS	"|init|purge|destroy|list|lookup|access|add|delete|wait|help|"

define	FC_INIT		1
define	FC_PURGE	2
define	FC_DESTROY	3
define	FC_LIST		4
define	FC_LOOKUP	5
define	FC_ACCESS	6
define	FC_ADD		7
define	FC_DELETE	8
define	FC_WAIT		9
define	FC_HELP		10


# FCACHE -- Task interface to the file cache.

procedure t_fcache ()

char	cmd[SZ_FNAME], cache[SZ_FNAME], fname[SZ_FNAME]
char	pattern[SZ_FNAME], src[SZ_FNAME], cname[SZ_FNAME], extn[SZ_FNAME]
bool	verbose, in_src, exists
int	age

int	strdic(), clgeti(), envgeti()
bool	clgetb(), fcaccess()

begin
	# Get the common parameters.
	call clgstr ("cmd", cmd, SZ_FNAME)
	call clgstr ("cache", cache, SZ_FNAME)
	verbose = clgetb ("verbose")

	# Process the requested command.
	switch (strdic (cmd, cmd, SZ_FNAME, FC_CMDS)) {
	case FC_INIT:
	    call clgstr ("pattern", pattern, SZ_FNAME)
	    call fcinit (cache, pattern)

	case FC_PURGE:
	    age = clgeti ("age")
	    if (age < 0)
		age = envgeti ("cache_age")
	    call fcpurge (cache, verbose, age)

	case FC_DESTROY:
	    call fcdestroy (cache, verbose)

	case FC_LIST:
	    call fclist (cache, verbose, STDOUT)

	case FC_LOOKUP:
	    call clgstr ("src", src, SZ_FNAME)
	    if (src[1] != EOS) {
	        call fclookup (cache, src, fname, extn, SZ_FNAME)
	        if (verbose) {
		    call printf ("%s\n")
		        call pargstr (fname)
	        }
	        call clpstr ("fname", fname)
	        call clpstr ("extn", extn)
	    } else {
	        call clgstr ("fname", fname, SZ_FNAME)
	        call clgstr ("extn", extn, SZ_FNAME)
	        call fclookup (cache, src, fname, extn, SZ_FNAME)
		if (verbose) {
		    call printf ("%s\n")
		        call pargstr (src)
		}
	        call clpstr ("src", src)
	    }

	case FC_ACCESS:
	    call clgstr ("src", src, SZ_FNAME)
	    call clgstr ("extn", extn, SZ_FNAME)

	    exists =  fcaccess (cache, src, extn)
	    call printf ("%b\n")
	        call pargb (exists)

	case FC_ADD:
	    call clgstr ("src", src, SZ_FNAME)
	    call clgstr ("extn", extn, SZ_FNAME)
	    call fcadd (cache, src, extn, cname, SZ_FNAME)
	    if (verbose) {
		call eprintf ("%s\n")
		    call pargstr (cname)
	    }
	    call clpstr ("fname", cname)
	    if (clgetb ("wait"))
	        call fcwait (cache, cname)

	case FC_DELETE:
	    call clgstr ("src", src, SZ_FNAME)
	    if (src[1] != EOS) {
		# Delete by src string.
	        call fclookup (cache, src, fname, extn, SZ_FNAME)
	        call fcdelete (cache, fname)
	    } else {
		# Delete by cached filename.
	        call clgstr ("fname", fname, SZ_FNAME)
	        call fcdelete (cache, fname)
	    }

	case FC_WAIT:
	    call clgstr ("src", src, SZ_FNAME)
	    call fcwait (cache, src)

	case FC_HELP:

	default:
	    call eprintf ("Unknown command '%s'\n")
		call pargstr (cmd)
	}
end
