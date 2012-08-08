include "rvpackage.h"
include "rvflags.h"

# RV_ERRMSG - Print an error message to STDERR and flush the buffer

procedure rv_errmsg (errstr)

char	errstr[SZ_LINE]				#I Error message to be printed

int	ip, stridxs()

begin
	if (stridxs("%", errstr) > 0) {
	    # The errstr contains a format specifier and an argument is
	    # expected in a 'parg' call following return.  The caller will
	    # handle the stream flush.
	    call eprintf (errstr)

	} else {
	    # At this point the error message is simply a text string we want
	    # to output.  No format has been detected and no arguments are
	    # expected.

	    ip = stridxs("\n", errstr) 		# replace any misc. newlines
	    if (ip > 0) 
	        errstr[ip] = EOS

	    call eprintf ("%s\n")
	        call pargstr (errstr)
	    call flush (STDERR)
	    call tsleep (1)			# delay so it can be read
	}
end


# RV_ERR_COMMENT - Record a message for the comment section of the logs

procedure rv_err_comment (rv, errstr, arg)

pointer	rv					#I RV struct pointer
char	errstr[SZ_LINE]				#I Error message to be printed
char	arg[SZ_LINE]				#I Error argument to be printed

pointer	sp, tmp
int	ip, newlen
int	stridxs(), strlen()
errchk	realloc

define	MAX_ERRBUF	4192

begin
	if (RV_VERBOSE(rv) == OF_SHORT || RV_VERBOSE(rv) == OF_NOLOG ||
	    RV_VERBOSE(rv) == OF_TXTONLY || RV_VERBOSE(rv) == OF_STXTONLY)
	        return

	call smark (sp)
	call salloc (tmp, SZ_LINE, TY_CHAR)

	# Re-allocate the error string.
	if (RV_ERRCOMMENTS(rv) == NULL) {
	    newlen = strlen (errstr) + 6 + SZ_FNAME
	    call realloc (RV_ERRCOMMENTS(rv), newlen, TY_CHAR)
	    call strcpy ("\t\0", ERRCOMMENTS(rv), 4)
	} else {
	    newlen = strlen (ERRCOMMENTS(rv)) +  strlen (errstr) + 6 + SZ_FNAME
	    call realloc (RV_ERRCOMMENTS(rv), newlen, TY_CHAR)
	}

	if (stridxs("%", errstr) > 0) {
	    # The errstr contains a format specifier and an argument is
	    # expected in a 'parg' call.

	    call sprintf (Memc[tmp], SZ_LINE, errstr)
	        call pargstr (arg)
	    call strcat (Memc[tmp], ERRCOMMENTS(rv), MAX_ERRBUF)
	    call strcat ("\n\t", ERRCOMMENTS(rv), MAX_ERRBUF)

	} else {
	    # At this point the error message is simply a text string we want
	    # to output.  No format has been detected and no arguments are
	    # expected.

	    ip = stridxs("\n", errstr) 		# replace any misc. newlines
	    if (ip > 0) 
	        errstr[ip] = EOS

	    call sprintf (Memc[tmp], SZ_LINE, "%s\n\t")
	        call pargstr (errstr)
	    call strcat (Memc[tmp], ERRCOMMENTS(rv), MAX_ERRBUF)
	}

	call sfree (sp)
end


# RESET_ERRCOM --  Clear the error comments structure.

procedure reset_errcom (rv)

pointer	rv						#I RV struct pointer

begin
	call mfree (RV_ERRCOMMENTS(rv), TY_CHAR)
	RV_ERRCOMMENTS(rv) = NULL
end
