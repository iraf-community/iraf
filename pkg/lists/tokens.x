# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>

.help tokens
.nf ___________________________________________________________________________
TOKENS -- Break the input up into a series of tokens.  The makeup of the
various tokens is defined by the FMTIO primitive ctotok, which is not very 
sophisticated, and does not claim to recognize the tokens for any particular
language (though it does reasonably well for most modern languages).  Comments
can be deleted if desired, and newlines may be passed on to the output as
tokens.

Comments are delimited by user specified strings.  Only strings which are also
recognized by ctotok() as legal tokens may be used as comment delimiters.
If newline marks the end of a comment, the end_comment string should be given
as "eol".  Examples of acceptable comment conventions are ("#", eol),
("/*", "*/"), ("{", "}"), and ("!", eol).  Fortran style comments ("^{c}",eol)
can be stripped by filtering with match beforehand.

Each token is passed to the output on a separate line.  Multiple newline
tokens are compressed to a single token (a blank line).  If newline is not
desired as an output token, it is considered whitespace and serves only to
delimit tokens.
.endhelp ______________________________________________________________________

define	SZ_COMDELIMSTR	20		# Comment delimiter string.

procedure t_tokens()

bool	ignore_comments, comment_delimiter_is_eol
bool	in_comment, pass_newlines
char	begin_comment[SZ_COMDELIMSTR], end_comment[SZ_COMDELIMSTR]
int	fd, list, token, last_token, last_nscan
pointer	sp, fname, tokbuf, outstr, ip, op

bool	streq(), clgetb()
int	clpopni(), clgfil(), fscan(), nscan(), open(), ctocc()

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (tokbuf, SZ_LINE, TY_CHAR)
	call salloc (outstr, SZ_LINE, TY_CHAR)

	# If comments are to be ignored, get comment delimiters.
	ignore_comments = clgetb ("ignore_comments")
	if (ignore_comments) {
	    call clgstr ("begin_comment", begin_comment, SZ_COMDELIMSTR)
	    call clgstr ("end_comment", end_comment, SZ_COMDELIMSTR)
	    comment_delimiter_is_eol = streq (end_comment, "eol")
	} else {
	    # Set begin_comment to null string to ensure that we never
	    # enter skip comment mode.  This requires that we check for the
	    # EOS token before the begin_comment token below.
	    begin_comment[1] = EOS
	}

	# Is newline a token?
	pass_newlines = clgetb ("newlines")


	# Merge all input files into a single stream of tokens on the standard
	# output.
	list = clpopni ("files")

	while (clgfil (list, Memc[fname], SZ_FNAME) != EOF) {
	    fd = open (Memc[fname], READ_ONLY, TEXT_FILE)
	    last_token = NULL

	    while (fscan (fd) != EOF) {
		# Break input line into a stream of tokens.
		repeat {
		    last_nscan = nscan()
		    call gargtok (token, Memc[tokbuf], SZ_LINE)

		    # If "nscan" did not increment (actually impossible with
		    # gargtok) the line has been exhausted.
		    if (nscan() == last_nscan)
			break

		    # If busy ignoring a comment, check for delimiter.
		    if (in_comment) {
			if (comment_delimiter_is_eol &&
			(token == TOK_NEWLINE || token == TOK_EOS)) {
			    in_comment = false
			    if (pass_newlines && last_token != TOK_NEWLINE) {
				call printf ("\n")
				last_token = TOK_NEWLINE
			    }
			    break
			} else if (streq (Memc[tokbuf], end_comment)) {
			    in_comment = false
			    next
			} else
			    next
		    }

		    # If we get here, we are not processing a comment.

		    if (token == TOK_NEWLINE) {
			if (pass_newlines && last_token != TOK_NEWLINE)
			    call printf ("\n")
			last_token = TOK_NEWLINE
			break

		    } else if (token == TOK_EOS) {
			# EOS is not counted as a token (do not set last_token,
			# do not generate any output).
			break

		    } else if (streq (Memc[tokbuf], begin_comment)) {
			in_comment = true
			# Do not change last_token, since comment token
			# is to be ignored.
			next

		    } else if (token == TOK_STRING) {
			# Convert control characters into printable
			# sequences before printing string token.
			op = outstr
			for (ip=tokbuf;  Memc[ip] != EOS;  ip=ip+1)
			    op = op + ctocc (Memc[ip], Memc[op], SZ_LINE)
			call printf ("\"%s\"\n")
			    call pargstr (Memc[outstr])

		    } else {				# most tokens
			call printf ("%s\n")
			    call pargstr (Memc[tokbuf])
		    }

		    last_token = token
		}
	    }
	    call close (fd)
	}

	call clpcls (list)
	call sfree (sp)
end
