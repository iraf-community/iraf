# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# XER_FMTERRMSG -- Expand error message encoded as "123 user_string" into
# a full error message by looking the error message string up in lib$syserrmsg.
# If the first character of ERRMSG is nonnumeric no processing is done.
# The conversion may be performed in place, i.e., errmsg and outstr may be
# the same array.

procedure xer_fmterrmsg (errmsg, outstr, maxch)

char	errmsg[ARB]			# encoded error message.
char	outstr[maxch]			# output string
int	maxch

char	buf[SZ_LINE], user_string[SZ_FNAME]
int	codelen, nchars, chan, ip, op, junk
int	strncmp(), envfind(), gstrcpy()
define	nofile_ 91

begin
	# Determine ndigits in error code.
	for (ip=1;  IS_DIGIT (errmsg[ip]);  ip=ip+1)
	    ;
	codelen = ip - 1

	# Output message as is if no error code.  Copy into local buf first
	# in case errmsg and outstr overlap.

	if (codelen == 0) {
	    call strcpy (errmsg, buf, SZ_LINE)
	    call strcpy (buf, outstr, maxch)
	    return
	}

	# Extract the user string into a local buffer.
	while (IS_WHITE (errmsg[ip]))
	    ip = ip + 1
	for (op=1;  errmsg[ip] != EOS && errmsg[ip] != '\n';  ip=ip+1) {
	    user_string[op] = errmsg[ip]
	    op = op + 1
	}
	user_string[op] = EOS

	# Generate the OS pathname of the "lib$syserrmsg" file.
	if (envfind ("iraf", buf, SZ_LINE) > 0) {
	    call zfsubd (buf, SZ_LINE, "lib", nchars)
	    call strcat ("syserrmsg", buf, SZ_LINE)
	    call strpak (buf, buf, SZ_LINE)
	} else
	    goto nofile_

	# Open and search the system error message file.
	call zopntx (buf, READ_ONLY, chan)
	if (chan == ERR)
	    goto nofile_

	repeat {
	    call zgettx (chan, buf, SZ_LINE, nchars)
	    if (nchars == 0 || nchars == ERR) {
		call zclstx (chan, junk)
		goto nofile_
	    } else if (strncmp (buf, errmsg, codelen) == 0) {
		call zclstx (chan, junk)
		break
	    }
	}

	# Skip the error code prefix and the blank which follows.
	for (ip=codelen+1;  IS_WHITE(buf[ip]);  ip=ip+1)
	    ;

	# Output system error message.
	for (op=1;  buf[ip] != '\n' && buf[ip] != EOS;  ip=ip+1) {
	    outstr[op] = buf[ip]
	    op = op + 1
	}
	
	# Add user operand, if supplied, enclosed in parens.
	if (user_string[1] != EOS) {
	    outstr[op] = ' '
	    outstr[op+1] = '('
	    op = op + 2
	    op = op + gstrcpy (user_string, outstr[op], maxch-op+1)
	    outstr[op] = ')'
	    op = op + 1
	}

	outstr[op] = EOS
	return

nofile_
	call strcpy (errmsg, buf, SZ_LINE)
	call strcpy (buf, outstr, maxch)
end
