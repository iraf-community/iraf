# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	SZ_ERRMSG	SZ_LINE

# SYSERR -- Process a system error.  No arguments; print only the error
# message from the system error message file.

procedure syserr (errcode)

int	errcode

begin
	call syserrs (errcode, "")
end


# SYSERRS -- System error, with a user supplied string argument.   We do not
# want to search the system error message file until ERRACT is called to
# output the error message and initiate error recovery, because if an IFERR
# error handler is posted the message will never be used.  Hence we encode
# an error message of the form "123 user_string", where "123" is the encoded
# system error message number.  If a message is ever actually output the
# 123 will be expanded into a readable error message.

procedure syserrs (errcode, user_string)

int	errcode
char	user_string[ARB]

char	buf[SZ_ERRMSG]
int	ip, op
int	itoc()

begin
	# Encode error code, to be used to search error message file.
	op = itoc (errcode, buf, SZ_ERRMSG) + 1

	if (user_string[1] != EOS) {
	    buf[op] = ' '
	    op = op + 1
	    for (ip=1;  op <= SZ_ERRMSG && user_string[ip] != EOS;  ip=ip+1) {
		buf[op] = user_string[ip]
		op = op + 1
	    }
	}
	buf[op] = EOS

	call error (errcode, buf)
end
