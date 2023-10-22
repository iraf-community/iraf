
task tbbnll = t_tbbnll


procedure t_tbbnll ()

short	sbuf
bool	bbuf
char    out[SZ_FNAME]

begin
        call pbuf ("in real", INDEFR, SZ_REAL)
	call tbbeqr (INDEFR, out)
        call pbuf ("out real", out, SZ_REAL)

        call pbuf ("in double", INDEFD, SZ_REAL)
	call tbbeqd (INDEFD, out)
        call pbuf ("out double", out, SZ_REAL)

        call pbuf ("in int", INDEFI, SZ_INT32)
	call tbbeqi (INDEFI, out)
        call pbuf ("out int", out, SZ_INT32)

        sbuf = INDEFS
        call pbuf ("in short", sbuf, SZ_SHORT)
	call tbbeqs (sbuf, out)
        call pbuf ("out short", out, SZ_SHORT)

        bbuf = false
        call pbuf ("in bool", bbuf, SZ_BOOL)
	call tbbeqb (bbuf, out)
        call pbuf ("out bool", out, SZ_BOOL)
end


procedure pbuf (what, out, sz)
char    what[ARB]
char    out[ARB]
int     sz
int     i
begin
    call eprintf ("%-16s\t")
        call pargstr(what)
    do i = 1, sz {
        call eprintf ("%d ")
            call pargc (out[i])
    }
    call eprintf ("\n")
end


# tbbeqd -- assign a double-precision value
# The purpose of this is to assign the input value of type double to
# a character output buffer.

procedure tbbeqd (input, output)

double	input			# i: input double-precision value
char	output[ARB]		# o: same as input, bit for bit
#--
double	buf			# local copy of input
char	cbuf[SZ_DOUBLE]		# will be copied to output
int	i
equivalence (buf, cbuf)

begin
        call aclrc (output, SZ_DOUBLE)
	buf = input
	do i = 1, SZ_DOUBLE
	    output[i] = cbuf[i]
end

# tbbeqr -- assign a single-precision value
# The purpose of this is to assign the input value of type real to
# a character output buffer.

procedure tbbeqr (input, output)

real	input			# i: input single-precision value
char	output[ARB]		# o: same as input, bit for bit
#--
real	buf			# local copy of input
char	cbuf[SZ_REAL]		# will be copied to output
int	i
equivalence (buf, cbuf)

begin
        call aclrc (output, SZ_REAL)
	buf = input
	do i = 1, SZ_REAL
	    output[i] = cbuf[i]
end

# tbbeqi -- assign an integer value
# The purpose of this is to assign the input value of type integer to
# a character output buffer.

procedure tbbeqi (input, output)

int	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
int	buf			# local copy of input
char	cbuf[SZ_INT32]		# will be copied to output
int	i
equivalence (buf, cbuf)

begin
        call aclrc (output, SZ_INT32)
	buf = input
	do i = 1, SZ_INT32
	    output[i] = cbuf[i]
end

# tbbeqs -- assign a short integer value
# The purpose of this is to assign the input value of type short integer to
# a character output buffer.

procedure tbbeqs (input, output)

short	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
short	buf			# local copy of input
char	cbuf[SZ_SHORT]		# will be copied to output
int	i
equivalence (buf, cbuf)

begin
        call aclrc (output, SZ_SHORT)
	if (SZ_SHORT == SZ_CHAR) {
	    output[1] = input
	} else {
	    buf = input
	    do i = 1, SZ_SHORT
		output[i] = cbuf[i]
	}
end

# tbbeqb -- assign a boolean value
# The purpose of this is to assign the input value of type boolean to
# a character output buffer.

procedure tbbeqb (input, output)

bool	input			# i: input integer value
char	output[ARB]		# o: same as input, bit for bit
#--
bool	buf			# local copy of input
char	cbuf[SZ_BOOL]		# will be copied to output
int	i
equivalence (buf, cbuf)

begin
        call aclrc (output, SZ_BOOL)
	buf = input
	do i = 1, SZ_BOOL
	    output[i] = cbuf[i]
end
