# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<gio.h>
include	<fset.h>
include	<chars.h>
include	<mach.h>

# GMSG -- Write a string value to a UI (user interface) parameter.  Another
# way to look at this is that we are sending a message to a UI object, hence
# this is called a message facility.
#
# NOTE -- This routine quotes the string with curly braces { } and prefaces
# the string with a "setValue", as required to set the value of a GUI client
# state variable.  This is done here rather than build knowledge into the
# lower level i/o system about the requirements for sending messages to UI
# parameters.  The low level i/o system just sends arbitrary messages to
# named UI objects; setting the value of a UI parameter object is a higher
# level abstraction layered upon the general i/o mechanism.
#
# One limitation of the UI parameter mechanism as it currently stands is that
# if the message contains curly braces, they must match up to avoid having
# the message be prematurely delimited (fortunately curly braces tend to
# match up in any valid text that uses them).  So far I haven't found a way
# around this.  The problem is that while Tcl allows braces to be backslash
# escaped to avoid being treated as delimiters, the backslashes are not
# removed, they are left in the message as data.  Hence they cannot be
# inserted in an arbitrary string without changing the string.
#
# Messages may be arbitrarily large and may extend over multiple lines.  The
# only restriction is that if the messages contain curly braces they must
# match up.

procedure gmsg (gp, object, message)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
char	message[ARB]		#I message text

int	flushnl, control_stream
int	fstati()
bool	ttygetb()

begin
	call gflush (gp)
	call flush (STDOUT)
	call flush (STDERR)

	control_stream = STDERR

	if (ttygetb (GP_TTY(gp), "EM")) {
	    flushnl = fstati (control_stream, F_FLUSHNL)
	    if (flushnl == YES)
		call fseti (control_stream, F_FLUSHNL, NO)

	    call putci (control_stream, EM)
	    call putline (control_stream, object)
	    call putci (control_stream, ' ')
	    call putline (control_stream, "setValue ")

	    call putci (control_stream, '{')
	    call putline (control_stream, message)
	    call putci (control_stream, '}')

	    call putci (control_stream, GS)
	    call putci (control_stream, US)
	    call flush (control_stream)

	    if (flushnl == YES)
		call fseti (control_stream, F_FLUSHNL, YES)
	}
end


# GMSGB -- Set the value of a boolean UI parameter.

procedure gmsgb (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
bool	value			#I value

begin
	if (value)
	    call gmsg (gp, object, "yes")
	else
	    call gmsg (gp, object, "no")
end


# GMSGC -- Set the value of a character UI parameter.

procedure gmsgc (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
char	value			#I value

char	buf[10]
int	junk, ctocc()

begin
	junk = ctocc (value, buf, 10)
	call gmsg (gp, object, buf)
end


# GMSGS -- Set the value of a short integer UI parameter.

procedure gmsgs (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
short	value			#I value

long	val
char	buf[32]
int	junk, ltoc()

begin
	if (IS_INDEFS (value))
	    call gmsg (gp, object, "INDEF")
	else {
	    val = value
	    junk = ltoc (val, buf, 32)
	    call gmsg (gp, object, buf)
	}
end


# GMSGI -- Set the value of an integer UI parameter.

procedure gmsgi (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
int	value			#I value

long	val
char	buf[32]
int	junk, ltoc()

begin
	if (IS_INDEFI (value))
	    call gmsg (gp, object, "INDEF")
	else {
	    val = value
	    junk = ltoc (val, buf, 32)
	    call gmsg (gp, object, buf)
	}
end


# GMSGL -- Set the value of a long integer UI parameter.

procedure gmsgl (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
long	value			#I value

char	buf[32]
int	junk, ltoc()

begin
	if (IS_INDEFL (value))
	    call gmsg (gp, object, "INDEF")
	else {
	    junk = ltoc (value, buf, 32)
	    call gmsg (gp, object, buf)
	}
end


# GMSGR -- Set the value of a type real UI parameter.

procedure gmsgr (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
real	value			#I value

double	dval
char	buf[MAX_DIGITS]
int	junk, dtoc()

begin
	if (IS_INDEFR (value))
	    call gmsg (gp, object, "INDEF")
	else {
	    dval = value
	    junk = dtoc (dval, buf, MAX_DIGITS, NDIGITS_RP, 'g', MAX_DIGITS)
	    call gmsg (gp, object, buf)
	}
end


# GMSGD -- Set the value of a type double UI parameter.

procedure gmsgd (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
double	value			#I value

char	buf[MAX_DIGITS]
int	junk, dtoc()

begin
	if (IS_INDEFR (value))
	    call gmsg (gp, object, "INDEF")
	else {
	    junk = dtoc (value, buf, MAX_DIGITS, NDIGITS_DP, 'g', MAX_DIGITS)
	    call gmsg (gp, object, buf)
	}
end


# GMSGX -- Set the value of a type complex UI parameter.

procedure gmsgx (gp, object, value)

pointer	gp			#I graphics descriptor
char	object[ARB]		#I object name
complex	value			#I value

char	buf[MAX_DIGITS]
int	junk, xtoc()

begin
	junk = xtoc (value, buf, MAX_DIGITS, NDIGITS_RP, 'g', MAX_DIGITS/2)
	call gmsg (gp, object, buf)
end
