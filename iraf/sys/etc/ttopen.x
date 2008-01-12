# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fset.h>

# TTOPEN -- Open a terminal for direct i/o.  The logical device "dev$tty"
# denotes the user terminal.  Note that this string is passed on to the
# kernel without modification, despite the apparent use of a logical directory.
# (See also fio$zfiott.x, the logical terminal driver).

int procedure ttopen (terminal, mode)

char	terminal[ARB]		# device to be opened
int	mode

int	fopntx()
extern	zopntt(), zgettt(), zputtt(), zflstt(), zstttt(), zclstt(),
	zsektt(), znottt()

begin
	return (fopntx (terminal, mode, zopntt, zgettt, zputtt, zflstt,
	    zstttt, zclstt, zsektt, znottt))
end


# TTSETI -- Set special terminal driver options.  The regular FIO options
# are set using FSETI.

procedure ttseti (fd, param, value)

int	fd			# file descriptor
int	param			# parameter to be set
int	value			# new value

int	channel
int	fstati()

begin
	channel = fstati (fd, F_CHANNEL)
	call zsettt (channel, param, value)
end


# TTSTATI -- Stat special terminal driver options.

int procedure ttstati (fd, param)

int	fd			# file descriptor
int	param			# parameter to be set

long	lvalue
int	channel
int	fstati()

begin
	channel = fstati (fd, F_CHANNEL)
	call zstttt (channel, param, lvalue)
	return (lvalue)
end


# TTSETS -- Set special terminal driver option, type string.  The regular FIO
# options are set using FSETI.

procedure ttsets (fd, param, svalue)

int	fd			# file descriptor
int	param			# parameter to be set
char	svalue[ARB]		# new string value

int	channel
int	fstati()

begin
	channel = fstati (fd, F_CHANNEL)
	call zsestt (channel, param, svalue)
end


# TTSTATS -- Stat special terminal driver options, type string.

int procedure ttstats (fd, param, outstr, maxch)

int	fd			# file descriptor
int	param			# parameter to be set
char	outstr[maxch]		# receives parameter value
int	maxch

int	nchars
int	channel
int	fstati()

begin
	channel = fstati (fd, F_CHANNEL)
	call zststt (channel, param, outstr, maxch, nchars)
	return (nchars)
end
