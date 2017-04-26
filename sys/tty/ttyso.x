# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYSO -- Turn "standout" mode on or off.

procedure ttyso (fd, tty, onflag)

int	fd, onflag
pointer	tty

char	cap[2]
bool	ttygetb()
int	junk, ttyctrl()
errchk	ttygetb

begin
	# Select name of capability (so, se, us, ue).  Use so/se if it is
	# available for the terminal, otherwise try us/ue.
	if (ttygetb (tty, "so")) {
	    cap[1] = 's'
	    cap[2] = 'o'
	} else {
	    cap[1] = 'u'
	    cap[2] = 's'
	}
	if (onflag == NO)
	    cap[2] = 'e'
	cap[3] = EOS

	# Output the control sequence.  If cap is not available for the
	# terminal, nothing will be output.
	junk = ttyctrl (fd, tty, cap, 1)
end
