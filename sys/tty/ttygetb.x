# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYGETB -- Determine whether or not a capability exists for a device.
# If there is any entry at all, the capability exists.

bool procedure ttygetb (tty, cap)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character capability name
pointer	ip
int	tty_find_capability()

begin
	return (tty_find_capability (tty, cap, ip) == YES)
end
