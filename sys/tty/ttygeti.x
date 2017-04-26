# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TTYGETI -- Get an integer valued capability.  If the capability is not
# found for the device, or cannot be interpreted as an integer, zero is
# returned.  Integer capabilities have the format ":xx#dd:".

int procedure ttygeti (tty, cap)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character capability name
int	ival
pointer	ip
int	tty_find_capability(), ctoi()

begin
	if (tty_find_capability (tty, cap, ip) == NO)
	    return (0)
	else if (Memc[ip] != '#')
	    return (0)
	else {
	    ip = ip + 1				# skip the '#'
	    if (ctoi (Memc, ip, ival) == 0)
		return (0)
	    else
		return (ival)
	}
end
