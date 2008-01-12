# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GTYGETI -- Get an integer valued capability.  If the capability is not
# found for the device, or cannot be interpreted as an integer, zero is
# returned.  Integer capabilities have the format ":xx#dd:".

int procedure gtygeti (tty, cap)

pointer	tty			# tty descriptor
char	cap[ARB]		# two character capability name
int	ival
pointer	ip
int	gty_find_capability(), ctoi()

begin
	if (gty_find_capability (tty, cap, ip) == NO)
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
