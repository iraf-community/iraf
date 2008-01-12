include	<ctype.h>

# CCD_SECTION -- Parse a 2D image section into its elements.
# 1. The default values must be set by the caller.
# 2. A null image section is OK.
# 3. The first nonwhitespace character must be '['.
# 4. The last interpreted character must be ']'.
#
# This procedure should be replaced with an IMIO procedure at some
# point.

procedure ccd_section (section, x1, x2, xstep, y1, y2, ystep)

char	section[ARB]		# Image section
int	x1, x2, xstep		# X image section parameters
int	y1, y2, ystep		# X image section parameters

int	i, ip, a, b, c, temp, ctoi()
define	error_	99

begin
	# Decode the section string.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1
	if (section[ip] == '[')
	    ip = ip + 1
	else if (section[ip] == EOS)
	    return
	else
	    goto error_

	do i = 1, 2 {
	    while (IS_WHITE(section[ip]))
	        ip = ip + 1

	    # Default values
	    if (i == 1) {
	        a = x1
	        b = x2
	        c = xstep
	    } else {
	        a = y1
	        b = y2
	        c = ystep
	    }

	    # Get a:b:c.  Allow notation such as "-*:c"
	    # (or even "-:c") where the step is obviously negative.

	    if (ctoi (section, ip, temp) > 0) {			# a
		a = temp
	        if (section[ip] == ':') {	
		    ip = ip + 1
		    if (ctoi (section, ip, b) == 0)		# a:b
		        goto error_
	        } else
		    b = a
	    } else if (section[ip] == '-') {			# -*
		temp = a
		a = b
		b = temp
	        ip = ip + 1
	        if (section[ip] == '*')
		    ip = ip + 1
	    } else if (section[ip] == '*')			# *
	        ip = ip + 1
	    if (section[ip] == ':') {				# ..:step
	        ip = ip + 1
	        if (ctoi (section, ip, c) == 0)
		    goto error_
	        else if (c == 0)
		    goto error_
	    }
	    if (a > b && c > 0)
	        c = -c

	    if (i == 1) {
		x1 = a
		x2 = b
		xstep = c
	    } else {
		y1 = a
		y2 = b
		ystep = c
	    }

	    while (IS_WHITE(section[ip]))
	        ip = ip + 1
	    if (section[ip] == ',')
		ip = ip + 1
	}

	if (section[ip] != ']')
	    goto error_

	return
error_
	call error (0, "Error in image section specification")
end
