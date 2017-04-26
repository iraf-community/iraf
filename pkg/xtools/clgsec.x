# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<imhdr.h>

define	FIRST	1
define	LAST	MAX_LONG
define	STEP	1

# CLGSEC -- Get an image section and decode it.
#
# A section string may be either a null string or bracketed by [].
# The arrays x1, x2, and step are initialized to FIRST, LAST, and STEP.
# The number of subscripts decoded is returned in nsubscripts.
# This routine uses the same decode routine as IMIO.

procedure clgsec (prompt, section, x1, x2, step, nsubscripts)

char	prompt[ARB]
char	section[ARB]
long	x1[IM_MAXDIM]
long	x2[IM_MAXDIM]
long	step[IM_MAXDIM]
int	nsubscripts

int	i, ip

begin
	# Get section string.
	call clgstr (prompt, section ,SZ_LINE)

	# Set default values.
	nsubscripts = 0
	call amovkl (long (FIRST), x1, IM_MAXDIM)
	call amovkl (long (LAST), x2, IM_MAXDIM)
	call amovkl (long (STEP), step, IM_MAXDIM)

	# Skip leading whitespace.
	ip = 1
	while (IS_WHITE(section[ip]))
	    ip = ip + 1

	# Check for absent section.
	if (section[ip] == EOS)
	    return

	# Check for start of section string.
	if (section[ip] != '[')
	    call error (0, "Invalid image section")

	# Decode section.
	ip = ip + 1
	for (i=1;  i <= IM_MAXDIM && section[ip] != ']';  i=i+1)
	    call im_decode_subscript (section, ip, x1[i], x2[i], step[i])
	nsubscripts = i - 1
end
