# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<fmset.h>

# FM_LFPARSE -- Parse an encoded lfile filename.
# The filename syntax is "Tddd.fff" where
#
#	T	is 'B' or 'T' for text or binary
#	ddd	is the encoded descriptor pointer
#	fff	is the encoded lfile number

int procedure fm_lfparse (lfname, fm, lfile, type)

char	lfname[ARB]		#I encoded lfile filename
pointer	fm			#O FMIO descriptor
int	lfile			#O lfile number
int	type			#O lfile file type (text or binary)

long	lval
int	ip
int	ctoi(), ctol()

begin
	# Determine file type.
	if (lfname[1] == 'T')
	    type = TEXT_FILE
	else
	    type = BINARY_FILE

	# Get FMIO descriptor.
	ip = 2
	if (ctol (lfname, ip, lval) <= 0)
	    return (ERR)
	fm = lval
	
	# Skip . delimiter.
	if (lfname[ip] == '.')
	    ip = ip + 1
	else
	    return (ERR)

	# Get lfile number.
	if (ctoi (lfname, ip, lfile) <= 0)
	    return (ERR)

	return (OK)
end
