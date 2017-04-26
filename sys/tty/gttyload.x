# G_TTYLOAD -- Version of the TTYLOAD procedure for accessing compiled entries
# from the GRAPHCAP database file.  Search the database of compiled GRAPHCAP
# entries for the named device, and if found, return the CAPLIST string (list
# of device capabilities) in the output string.  The number of characters output
# is returned as the function value.  The compiled database is defined by the
# include file "cacheg.dat", which serves as a cache for the GRAPHCAP
# entries of heavily used devices (see TTYLOAD, TTYCOMPILE).

int procedure g_ttyload (fname, device, outstr, maxch)

char	fname[ARB]		# name of termcap file being referenced
char	device[ARB]		# device name as in TERMCAP entry
char	outstr[maxch]		# output string to receive caplist
int	maxch

int	dev
bool	streq(), strne()
int	gstrcpy()

include	"dev$cacheg.dat"

begin
	# If the name of the file being referenced is not the same as the
	# name of the file used to build the cache, then the cache is
	# invalidated.

	if (strne (fname, sbuf[termcap_filename]))
	    return (0)

	# NDEVICES, DEVNAME, DEVCAPS, and SBUF are defined and initialized
	# in the include file.

	do dev = 1, ndevices
	    if (streq (sbuf[devname[dev]], device))
		return (gstrcpy (sbuf[devcaps[dev]], outstr, maxch))

	return (0)
end
