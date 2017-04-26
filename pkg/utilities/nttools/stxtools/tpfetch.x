include	"template.h"

# TP_FETCH -- Fetch the next image name from the group image template
#
# Create the next file name from the structure created by tp_open. 
# Return true if another group exists, false otherwise.
#
# B.Simon	28-Feb-89	Original
# B.Simon	21-Aug-91	Changed template structure
# B.Simon	24-Jul-98	Revised to handle unparsable sections

bool procedure tp_fetch (ptr, file_name)

pointer	ptr			# i: A pointer to a list of file names
char	file_name[SZ_FNAME]	# o: The next file name in the list
#--

begin
	if ((TP_INDEX(ptr) - TP_START(ptr)) + 1 > TP_COUNT(ptr))
	    return (false)

	if (TP_START(ptr) == ERR) {
	    call sprintf (file_name, SZ_FNAME, "%s%s")
	    call pargstr (TP_ROOT(ptr))
	    call pargstr (TP_SECT(ptr))

	} else if (TP_COUNT(ptr) > 1 && TP_INDEX(ptr) == TP_START(ptr)) {
	    call sprintf (file_name, SZ_FNAME, "%s[%d/%d]%s")
	    call pargstr (TP_ROOT(ptr))
	    call pargi (TP_INDEX(ptr))
	    call pargi (TP_COUNT(ptr))
	    call pargstr (TP_SECT(ptr))

	} else {
	    call sprintf (file_name, SZ_FNAME, "%s[%d]%s")
	    call pargstr (TP_ROOT(ptr))
	    call pargi (TP_INDEX(ptr))
	    call pargstr (TP_SECT(ptr))
	}

	TP_INDEX(ptr) = TP_INDEX(ptr) + 1
	return (true)
end
