# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<imhdr.h>
include	"imexam.h"


# IE_OPENLOG -- Open the log file.

procedure ie_openlog (ie)

pointer	ie		#I imexamine descriptor

int	nowhite(), open()
errchk	open, close

begin
	if (IE_LOGFD(ie) != NULL) {
	    call close (IE_LOGFD(ie))
	    IE_LOGFD(ie) = NULL
	}

	if (nowhite (IE_LOGFILE(ie), IE_LOGFILE(ie), SZ_FNAME) > 0) {
	    iferr {
		IE_LOGFD(ie) = open (IE_LOGFILE(ie), APPEND, TEXT_FILE)
		call printf ("Log file %s open\n")
		    call pargstr (IE_LOGFILE(ie))

		if (IE_IM(ie) != NULL) {
		    call fprintf (IE_LOGFD(ie), "# [%d] %s - %s\n")
			call pargi (IE_INDEX(ie))
			call pargstr (IE_IMNAME(ie))
			call pargstr (IM_TITLE(IE_IM(ie)))
		}

	    } then
		call erract (EA_WARN)
	}
end
