include	"window.h"

# ENDWIN -- Finish up window routines
#
# B.Simon	28-Sep-90	Original

procedure endwin ()

#--
include "window.com"

int	win
pointer	pwin

begin
	# Release windows that are still active

	do win = 1, MAXWIN {
	    pwin = warray[win]
	    if (pwin != NULL) {
		if (WIN_BUFFER(pwin) != NULL)
		    call freescreen (WIN_BUFFER(pwin))
		if (WIN_DATA(pwin) != NULL)
		    call mfree (WIN_DATA(pwin), TY_STRUCT)
		call mfree (pwin, TY_STRUCT)
	    }
	}

	# Reset terminal

	call k_end
	call ps_end

end
