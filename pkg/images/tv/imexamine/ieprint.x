# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	"imexam.h"
 
# IE_PRINT -- Print box of pixel values
 
procedure ie_print (ie, x, y)
 
pointer	ie			# IMEXAM structure
real	x, y			# Center of box
 
int	i, j, x1, x2, y1, y2, nx
pointer	im, data, ie_gimage(), ie_gdata()
 
begin
	iferr (im = ie_gimage (ie, NO)) {
	    call erract (EA_WARN)
	    return
	}

	x1 = x - 5 + 0.5
	x2 = x + 5 + 0.5
	y1 = y - 5 + 0.5
	y2 = y + 5 + 0.5
	iferr (data = ie_gdata (im, x1, x2, y1, y2)) {
	    call erract (EA_WARN)
	    return
	}
	nx = x2 - x1 + 1

	call printf ("%4w") 
	do i = x1, x2 {
	    call printf (" %4d ")
		call pargi (i)
	}
	call printf ("\n")
 
	do j = y2, y1, -1 {
	    call printf ("%4d")
		call pargi (j)
	    do i = x1, x2 {
		call printf (" %5g")
		    call pargr (Memr[data+(j-y1)*nx+(i-x1)])
	    }
	    call printf ("\n")
	}

	if (IE_LOGFD(ie) != NULL) {
	    call fprintf (IE_LOGFD(ie), "%4w") 
	    do i = x1, x2 {
	        call fprintf (IE_LOGFD(ie), " %4d ")
		    call pargi (i)
	    }
	    call fprintf (IE_LOGFD(ie), "\n")
 
	    do j = y2, y1, -1 {
	        call fprintf (IE_LOGFD(ie), "%4d")
		    call pargi (j)
	        do i = x1, x2 {
		    call fprintf (IE_LOGFD(ie), " %5g")
			call pargr (Memr[data+(j-y1)*nx+(i-x1)])
	        }
	        call fprintf (IE_LOGFD(ie), "\n")
	    }
	}
end
