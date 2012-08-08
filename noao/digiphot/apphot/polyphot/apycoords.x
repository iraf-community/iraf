include	"../lib/polyphot.h"

# AP_YCOORDS -- Procedure to fetch the next center coordinates for the polygon.

int procedure ap_ycoords (cl, delim, xshift, yshift, stdin)

int	cl	# coordinates file descriptor
int	delim	# delimiter character
real	xshift	# new x coordinate
real	yshift	# new y coordinate
int	stdin	# is cl STDIN?

char	marker
int	stat
int	fscan(), nscan()

begin
	if (stdin == YES) {
	    call printf ("Type object x and y coordinates (^D or^Z to end): ")
		call flush (STDOUT)
	}

	stat = fscan (cl)
	while (stat != EOF) {

            call gargr (xshift)
            call gargr (yshift)

	    if (nscan() != 2) {
		call reset_scan()
		call gargc (marker)
		if (int (marker) == delim)
		    return (NEXT_POLYGON)
		#else
		    #return (NEXT_OBJECT)
	    } else
	        return (THIS_OBJECT)

	    stat = fscan (cl)
	}

	return (stat)
end
