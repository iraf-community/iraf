include <mach.h>

# MK_FIND -- Procedure to detect the object in a file closest to the
# input cursor position.

int procedure mk_find (cl, xcur, ycur, xlist, ylist, label, id, ltid, tol)

int	cl		# coordinates file descriptor
real	xcur, ycur	# x and y cursor position
real	xlist, ylist	# x and y  list position
char	label[ARB]	# label string
int	id		# sequence number of detected object in list
int	ltid		# current sequence number in the list
real	tol		# tolerance for detection

real	x, y, dist2, ldist2, tol2
int	fscan(), nscan()

begin
	if (cl == NULL)
	    return (0)
	call seek (cl, BOF)
	ltid = 0

	# Initialize
	id = 0
	dist2 = MAX_REAL
	tol2 = tol ** 2

	# Fetch the coordinates.
	while (fscan (cl) != EOF) {
	    call gargr (x)
	    call gargr (y)
	    call gargwrd (label, SZ_LINE)
	    if (nscan () < 2)
		next
	    if (nscan () < 3)
		label[1] = EOS
	    ltid = ltid + 1
	    ldist2 = (x - xcur) ** 2 + (y - ycur) ** 2
	    if (ldist2 > tol2)
		next
	    if (ldist2 > dist2)
		next
	    xlist = x
	    ylist = y
	    dist2 = ldist2
	    id = ltid
	}

	return (id)
end
