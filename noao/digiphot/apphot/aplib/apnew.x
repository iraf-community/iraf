include "../lib/apphot.h"

# APNEW -- Procedure to determine whether the current star is the same as
# the previous star and/or whether the current star belongs to the coordinate
# list or not.

int procedure apnew (ap, wx, wy, xlist, ylist, newlist)

pointer	ap		# pointer to the apphot structure
real	wx		# x cursor coordinate
real	wy		# y cursor coordinate
real	xlist		# x list coordinate
real	ylist		# y list coordinate
int	newlist		# integer new list

bool	fp_equalr()
int	newobject
real	deltaxy
real	apstatr()

begin
	deltaxy = apstatr (ap, FWHMPSF) * apstatr (ap, SCALE)

	if (newlist == NO) {
            if (! fp_equalr (wx, apstatr (ap, WX)) || ! fp_equalr (wy,
	            apstatr (ap, WY)))
		newobject = YES
	    else
		newobject = NO
	} else if ((abs (xlist - wx) <= deltaxy) &&
	        (abs (ylist - wy) <= deltaxy)) {
	    wx = xlist
	    wy = ylist
	    newobject = NO
	} else if (fp_equalr (wx, apstatr (ap, WX)) && fp_equalr (wy,
		apstatr (ap, WY))) {
	    wx = xlist
	    wy = ylist
	    newobject = NO
	} else {
	    newlist = NO
	    newobject = YES
	}

	return (newobject)
end
