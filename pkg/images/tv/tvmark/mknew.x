# MK_NEW -- Procedure to determine whether the current star is the same as
# the previous star and/or whether the current star belongs to the coordinate
# list or not.

int procedure mk_new (wx, wy, owx, owy, xlist, ylist, newlist)

real	wx		# x cursor coordinate
real	wy		# y cursor coordinate
real	owx		# old x cursor coordinate
real	owy		# old y cursor coordinate
real	xlist		# x list coordinate
real	ylist		# y list coordinate
int	newlist		# integer new list

int	newobject
real	deltaxy
bool	fp_equalr()

begin
	deltaxy = 1.0

	if (newlist == NO) {
            if (! fp_equalr (wx, owx) || ! fp_equalr (wy, owy))
		newobject = YES
	    else
		newobject = NO
	} else if ((abs (xlist - wx) <= deltaxy) &&
	        (abs (ylist - wy) <= deltaxy)) {
	    wx = xlist
	    wy = ylist
	    newobject = NO
	} else if (fp_equalr (wx, owx) && fp_equalr (wy, owy)) {
	    wx = xlist
	    wy = ylist
	    newobject = NO
	} else {
	    newlist = NO
	    newobject = YES
	}

	return (newobject)
end
