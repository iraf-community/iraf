# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"imwcs.h"

# IW_FINDCARD -- Search the card list in the IMWCS descriptor for a card of
# the given type, with the given axis and index numbers.  Return a pointer to
# the card if found, else NULL.

pointer procedure iw_findcard (iw, type, axis, index)

pointer	iw			#I pointer to IMWCS descriptor
int	type			#I card type code
int	axis			#I axis number, or <0 to ignore
int	index			#I index number, or <=0 to ignore

int	i
pointer	cp

begin
	do i = 1, IW_NCARDS(iw) {
	    cp = IW_CARD(iw,i)
	    if (C_TYPE(cp) != type)
		next
	    if (axis >= 0)
		if (C_AXIS(cp) != axis)
		    next
	    if (index > 0)
		if (C_INDEX(cp) != index)
		    next
	    return (cp)
	}

	return (NULL)
end
