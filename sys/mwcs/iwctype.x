# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"imwcs.h"

# IW_CARDTYPE -- Examine a FITS card to see if it is a WCS specification card,
# and if so, return the card type, axis number, and index number.  ERR is
# return if the card is not a WCS card.

int procedure iw_cardtype (card, type, axis, index)

char	card[ARB]		#I card to be examined
int	type			#O card type
int	axis			#O axis number or ERR
int	index			#O index number or ERR

int	ch1, ch2, ip
int	strncmp(), ctoi()

begin
	ch1 = card[1]
	ch2 = card[2]
	type = ERR
	ip = 6

	# This is hardcoded for the sake of efficiency.
	if (ch1 == 'C') {
	    if (ch2 == 'D') {
		if (IS_DIGIT (card[3])) {
		    # CDi_j
		    type = TY_CD
		    axis = TO_INTEG (card[5])
		    index = TO_INTEG (card[3])
		    if (card[6] != ' ')
			type = ERR
		} else if (strncmp (card, "CDELT", 5) == 0) {
		    # CDELTi
		    type = TY_CDELT
		    axis = TO_INTEG (card[6])
		    index = ERR
		    if (card[7] != ' ')
			type = ERR
		}
	    } else if (ch2 == 'R') {
		if (strncmp (card, "CROTA2", 6) == 0) {
		    # CROTA2
		    type = TY_CROTA
		    axis = ERR
		    index = ERR
		} else if (strncmp (card, "CRPIX", 5) == 0) {
		    # CRPIXi
		    type = TY_CRPIX
		    axis = TO_INTEG (card[6])
		    index = ERR
		    if (card[7] != ' ')
			type = ERR
		} else if (strncmp (card, "CRVAL", 5) == 0) {
		    # CRVALi
		    type = TY_CRVAL
		    axis = TO_INTEG (card[6])
		    index = ERR
		    if (card[7] != ' ')
			type = ERR
		}
	    } else if (ch2 == 'T') {
		if (strncmp (card, "CTYPE", 5) == 0) {
		    # CTYPEi
		    type = TY_CTYPE
		    axis = TO_INTEG (card[6])
		    index = ERR
		    if (card[7] != ' ')
			type = ERR
		}
	    }
	} else if (ch1 == 'L' && ch2 == 'T') {
	    if (card[3] == 'V' && IS_DIGIT (card[4])) {
		type = TY_LTV
		axis = TO_INTEG (card[4])
		index = ERR
	    } else if (card[3] == 'M' && IS_DIGIT (card[4])) {
		type = TY_LTM
		axis = TO_INTEG (card[4])
		index = TO_INTEG (card[6])
	    }
	} else if (ch1 == 'W') {
	    if (ch2 == 'A') {
		if (card[3] == 'T' && IS_DIGIT (card[4])) {
		    type = TY_WATDATA
		    axis = TO_INTEG (card[4])
		    if (IS_DIGIT(card[5]))
			ip = 5
		    if (ctoi (card, ip, index) <= 0)
			type = ERR
		} else if (strncmp (card, "WAXMAP", 6) == 0) {
		    type = TY_WAXMAP
		    axis = ERR
		    ip = 7
		    if (ctoi (card, ip, index) <= 0)
			type = ERR
		}
	    } else if (ch2 == 'C') {
		if (strncmp (card, "WCSDIM", 6) == 0) {
		    type = TY_WCSDIM
		    axis = ERR
		    index = ERR
		}
	    } else if (ch2 == 'S') {
		if (card[3] == 'V' && IS_DIGIT (card[4])) {
		    if (strncmp (card[5], "_LEN", 4) == 0) {
			type = TY_WSVLEN
			axis = TO_INTEG (card[4])
			index = ERR
		    } else {
			if (IS_DIGIT(card[5]))
			    ip = 5
			if (ctoi (card, ip, index) > 0) {
			    type = TY_WSVDATA
			    axis = TO_INTEG (card[4])
			}
		    }
		}
	    }
	}

	return (type)
end
