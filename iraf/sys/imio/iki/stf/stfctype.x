# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	"stf.h"

# STF_CTYPE -- Determine the type of a FITS card.  STF recognizes only those
# cards which define the image format and the group parameter block.

int procedure stf_ctype (card, index)

char	card[ARB]			#I FITS card (or keyword)
int	index				#O index number, if any

int	ch1, ch2, ip
int	strncmp(), ctoi()

begin
	ch1 = card[1]
	ch2 = card[2]

	# The set of keywords is fixed and small, so a simple brute force
	# recognizer is about as good as anything.

	if (ch1 == 'B') {
	    if (ch2 == 'I')
		if (strncmp (card, "BITPIX  ", 8) == 0)
		    return (KW_BITPIX)				# BITPIX
	} else if (ch1 == 'D') {
	    if (ch2 == 'A')
		if (strncmp (card, "DATATYPE", 8) == 0)
		    return (KW_DATATYPE)			# DATATYPE
	} else if (ch1 == 'E') {
	    if (ch2 == 'N')
		if (card[3] == 'D' && card[4] == ' ')
		    return (KW_END)				# END card
	} else if (ch1 == 'G') {
	    if (ch2 == 'C') {
		if (strncmp (card, "GCOUNT  ", 8) == 0)
		    return (KW_GCOUNT)				# GCOUNT
	    } else if (ch2 == 'R') {
		if (strncmp (card, "GROUPS  ", 8) == 0)
		    return (KW_GROUPS)				# GROUPS
	    }
	} else if (ch1 == 'N') {
	    if (ch2 == 'A')
		if (strncmp (card, "NAXIS", 5) == 0)
		    if (card[6] == ' ')
			return (KW_NAXIS)			# NAXIS
		    else if (IS_DIGIT(card[6])) {
			index = TO_INTEG(card[6])
			return (KW_NAXISN)			# NAXISn
		    }
	} else if (ch1 == 'P') {
	    if (ch2 == 'C') {
		if (strncmp (card, "PCOUNT  ", 8) == 0)
		    return (KW_PCOUNT)				# PCOUNT
	    } else if (ch2 == 'D') {
		if (strncmp (card, "PDTYPE", 6) == 0) {
		    ip = 7
		    if (ctoi (card, ip, index) > 0)
			return (KW_PDTYPE)			# PDTYPEn
		}
	    } else if (ch2 == 'S') {
		if (strncmp (card, "PSIZE", 5) == 0) {
		    ip = 6
		    if (card[ip] == ' ')
			return (KW_PSIZE)
		    else if (ctoi (card, ip, index) > 0)
			return (KW_PSIZEN)			# PSIZEn
		}
	    } else if (ch2 == 'T') {
		if (strncmp (card, "PTYPE", 5) == 0) {
		    ip = 6
		    if (ctoi (card, ip, index) > 0)
			return (KW_PTYPE)			# PTYPEn
		}
	    }
	} else if (ch1 == 'S') {
	    if (ch2 == 'I')
		if (strncmp (card, "SIMPLE  ", 8) == 0)
		    return (KW_SIMPLE)				# SIMPLE
	}

	return (ERR)
end
