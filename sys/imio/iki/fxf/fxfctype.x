# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<ctype.h>
include	"fxf.h"


# FXF_CTYPE -- Determine the type of a FITS card. 

int procedure fxf_ctype (card, index)

char	card[ARB]			#I FITS card (or keyword)
int	index				#O index number, if any

int	ch1, ch2
int	strncmp()

begin
	ch1 = card[1]
	ch2 = card[2]

	if (ch1 == 'B') {
	    if (ch2 == 'I') {
		if (strncmp (card, "BITPIX  ", 8) == 0)
		    return (KW_BITPIX)				# BITPIX
	    } else if (ch2 == 'S') {
		if (strncmp (card, "BSCALE  ", 8) == 0)
		    return (KW_BSCALE)				# BSCALE
	    } else if (ch2 == 'Z') {
		if (strncmp (card, "BZERO   ", 8) == 0)
		    return (KW_BZERO)				# BZERO
	    }
	} else if (ch1 == 'P') {
	    if (strncmp (card, "PCOUNT  ", 8) == 0)
	        return (KW_PCOUNT)			        # PCOUNT
	} else if (ch1 == 'I') {
	    if (ch2 == 'N') {
	        if (strncmp (card, "INHERIT ", 8) == 0)
	            return (KW_INHERIT)			        # INHERIT
	    } else if (ch2 == 'R') {
		if (strncmp (card, "IRAF-MAX", 8) == 0)
		    return (KW_IRAFMAX )			# IRAF-MAX
		if (strncmp (card, "IRAF-MIN", 8) == 0)
		    return (KW_IRAFMIN )			# IRAF-MIN
		if (strncmp (card, "IRAF-TLM", 8) == 0)
		    return (KW_IRAFTLM )			# IRAF-TLM
	    }
	} else if (ch1 == 'D') {
	    if (ch2 == 'A') {
		if (strncmp (card, "DATATYPE", 8) == 0)
		    return (KW_DATATYPE)			# DATATYPE
		if (strncmp (card, "DATAMAX ", 8) == 0)
		    return (KW_DATAMAX)				# DATAMAX
		if (strncmp (card, "DATAMIN ", 8) == 0)
		    return (KW_DATAMIN)				# DATAMIN
	    }
	} else if (ch1 == 'E') {
	    if (ch2 == 'N') {
		if (card[3] == 'D' && card[4] == ' ')
		    return (KW_END)				# END
	    } if (ch2 == 'X') {
		if (strncmp (card, "EXTEND  ", 8) == 0)
		    return (KW_EXTEND)				# EXTEND
		if (strncmp (card, "EXTNAME ", 8) == 0)
		    return (KW_EXTNAME)				# EXTNAME
		if (strncmp (card, "EXTVER  ", 8) == 0)
		    return (KW_EXTVER)				# EXTVER
	    }
	} else if (ch1 == 'N') {
	    if (ch2 == 'A') {
		if (strncmp (card, "NAXIS", 5) == 0) {
		    if (card[6] == ' ')
			return (KW_NAXIS)			# NAXIS
		    else if (IS_DIGIT(card[6])) {
			index = TO_INTEG(card[6])
			return (KW_NAXISN)			# NAXISn
		    }
	        }			
	    } else if (ch2 == 'E') {
		if (strncmp (card, "NEXTEND ", 8) == 0) 
			return (KW_NEXTEND)			# NEXTEND
	    }
	} else if (ch1 == 'S') {
	    if (ch2 == 'I')
		if (strncmp (card, "SIMPLE  ", 8) == 0)
		    return (KW_SIMPLE)				# SIMPLE
	} else if (ch1 == 'O') {
	    if (ch2 == 'R') {
		if (strncmp (card, "ORIGIN  ", 8) == 0)
		    return (KW_ORIGIN)				# ORIGIN
	    } else if (ch2 == 'B') {
		if (strncmp (card, "OBJECT  ", 8) == 0)
		    return (KW_OBJECT)				# OBJECT
	    }
	} else if (ch1 == 'X') {
	    if (strncmp (card, "XTENSION", 8) == 0)
		return (KW_XTENSION)				# XTENSION
	}

	return (ERR)
end
