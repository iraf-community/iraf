# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# RANGE -- set the scaling (range) registers

procedure range()

char	token[SZ_LINE]
int	tok, i, nchar, ip
short	color[IDS_MAXGCOLOR+1]
short	rdata[4]			# extra space for cvmove EOD
int	count, ctoi()

include "cv.com"

begin
	# In principle, we should be able to accept input for color group
	# followed by range value(s) or "vice versa" or for a series of
	# color/range pairs.  We try for most of that.
	color[1] = IDS_EOD
	rdata[1] = ERR
	count = 1
	# anything but TOK_NEWLINE
	tok = TOK_NUMBER
	repeat {
	    if (tok == TOK_NEWLINE) {
		call eprintf ("Insufficient range specification\n")
		return
	    }
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	    if (token[1] == 'c') {
		call cv_color (token[2], color)
		if (color[1] == ERR)
		    return
	    } else if (tok == TOK_NUMBER) {
		ip = 1
		nchar = ctoi (token, ip, i)
		if (i < 1) {
		    call eprintf ("bad range specification: %d\n")
			call pargi (i)
		    return
		}
		if ( count <= 3) {
		    rdata[count] = i
		    count = count + 1
		}
	    }
	} until ( (rdata[1] != ERR)  && (tok == TOK_NEWLINE ))

	rdata[count] = IDS_EOD			# mark end

	call cvrange ( color, rdata)
end
