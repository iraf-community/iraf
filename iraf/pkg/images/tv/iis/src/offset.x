# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# OFFSET -- Change the bias (offset) for certain colors

procedure offset()

int	tok, i, nchar, ip
char	token[SZ_LINE]
short	color[IDS_MAXGCOLOR+1]
short	offsetdata[4]			# extra space for cvmove EOD
int	count, ctoi()

include "cv.com"

begin
	# In principle, we should be able to accept input for color group
	# followed by offset value(s) or "vice versa" or for a series of
	# color/offset pairs.  We try for most of that.
	color[1] = ERR
	offsetdata[1] = ERR
	count = 1
	# anything but TOK_NEWLINE
	tok = TOK_NUMBER
	repeat {
	    if (tok == TOK_NEWLINE) {
		call eprintf ("Insufficient offset specification\n")
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
		if ( count <= 3) {
		    offsetdata[count] = i
		    count = count + 1
		}
	    }
	} until ( (color[1] != ERR) && (offsetdata[1] != ERR) &&
		  (tok == TOK_NEWLINE) )

	offsetdata[count] = IDS_EOD		# mark end

	call cvoffset (color, offsetdata)
end
