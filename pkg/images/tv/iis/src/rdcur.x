# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include <gki.h>
include	"../lib/ids.h"

# RDCUR -- read cursor and datum

procedure rdcur()

char	token[SZ_LINE], ch
int	tok, cnum, px, py
int	junk, ip, fx, fy
real	x,y
short	datum
short	frames[IDS_MAXIMPL+2]		# frames, one graphics, EOD
int	scan(), ctoi(), mod(), and()

include "cv.com"

begin
	cnum = ERR
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (tok == TOK_NUMBER) {
	    ip = 1
	    junk = ctoi (token, ip, cnum)
	    frames[1] = cnum
	    frames[2] = IDS_EOD
	}
	else if (tok == TOK_IDENTIFIER) {
	    if (token[1] == 'o') {
		if (token[2] == 'n')
		    call cvcur(IDS_ON)
		else if (token[2] == 'f')
		    call cvcur(IDS_OFF)
		else {
		    call eprintf ("Unrecognized cursor command: %s\n")
			call pargstr (token)
		}
		return
	    }
	    call cv_frame (token[2], frames)
	    cnum = frames[1]
	    if ( cnum == IDS_EOD) {
	        call eprintf ("Please specify a particular frame\n")
	        return
	    }
	}
	if ( (cnum == ERR) || (cnum < 1) ) {
	    call eprintf ("bad cursor number: %d\n")
		call pargi (cnum)
	    return
	}

	# set kernel to do i/o on specified frames (for ggcell routine)
	call cv_iset (frames)

	call eprintf ("Press <cr> for each read; any key but <sp>, and then <cr>, to exit\n")
	repeat {
	    if (scan() != EOS)
		break
	    repeat {
		call scanc (ch)
	    } until (ch != ' ')
	    if (ch != '\n')
		break
	    call cv_rcur (cnum, x, y)
	    call ggcell (cv_gp, datum, 1, 1, x, y, x, y)
	    x = x * GKI_MAXNDC / cv_xcon + 1.
	    y = y * GKI_MAXNDC / cv_ycon + 1.
	    px = int(x)
	    py = int(y)
	    # Only allow fractions to 1/8 as that is max zoom for IIS
	    x = real (int((x - px)*8))/8.
	    y = real (int((y - py)*8))/8.
	    # Print minimum number of decimal places, but do x and y the same
	    call eprintf ("frame %d, pixel (")
	        call pargi (cnum)
	    fx = x * 8
	    fy = y * 8
	    if ((fx == 0) && (fy == 0)) {
		call eprintf ("%d,%d")
		    call pargi (px)
		    call pargi (py)
		junk = 0
	    } else {
		call eprintf ("%.*f,%.*f")

		if ( (mod(fx,4) == 0) && (mod(fy,4) == 0) )
		    junk = 1
		else if ( (and(fx,1) != 0) || (and(fy,1) != 0) )
		    junk = 3
		else
		    junk = 2

		    call pargi (junk)
		    call pargr (px+x)
		    call pargi (junk)
		    call pargr (py+y)
	    }
	    if (junk == 0)
		junk = 8
	    else
		junk = 6 - 2 * junk
	    call eprintf ("): %*w%4d\n")
		call pargi (junk)
	        call pargs (datum)
	}
end
