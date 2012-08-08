# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	<gki.h>
include	"../lib/ids.h"

# BLINK -- blink the display.

procedure blink()

char	token[SZ_LINE]
int	tok, count, rate
int	sets, button, i
int	ctoi(), ip
pointer	sp, setp, ptr
int	cv_rdbut()
int	val, nchar

define	errmsg	10

include "cv.com"

begin
	# get rate for blink

	call gargtok (tok, token, SZ_LINE)
	if (tok != TOK_NUMBER) {
	    call eprintf ("Bad blink rate: %s\n")
		call pargstr (token)
	    return
	}
	ip = 1
	count = ctoi(token, ip, rate)
	if (rate < 0) {
	    call eprintf ("negative rate not legal\n")
	    return
	}

	call smark (sp)
	# The "3" is to hold frame/color/quad for one frame;
	# the "2" is to allow duplication of each frame so that
	# some frames can stay "on" longer.  The extra "1" is for graphics.
	call salloc (setp, 2 * 3 * (cv_maxframes+1), TY_POINTER)
	sets = 0

	# which frames to blink

	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	while ( (sets <= cv_maxframes+1) && (tok != TOK_NEWLINE) ) {
	    sets = sets + 1
	    ptr = setp + (3 * (sets-1))
	    call salloc (Memi[ptr], IDS_MAXIMPL+1, TY_SHORT)
	    if (tok == TOK_IDENTIFIER) {
	        if (token[1] == 'f') {
	            call cv_frame (token[2], Mems[Memi[ptr]])
	            if (Mems[Memi[ptr]] == ERR) {
			call sfree (sp)
	    	        return
		    }
	        }
	    } else if (tok == TOK_NUMBER) {
		ip = 1
		nchar = ctoi (token[1], ip, val)
		if ( (val < 0) || (val > cv_maxframes)) {
		    call eprintf ("illegal frame value: %s\n")
			call pargstr (token)
		    call sfree (sp)
		    return
		}
		Mems[Memi[ptr]] = val
		Mems[Memi[ptr]+1] = IDS_EOD
	    } else {
errmsg
		call eprintf ("Unexpected input: %s\n")
		    call pargstr (token)
		call sfree (sp)
		return
	    }
	    ptr = ptr + 1
	    call salloc (Memi[ptr], IDS_MAXGCOLOR+1, TY_SHORT)
	    call salloc (Memi[ptr+1], 5, TY_SHORT)
	    Mems[Memi[ptr]] = IDS_EOD		# default all colors
	    Mems[Memi[ptr+1]] = IDS_EOD		# default all quads
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	    if ( (tok != TOK_IDENTIFIER) && (tok != TOK_NEWLINE))
		goto errmsg
	    if ((tok == TOK_IDENTIFIER) && (token[1] == 'c')) {
		call cv_color (token[2], Mems[Memi[ptr]])
		if (Mems[Memi[ptr]] == ERR) {
		    call sfree (sp)
		    return
		}
		call gargtok (tok, token, SZ_LINE)
		call strlwr (token)
	    }
	    if ( (tok != TOK_IDENTIFIER) && (tok != TOK_NEWLINE))
		goto errmsg
	    if ((tok == TOK_IDENTIFIER) && (token[1] == 'q')) {
		call cv_quad (token[2], Mems[Memi[ptr+1]])
		if (Mems[Memi[ptr+1]] == ERR) {
		    call sfree (sp)
		    return
		}
		call gargtok (tok, token, SZ_LINE)
		call strlwr (token)
	    }
	}	# end while

	button = cv_rdbut()		# clear any buttons pressed
	call eprintf ("Press any button to terminate blink\n")
	repeat {
	    do i = 1, sets {
		ptr = setp + 3 * (i-1)
		call cvdisplay (IDS_ON, IDS_DISPLAY_I, Mems[Memi[ptr]],
		     Mems[Memi[ptr+1]], Mems[Memi[ptr+2]])
		# Delay for "rate*100" milliseconds
		call zwmsec (rate * 100)

		# Leave something on screen when button pushed
	        button = cv_rdbut()
		if (button > 0)
		    break
		call cvdisplay (IDS_OFF, IDS_DISPLAY_I, Mems[Memi[ptr]],
		     Mems[Memi[ptr+1]], Mems[Memi[ptr+2]])
	    }
	} until (button > 0)

	call sfree (sp)
end
