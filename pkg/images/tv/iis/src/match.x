# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	"../lib/ids.h"

# MATCH -- Match look up tables.  The command reads
#	match this_one (to) that one

procedure match

char	token[SZ_LINE]
int	tok
short	f_ref[2]
short	c_ref[IDS_MAXGCOLOR+1]
short	frames[IDS_MAXIMPL+1]
short	colors[IDS_MAXGCOLOR+1]
short	nextcolor
int	nchar, i, val, ctoi()
int	ltype

include "cv.com"

begin
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if ( (tok == TOK_IDENTIFIER) && (token[1] == 'o') ) {
	    ltype = IDS_OUTPUT_LUT
	} else {
	    ltype = IDS_FRAME_LUT
	    # "Push back" the token
	    call reset_scan
	    call gargtok (tok, token, SZ_LINE)
	}

	# All this parsing tells us why YACC and LEX were invented
	# Use "i" to tell if have parsed something useful

	i = -1
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if ((tok == TOK_IDENTIFIER) && (token[1] == 'f')) {
	    i = 1
	    call cv_frame (token[2], frames)
	    if (frames[1] == ERR)
	        return
	} else if (tok == TOK_NUMBER) {
	    i = 1
	    nchar = ctoi (token, i, val)
	    if ((val < 1) || (val > cv_maxframes)) {
	        call eprintf ("Invalid frame specification: %d\n")
		    call pargi (val)
	        return
	    } else {
		frames[1] = val
		frames[2] = IDS_EOD
	    }
	} else if (ltype == IDS_FRAME_LUT) {
	    call eprintf ("missing frame arguement\n")
	    return
	} else
	    frames[1] = IDS_EOD

	# default first color argument to all colors for both FRAME and OUTPUT
	# tables...means make all colors the same.

	colors[1] = IDS_EOD		# default all colors

	# Advance if previous token was useful

	if ( i != -1 ) {
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	}

	# Look for a color

	if ((tok == TOK_IDENTIFIER) && (token[1] == 'c')) {
	    call cv_color (token[2], colors)
	    if (colors[1] == ERR)
		return
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	}

	# look for fill word "to"

	if ((tok == TOK_IDENTIFIER) && (token[1] == 't')) {
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	}

	# if FRAME LUT, we default frame to first frame to be changed.
	# if OUTPUT LUT, frame is irrelevant

	i = -1
	if (tok == TOK_IDENTIFIER) {
	    if (token[1] == 'f')
		i = 2
	    else if (token[1] != 'c') {
		call eprintf ("Unexpected argument: %s\n")
		    call pargstr (token)
		return
	    }
	} else if (tok == TOK_NUMBER)
	    i = 1

	# if ltype is OUTPUT lut, don't care about frame type, but can't
	# omit it...so default to EOD

	f_ref[1] = IDS_EOD
	f_ref[2] = IDS_EOD
	if (ltype == IDS_FRAME_LUT) {
	    if (i == -1) {
		f_ref[1] = frames[1]
	    } else {
	        nchar = ctoi (token, i, val)
		if ((val < 1) || (val > cv_maxframes)) {
	            call eprintf ("Invalid frame specification: %d\n")
		        call pargi (val)
	            return
	        }
		f_ref[1] = val
	    }
	}

	# Only thing left should be the reference color.
	# If found a frame before, advance the token.

	if (i != -1) {
	    call gargtok (tok, token, SZ_LINE)
	    call strlwr (token)
	}
	if ((tok != TOK_NEWLINE) && (tok != TOK_IDENTIFIER)) {
	    call eprintf ("Unexpected input: %s\n")
		call pargstr (token)
	    return
	}
	c_ref[1] = IDS_EOD
	if (tok == TOK_IDENTIFIER) {
	    if (token[1] != 'c') {
	        call eprintf ("Unexpected input (color required): %s\n")
		    call pargstr (token)
	        return
	    } else {
	        call cv_color (token[2], c_ref)
	        if (c_ref[1] == ERR)
	            return
	    }
	}

	if (c_ref[1] != IDS_EOD)
	    call cvmatch (ltype, f_ref, c_ref, frames, colors)
	else {
	    # No specific color for reference.  If no color specified
	    # to copy into, do all.
	    c_ref[2] = IDS_EOD
	    if ( colors[1] == IDS_EOD ) {
		colors[1] = IDS_RED
		colors[2] = IDS_GREEN
		colors[3] = IDS_BLUE
		colors[4] = IDS_EOD
	    }
	    # Match for each color given in "colors"
	    for ( i = 1 ; colors[i] != IDS_EOD; i = i + 1) {
		nextcolor = colors[i+1]
		colors[i+1] = IDS_EOD
		c_ref[1] = colors[i]
		call cvmatch (ltype, f_ref, c_ref, frames, colors[i])
		colors[i+1] = nextcolor
	    }
	}
end
