# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# TEXT -- put text into image planes or graphics bit planes

procedure text()

char	token[SZ_LINE]
int	tok, ip, cnum
short	frames[IDS_MAXIMPL+2]			# frames, graphics, EOD
short	colors[IDS_MAXGCOLOR]
real	x, y
int	button, cv_wtbut()
char	line[SZ_LINE]
real	size, clgetr()

begin
	frames[1] = ERR
	colors[1] = ERR

	# which frames for text

	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (tok == TOK_IDENTIFIER) {
	    if (token[1] == 'f') {
	        call cv_frame (token[2], frames)
	        if (frames[1] == ERR)
		    return
	    } else if (token[1] == 'c') {
		call cv_color (token[2], colors)
		if (colors[1] == ERR)
		    return
	    }
	} else if (tok == TOK_NUMBER) {
	    call cv_frame (token[1], frames)
		if (frames[1] == ERR)
		    return
	}
	if ( (frames[1] == ERR) && (colors[1] == ERR)) {
	    call eprintf ("Inadequate text specification: %s\n")
	        call pargstr (token)
	    return
	}

	call gargstr (line, SZ_LINE)

	# Prompt user to set cursor

	call eprintf ("Set cursor to desired location, then press any button\n")
	button = cv_wtbut()

	# Set up kernel for write
	if (frames[1] != ERR) {
	    cnum = frames[1]
	    call cv_iset (frames)
	} else {
	    cnum = 16		# SORRY, is IIS specific - we should do better
	    call cv_gset (colors)
	}
	call cv_rcur (cnum, x, y)

	size = clgetr("textsize")
	ip = 1
	while (IS_WHITE(line[ip]))
	    ip = ip + 1
	call cvtext (x, y, line[ip], size)
end
