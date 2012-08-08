# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "../lib/ids.h"
include <ctype.h>

# CVPARSE -- parsing routines for the cv package

# CV_FRAME -- parse a frame specification

procedure cv_frame(str, result)

char	str[ARB]		# input string
short	result[ARB]		# result string

int	ip
int	op
int	i
int	used[IDS_MAXIMPL]
int	gused

include	"cv.com"

begin
	if (str[1] == 'a') {
	    result[1] = IDS_EOD
	    return
	}
	call aclrs(used,IDS_MAXIMPL)
	gused = 0
	op = 1
	for (ip = 1; str[ip] != EOS; ip = ip + 1) {
	    if (!IS_DIGIT(str[ip])) {
		if (str[ip] == 'g')
		    gused = 1
		else {
	            call eprintf("unknown frame specifier: %c\n")
		        call pargc(str[ip])
		}
		next
	    } 
	    i = TO_INTEG (str[ip])	# fail if > than 9 planes! use ctoi()
	    if ((i < 1) || (i > cv_maxframes) ) {
		call eprintf ("out of bounds frame: %d\n")
		    call pargi(i)
		next
	    } else
		used[i] = 1
	}
	do i= 1,IDS_MAXIMPL
	    if (used[i] != 0) {
		result[op] = i
		op = op + 1
	    }
	if (gused != 0) {
	    result[op] = cv_grch
	    op = op + 1
	}
	if (op > 1)
	    result[op] = IDS_EOD
	else
	    result[op] = ERR
end


# CV_COLOR -- parse a color specification

procedure cv_color(str, result)

char	str[ARB]		# input string
short	result[ARB]		# result string

int	ip
int	op
int	i
short	val
short	used[IDS_MAXGCOLOR+1]

include	"cv.com"

begin
	if (str[1] == 'a') {
	    result[1] = IDS_EOD
	    return
	}
	call aclrs (used, IDS_MAXGCOLOR+1)
	op = 1
	for (ip = 1; str[ip] != EOS; ip = ip + 1) {
	    switch (str[ip]) {
		case 'r':
		    val = IDS_RED

		case 'g':
		    val = IDS_GREEN

		case 'b':
		    val = IDS_BLUE

		case 'y':
		    val = IDS_YELLOW

		case 'w':
		    val = IDS_WHITE

		case 'p':
		    val = IDS_RDBL

		case 'm':
		    val = IDS_GRBL

		default:
		    call eprintf("unknown color: %c\n")
			call pargc(str[ip])
		    next
	    }
	    used[val] = 1
	}
	do i = 1, IDS_MAXGCOLOR+1
	    if (used[i] != 0) {
		result[op] = i
		op = op + 1
	    }
	if (op > 1)
	    result[op] = IDS_EOD
	else
	    result[op] = ERR
end


# CV_QUAD -- parse a quad specification

procedure cv_quad(str, result)

char	str[ARB]		# input string
short	result[ARB]		# result string

int	ip
int	op
int	i
short	used[4]

include	"cv.com"

begin
	if (str[1] == 'a') {
	    result[1] = IDS_EOD
	    return
	}
	call aclrs(used, 4)
	op = 1
	for (ip = 1; str[ip] != EOS; ip = ip + 1) {
	    if (!IS_DIGIT(str[ip])) {
		switch(str[ip]) {
		    case 'a':
			call amovks (1, used, 4)

		    case 't':
			used[1] = 1
			used[2] = 1

		    case 'b':
			used[3] = 1
			used[4] = 1

		    case 'l':
			used[2] = 1
			used[3] = 1

		    case 'r':
			used[1] = 1
			used[4] = 1
		    
		    default:
	                call eprintf("unknown quad specifier: %c\n")
		            call pargc(str[ip])
		}
	    } else {
	        i = TO_INTEG (str[ip])
	        if ((i < 1) || (i > 4)) {
		    call eprintf ("out of bounds quad: %d\n")
		        call pargi(i)
		    next
	        } else
		    used[i] = 1
	    }
	}
	do i = 1,4 {
	    if (used[i] != 0) {
		result[op] = i
		op = op + 1
	    }
	}
	if (op > 1)
	    result[op] = IDS_EOD
	else
	    result[op] = ERR
end
