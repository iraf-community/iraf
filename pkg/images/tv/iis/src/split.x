# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctotok.h>
include	<ctype.h>
include	"../lib/ids.h"

# SPLIT -- set the split screen point

procedure split()

char	token[SZ_LINE]
int	tok
int	nchar, ctoi()
int	i, x, y
real	xr, yr
int	ctor()
bool	a_real

define	errmsg	10

include "cv.com"

begin
	a_real = false
	call gargtok (tok, token, SZ_LINE)
	call strlwr (token)
	if (tok == TOK_IDENTIFIER) {
	    switch(token[1]) {
		case 'c':
		    x = cv_xcen
		    y = cv_ycen

		case 'o':
		    x = 1
		    y = 1

		case 'n', 'p':		# n: ndc, p: pixel 
		    if (token[1] == 'n')
			a_real = true
		    if (IS_DIGIT(token[2]))
			i = 2
		    else {
			call gargtok (tok, token, SZ_LINE)
			if (tok != TOK_NUMBER) {
errmsg
			    call eprintf ("bad split pixel: %s\n")
				call pargstr (token)
			    return
			} else
			    i = 1
		    }
		    if (a_real)
			nchar = ctor (token, i, xr)
		    else
		        nchar = ctoi (token, i, x)
		    if (nchar == 0) {
			call eprintf ("No conversion, ")
			goto errmsg
		    }
		    call gargtok (tok, token, SZ_LINE)
		    if (tok == TOK_PUNCTUATION)
			call gargtok (tok, token, SZ_LINE)
		    i = 1
		    if (a_real)
			nchar = ctor (token, i, yr)
		    else
		        nchar = ctoi (token, i, y)
		    if (nchar == 0) {
			call eprintf ("No conversion, ")
			goto errmsg
		    }

		default:
		    call eprintf ("unknown split code: %c\n")
			call pargc (token[1])
		    return
	    }
	}
	# Convert to NDC,  BUT note, that as x and y range from 1 through
	# cv_[xy]res, xr and yr will never be 1.0---and they must not be
	# (see cvsplit())
	if (!a_real ) {
	    xr = real(x-1) / cv_xres
	    yr = real(y-1) / cv_xres
	}
	if ( xr < 0 )
	    xr = 0
	if ( yr < 0 )
	    yr = 0
	if ( xr >= 1.0 )
	    xr = real(cv_xres-1)/cv_xres
	if ( yr >= 1.0 )
	    yr = real(cv_yres-1)/cv_yres
	call cvsplit (xr, yr)
end
