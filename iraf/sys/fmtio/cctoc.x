# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<chars.h>

define	OCTAL		8

# CCTOC -- Convert a character constant into the ASCII value of the character
# represented.  A character constant may be any whitespace delimited
# character, backslash escaped character, or a string of the form 'c', '\c',
# or '\nnn'.  The following are all legal character constants:
# 
# 	c	'c'	'\n'	'\07'	\	\\	\n
# 
# The number of characters successfully converted is returned as the function
# value.

int procedure cctoc (str, ip, cval)

char	str[ARB]		# input string
int	ip			# index into input string
char	cval			# receives character value

long	lval
bool	eat_tick
int	n, junk, ip_save
int	stridx(), gctol()
include	"escchars.inc"

begin
	while (IS_WHITE (str[ip]))
	    ip = ip + 1
	ip_save = ip

	if (str[ip] == SQUOTE) {				# '...'
	    eat_tick = true
	    ip = ip + 1
	} else
	    eat_tick = false

	if (str[ip] == ESCAPE && str[ip+1] != EOS) {		# \...
	    ip = ip + 1
	    n = stridx (str[ip], escape_chars)			# \c
	    if (n > 0) {
		cval = mapped_chars[n]
		ip = ip + 1
	    } else if (IS_DIGIT (str[ip])) {			# \nnn
		junk = gctol (str, ip, lval, -OCTAL)
		cval = lval
	    } else if (eat_tick) {				# '\c'
		cval = str[ip]
		ip = ip + 1
	    } else
		cval = ESCAPE					# \ alone

	} else if (str[ip] != EOS) {
	    cval = str[ip]					# c or 'c'
	    ip = ip + 1

	} else if (eat_tick)
	    cval = SQUOTE					# 'EOS

	if (eat_tick && str[ip] == SQUOTE)
	    ip = ip + 1

	return (ip - ip_save)
end
