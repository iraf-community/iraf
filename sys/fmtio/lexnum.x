# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<lexnum.h>

# LEXNUM -- Lexically analyse a character string, determine if string is
# a number, and if so, the type of number, and the number of characters
# in the number.  The ip_start argument is left pointing at the first char
# of the number (or other token), and the number of chars in the number is
# returned as the third argument (0 if not a number).
#
# NOTE - See .doc/lexnum.hlp for a description of the states of the automaton.

define	SZ_STACK	15

# Lexical actions.  "Reduce" means exit, returning code identifying lexical
# type of token.  "Shift" means switch to a new state in the automaton.
# "Revert" means reduce class "other" in the previous state.

define	ACCEPT		-6		# remain in same state
define	REVERT		-5		# revert to earlier state


# Character classes

define	SIGNCHAR	1		# +-
define	OCTDIG		2		# 0-7
define	DECDIG		3		# 8-9
define	HEXDIG		4		# a-fA-F
define	REALEXP		5		# eEdD
define	SEXAG		6		# :
define	FRACTION	7		# .
define	HEXSUFFIX	8		# xX
define	OCTSUFFIX	9		# bB
define	OTHER		10		# invalid character
define	NCC		10


# States of the automaton

define	START		1		# initial state
define	UNM		2		# unop or number
define	ODH		3		# octal, decimal, hex, or real
define	DHR		4		# decimal, hex, or real
define	QRF		5		# maybe real fraction
define	HEX		6		# hex
define	QHX		7		# maybe hex or real exponent
define	QRN		8		# maybe real number
define	OHN		9		# octal or hex number
define	RFR		10		# real fraction
define	RRX		11		# real or real exponent
define	QRX		12		# maybe real exponent
define	HRX		13		# hex or real exponent
define	RNM		14		# real number
define	REX		15		# real exponent
define	NSTATES		15


# LEXNUM -- Determine if the next sequence of characters in the string STR
# can be interpreted as a number.  Return the numeric type as the function
# value or LEX_NONNUM if the string is not a number.

int procedure lexnum (str, ip_start, nchars)

char	str[ARB]		# string to be decoded
int	ip_start		# starting index in string
int	nchars			# receives nchars in next token

char	ch
int	stk_ip[SZ_STACK]
int	ip, sp, cc, state, ip_save, toktype, act
short	stk_state[SZ_STACK], action[NCC,NSTATES]
int	strncmp()
include	"lexdata.inc"

begin
	while (IS_WHITE (str[ip_start]))
	    ip_start = ip_start + 1
	ip = ip_start

	# INDEF is a legal number and is best dealt with as a special case.
	if (str[ip] == 'I')
	    if (strncmp (str[ip], "INDEF", 5) == 0) {
		nchars = 5
		return (LEX_REAL)
	    }

	state = START				# initialization
	ip_save = ip
	sp = 0

	repeat {
	    ch = str[ip]

	    repeat {				# determine character class
		switch (ch) {
		case '+','-':
		    cc = SIGNCHAR
		    break
		case '0','1','2','3','4','5','6','7':
		    cc = OCTDIG
		    break
		case '8','9':
		    cc = DECDIG
		    break
		case 'B':
		    cc = OCTSUFFIX
		    break
		case 'D','E':
		    cc = REALEXP
		    break
		case 'A','C','F':
		    cc = HEXDIG
		    break
		case ':':
		    cc = SEXAG
		    break
		case '.':
		    cc = FRACTION
		    break
		default:
		    if (IS_LOWER (ch))
			ch = TO_UPPER (ch)	# and repeat
		    else if (ch == 'X') {
			cc = HEXSUFFIX
			break
		    } else {
			cc = OTHER
			break
		    }
		}
	    }

#call eprintf ("ip=%2d, sp=%2d, ch=%c, cc=%d, state=%d, action=%d\n")
#call pargi(ip); call pargi(sp)
#call pargc(ch); call pargi(cc); call pargi(state)
#call pargs(action[cc,state])

	    # Perform the action indicated by the action table when this
	    # class of character is encountered in the current state.

	    act = action[cc,state]
	    if (act == ACCEPT) {
		ip = ip + 1			# a simple optimization
		next
	    }

	    switch (act) {
	    case REVERT:
		repeat {
		    ip = stk_ip[sp]
		    state = stk_state[sp]
		    toktype = action[OTHER,state]
		    sp = sp - 1
		} until (toktype != REVERT || sp <= 0)

		break

	    case LEX_OCTAL, LEX_DECIMAL, LEX_HEX, LEX_REAL, LEX_NONNUM:
		toktype = action[cc,state]
		if (toktype == LEX_OCTAL && cc == OCTSUFFIX)
		    ip = ip + 1			# discard suffix char
		else if (toktype == LEX_HEX && cc == HEXSUFFIX)
		    ip = ip + 1
		break

	    default:				# shift to new state
		sp = sp + 1
		if (sp > SZ_STACK) {
		    toktype = LEX_NONNUM
		    break
		}
		stk_ip[sp] = ip
		stk_state[sp] = state

		ip = ip + 1
		state = action[cc,state]
		if (state < 1 || state > NSTATES)
		    call error (0, "In LEXNUM: cannot happen")
	    }
	}

	if (toktype == LEX_NONNUM)
	    nchars = 0
	else
	    nchars = ip - ip_save

	return (toktype)
end
