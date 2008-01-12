# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>
include	<gset.h>
include	<gio.h>

define	MAXCH	15

# GTXSET -- Parse a text drawing format string and set the values of the text
# attributes in the TX output structure.

procedure gtxset (tx, format, ip)

pointer	tx			# text attribute structure
char	format[ARB]		# text attribute format string
int	ip			# pointer into format string

char	attribute[MAXCH], value[MAXCH]
int	op, tip, temp, ch
int	h_v[4], v_v[4], f_v[4], q_v[4], p_v[4]
int	ctoi(), ctor(), stridx()
define	badformat_ 91

string	h_c	"nclr"
data	h_v	/GT_NORMAL,	GT_CENTER,	GT_LEFT,	GT_RIGHT/
string	v_c	"nctb"
data	v_v	/GT_NORMAL,	GT_CENTER,	GT_TOP,		GT_BOTTOM/
string	f_c	"rgib"
data	f_v	/GT_ROMAN,	GT_GREEK,	GT_ITALIC,	GT_BOLD/
string	q_c	"nlmh"
data	q_v	/GT_NORMAL,	GT_LOW,		GT_MEDIUM,	GT_HIGH/
string	p_c	"lrud"
data	p_v	/GT_LEFT,	GT_RIGHT,	GT_UP,		GT_DOWN/

begin
	# Parse the format string and set the text attributes.  The code is
	# more general than need be, i.e., the entire attribute name string
	# is extracted but only the first character is used.  Whitespace is
	# permitted and ignored.

	for (;  format[ip] != EOS;  ip=ip+1) {
	    # Extract the next "attribute=value" construct.
	    while (IS_WHITE (format[ip]))
		ip = ip +1

	    op = 1
	    for (ch=format[ip];  ch != EOS && ch != '=';  ch=format[ip]) {
		if (op <= MAXCH) {
		    attribute[op] = format[ip]
		    op = op + 1
		}
		ip = ip + 1
	    }
	    attribute[op] = EOS

	    if (ch == '=')
		ip = ip + 1

	    op = 1
	    while (IS_WHITE (format[ip]))
		ip = ip +1
	    ch = format[ip]
	    while (ch != EOS && ch != ';' && ch != ',') {
		if (op <= MAXCH) {
		    value[op] = format[ip]
		    op = op + 1
		}
		ip = ip + 1
		ch = format[ip]
	    }
	    value[op] = EOS

	    if (attribute[1] == EOS || value[1] == EOS)
		break

	    # Decode the assignment and set the corresponding text attribute
	    # in the graphics descriptor.

	    switch (attribute[1]) {
	    case 'u':				# character up vector
		tip = 1
		if (ctoi (value, tip, TX_UP(tx)) <= 0) {
		    TX_UP(tx) = 90
		    goto badformat_
		}

	    case 'p':				# path
		temp = stridx (value[1], p_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_PATH(tx) = p_v[temp]

	    case 'c':				# color
		tip = 1
		if (ctoi (value, tip, TX_COLOR(tx)) <= 0) {
		    TX_COLOR(tx) = 1
		    goto badformat_
		}

	    case 's':				# character size scale factor
		tip = 1
		if (ctor (value, tip, TX_SIZE(tx)) <= 0) {
		    TX_SIZE(tx) = 1.0
		    goto badformat_
		}

	    case 'h':				# horizontal justification
		temp = stridx (value[1], h_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_HJUSTIFY(tx) = h_v[temp]

	    case 'v':				# vertical justification
		temp = stridx (value[1], v_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_VJUSTIFY(tx) = v_v[temp]

	    case 'f':				# font
		temp = stridx (value[1], f_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_FONT(tx) = f_v[temp]
		
	    case 'q':				# font quality
		temp = stridx (value[1], q_c)
		if (temp <= 0)
		    goto badformat_
		else
		    TX_QUALITY(tx) = q_v[temp]

	    default:
badformat_	call eprintf ("Warning (GIO): bad gtext format '%s'\n")
		    call pargstr (format)
	    }

	    if (format[ip] == EOS)
		break
	}
end
