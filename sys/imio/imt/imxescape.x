# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include "imx.h"


# IMX_ESCAPE -- Return a pointer to the composed file name, escaping parts
# as needed.

pointer procedure imx_escape (in, index, extname, extver, ikparams, 
		section, expr, maxch)

char	in[ARB]			#I File image name (without kernel or image sec)
char	index[ARB]		#I Range list of extension indexes 
char	extname[ARB]		#I Pattern for extension names
char	extver[ARB]		#I Range list of extension versions
char	ikparams[ARB]		#I Image kernel parameters
char	section[ARB]		#I Image section
char	expr[ARB]		#I Selection expression
int	maxch			#I Print errors?

pointer	out, op
int	i, len, level
char	ch, peek, prev
bool	init_esc

int	strlen()

define output  {Memc[op]=$1;op=op+1}
define escape  {output('\\');output($1)}

begin
	len = max (SZ_LINE, strlen (in))
	call calloc (out, max (SZ_LINE, (4*len)), TY_CHAR)

	op = out
	level = 0

	init_esc = false
	for (i=1; i <= len; i=i+1) {
	    prev = in[max(1,i)]
	    ch   = in[i]
	    peek = in[i+1]

	    if (ch == EOS) 
		break;
	    if (ch == '[') {
		if (prev != ']' && !init_esc) {
		    output ('%')
		    output ('%')
		    output (CH_DELIM)
		    init_esc = true
		}
		escape (ch)
		level = level + 1
	    } else if (ch == ']') {
		output (ch)
		if (peek != '[')		# closing delim
		    output ('%')
		level = level - 1
	    } else if (ch == ',') {
                if (level > 0)
		    output('\\')
                if (level == 0)
		    init_esc = false
		output (ch)
	    } else if (ch == '*')
		escape (ch)
	    else
		output (ch)
	}
	output (EOS)

	return (out)
end
