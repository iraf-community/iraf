# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# QP_PARSE -- Parse a QPOE/QPIO specification into the root (poefile) name
# and event list filter expression fields.
#
#	Syntax:		root[filter]
#
# where the filter spec is optional.

procedure qp_parse (qpspec, root, sz_root, filter, sz_filter)

char	qpspec[ARB]		#I full event list specification
char	root[sz_root]		#O receives root name
int	sz_root			#I max chars in root name
char	filter[sz_filter]	#O receives filter
int	sz_filter		#I max chars in filter name

int	level, ip, op, ch

begin
	ip = 1
	op = 1

	# Extract root name.  The first (unescaped) [ marks the start of
	# the filter field.

	for (ch=qpspec[ip];  ch != EOS && ch != '[';  ch=qpspec[ip]) {
	    if (ch == '\\' && qpspec[ip+1] == '[') {
		root[op] = '\\'
		op = op + 1
		root[op] = '['  
		ip = ip + 1
	    } else
		root[op] = ch

	    op = min (sz_root, op + 1)
	    ip = ip + 1
	}

	root[op] = EOS
	level = 0
	op = 1

	# Extract the [] bracketed filter expression, allowing for nested
	# brackets.

	for (ch=qpspec[ip];  ch != EOS;  ch=qpspec[ip]) {
	    if (ch == '[')
		level = level + 1
	    else if (ch == ']')
		level = level - 1
		    
	    filter[op] = ch
	    op = min (sz_filter, op + 1)

	    ip = ip + 1
	    if (level <= 0)
		break
	}

	# Add closing brace if the user left it off.
	if (op > 1 && ch != ']') {
	    filter[op] = ']'
	    op = min (sz_filter, op + 1)
	}

	filter[op] = EOS
end
