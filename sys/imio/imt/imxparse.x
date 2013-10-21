# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include <imhdr.h>
include "imx.h"

		
define IMT_INDEX	1
define IMT_NAME		2
define IMT_VER		3
define IMT_EXPR		4
		
define DEBUG		FALSE



# IMX_PARSE -- Parse a filename to extract index ranges, extension names,
# versions and filtering expressions.

int procedure imx_parse (input, fname, index, extname, extver, 
			    expr, sec, ikparams, maxch)

char    input[ARB]                       #i template string ptr
char    fname[ARB]                      #o file name
char    index[ARB]                      #o index range string
char    extname[ARB]                    #o extension name
char    extver[ARB]                     #o extension version
char    expr[ARB]                       #o filtering expression string
char    sec[ARB]                        #o image section string
char    ikparams[ARB]                   #o image kernel section params
int	maxch				#i max chars in string params

pointer	im
int     nchars, ip, idx
char	comma, lexpr[SZ_LINE], subex[SZ_LINE], name[SZ_PATHNAME]

int	imx_breakout(), imx_next_expr(), imx_expr_type(), stridx()
pointer	immap()

begin
        call aclrc (expr, maxch)		# initialize
        call aclrc (index, maxch)
        call aclrc (fname, maxch)
        call aclrc (extver, maxch)
        call aclrc (extname, maxch)
        call aclrc (ikparams, maxch)
        call aclrc (lexpr, SZ_LINE)


	# Separate the filename from the expression string.
	nchars = imx_breakout (input, NO, fname, lexpr, sec, ikparams, maxch)

	# Parse into sub-expression strings, breaking it up into the 
	# appropriate form depending on the contents.
	if (lexpr[1] != EOS) {
	    ip = 1
	    while (imx_next_expr (lexpr, ip, subex, maxch) != EOS) {

		if (DEBUG) {
		    call eprintf ("parse subex = '%s'\t\t'%s'\n")
    			call pargstr (subex) ; call pargstr (lexpr)
		}

		switch (imx_expr_type (subex)) {
		case IMT_INDEX:
		    call strcpy (subex, index, maxch)
		case IMT_NAME:
		    call strcpy (subex, extname, maxch)
		case IMT_VER:
		    comma = ','
		    idx = stridx (comma, subex)
		    call strcpy (subex[idx+1], extver, maxch)
		    subex[idx] = '\0'
		    call strcpy (subex, extname, maxch)
		case IMT_EXPR:
		    if (expr[1] != EOS) {
		        call strcat ("||", expr, maxch)
		        call strcat (subex, expr, maxch)
		    } else
		        call strcpy (subex, expr, maxch)
		default:
		    call error (1, "unknown expression type")
		}

		ip = ip + 1
	    }
	}

	if (DEBUG) {
	    call eprintf ("final expr = '%s' index = '%s' sec = '%s'\n")
		call pargstr (expr)
		call pargstr (index)
		call pargstr (sec)
	}
	    
	call aclrc (name, SZ_PATHNAME)
	if (fname[1] == '@')
	    call strcpy (fname[2], name, SZ_PATHNAME)
	else
	    call strcpy (fname, name, SZ_PATHNAME)
	if (index[1] != EOS) {
	    call strcat ("[", name, SZ_PATHNAME)
	    call strcat (index, name, SZ_PATHNAME)
	    call strcat ("]", name, SZ_PATHNAME)
	}
	if (sec[1] != EOS) {
	    call strcat ("[", name, SZ_PATHNAME)
	    call strcat (sec, name, SZ_PATHNAME)
	    call strcat ("]", name, SZ_PATHNAME)
	}

#	iferr {
#	    im = immap (name, READ_ONLY, 0)
#	    call imunmap (im)
#	} then
#	    ;

        return (nchars)
end


# IMX_NEXT_EXPR -- Get the next sub expression from the string.  Expressions
# are delimited by semicolons, the location in the expression string is
# updated.

int procedure imx_next_expr (expr, ip, subex, maxch)

char    expr[ARB]                       #i input expression string
int	ip				#u location in expr
char    subex[ARB]                      #o sub expression string
int	maxch				#i max size of subexpr string

char	op

begin
	if (expr[ip] == EOS)
	    return (EOS)

	# Skip leading whitespace/delimiters.
	while (IS_WHITE(expr[ip]) || expr[ip] == ';')	
	    ip = ip + 1

	op = 1				# copy until EOS or next delimiter
	while (expr[ip] != EOS && expr[ip] != ';' && expr[ip] != ']') {
	    subex[op] = expr[ip]
	    ip = ip + 1
	    op = op + 1
	}
	subex[op] = EOS

	if (expr[ip] == ']')
	    ip = ip + 1

	return (ip)
end


# IMX_EXPR_TYPE -- Determine the type of expression we have.  A range list
# is assumed to be an extension index list; a single alphabetic word is 
# assumed to be an extension name, if followed by a numeric value it also
# contains an extension version;  anything else is a selection expression.

int procedure imx_expr_type (expr)

char	expr[ARB]			#i expression

int	ip, len
char	ch
int	strlen (), stridxs(), stridx()

begin
	len = strlen (expr)

	# [<expr>]
	ch = expr[1]
	if ((IS_ALNUM(expr[1]) || stridx (ch, "('\"") > 0) &&
	     stridxs ("?=:()<>&|@", expr) != 0)
	    	return (IMT_EXPR)

	# [extname,extver]
	ch = ','
	if (IS_ALPHA(expr[1]) && IS_DIGIT(expr[len]) && stridx (ch, expr) > 0)
	    return (IMT_VER)

	# [extname]
	if (IS_ALPHA(expr[1]) && stridx (ch, expr) == 0)
	    return (IMT_NAME)

	# [index] or [index_range]
	if ((IS_DIGIT(expr[1])) || 
	    ((expr[1] == '+' || expr[1] == '-') && IS_DIGIT(expr[2]))) {
	        for (ip=1; expr[ip] != EOS; ip = ip + 1) {
		    ch = expr[ip]
		    if (! IS_DIGIT(ch)) {
		        if (stridx (ch, "-x,+") == 0)
	    	            return (IMT_EXPR)
		    }
	        }
	        return (IMT_INDEX)
	}

	return (0)
end
