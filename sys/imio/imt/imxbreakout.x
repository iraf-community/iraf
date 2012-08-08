# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>


# IMX_BREAKOUT -- Break out the filename template from the filtering
# expression in the list item.  Our input value is a single item in the
# template list, we'll logically separate image parameters, section strings
# and extension values from expressions that might be used in filtering.

int procedure imx_breakout (item, expand, fname, expr, sec, ikparams, maxch)

char    item[ARB]                       #i template string ptr
int     expand                          #i expanding contents?
char    fname[ARB]                      #o list filename
char    expr[ARB]                       #o filtering expression
char    sec[ARB]                        #o section string
char    ikparams[ARB]                   #o image kernel params
int     maxch                           #i max chars in fname and expr

char    ch, str[SZ_LINE], sifname[SZ_LINE]
int     nchars, ip, op
bool	is_sif

bool 	imx_issection(), imx_sifmatch()
int	stridx()

define	next_str_	99

begin
        call aclrc (fname, maxch)
        call aclrc (expr, maxch)
        call aclrc (sec, maxch)

        # At the start the ip points to the '@' in the template string.
        # Skip ahead to the start of the filename template string.
        ip = 1
        if (expand == YES)
            ip = ip + 1

        # Copy out the filename template up to the EOS, a '[' to indicate
        # the start of a filter expression, or a comma indicating the next
        # item in the list.
        ch = item[ip]
        for (op=1; ch != EOS; op=op+1) {
            fname[op] = ch

            ch = item[ip+1]
            if (ch == ',' || ch == EOS)
                return (ip-1)           # next list item, no filter expr
            else if (ch == '[')
                break                   # break to get the filter expr

            ip = ip + 1
        }


        # Get the string up to the closing ']' char.
next_str_
        ip = ip + 2
        ch = item[ip]
        call aclrc (str, SZ_LINE)
        for (op=1; ch != EOS; op=op+1) {
            str[op] = ch

            ip = ip + 1
            ch = item[ip]
            if (ch == ']')
                break                   # break to get the filter expr
        }

	if (imx_issection (str)) {
	    call strcpy (str, sec, SZ_LINE)
	} else {
	    if (expr[1] != EOS) {
	        call strcat (",", expr, SZ_LINE)
	        call strcat (str, expr, SZ_LINE)
	    } else
	        call strcpy (str, expr, SZ_LINE)
	}

	if (item[ip+1] != EOS)
	    goto next_str_

	call imx_ikparams (expr, ikparams, SZ_LINE)

	# If we've found both a section and an expression, check that the
	# section isn't being confused with an index list.
	#if (sec[1] != EOS && expr[1] != EOS) {
	#    if (!is_sif && stridx (':', sec) == 0) {
	#        call strcat (",", expr, SZ_LINE)
	#        call strcat (sec, expr, SZ_LINE)
	#    }
	#}

	if (sec[1] != EOS) {
	    call aclrc (sifname, SZ_LINE)
	    call sprintf (sifname, SZ_LINE, "%s[1][%s]")
		if (fname[1] == '@')
		    call pargstr (fname[2])
		else
		    call pargstr (fname)
		call pargstr (sec)
	} else {
	    call strcpy (fname, sifname, SZ_LINE)
	}
	is_sif = imx_sifmatch (sifname, "yes")

        nchars = ip - 1
        return (nchars)
end


# IMX_ISSECTION -- Determine if the string is an image section.
#
# Note:  There is a possible ambiguity here where using an image section
# that represents a single pixel (e.g. foo.fits[100,100]) which might also
# be a list of image extensions.

bool procedure imx_issection (str)

char	str[ARB]			# string to be checked

int	ip, stridxs()

begin
	for (ip=1; str[ip] != EOS; ip=ip+1) {
	    if (IS_ALPHA(str[ip]) || stridxs ("x()<>?", str) > 0)
		return  (FALSE)
	}

	# Test for a range list, e.g. "[1-5]"
	if (stridxs ("-,", str) > 0 && stridxs (":*", str) == 0) 
	    return (FALSE);

	# Test for a section that flips axes, e.g. "[-*,*]"
	if (stridxs ("-*:,", str) > 0) 
	    return (TRUE);

	return (FALSE)
end


# IMX_IKPARMS -- Break out the image kernel params from the template list
# expression string.

procedure imx_ikparams (expr, ikparams, maxch)

char	expr[ARB]			# expression string to modify
char	ikparams[ARB]			# extracted image kernel params
int	maxch				# max size of output strings

int	ip, op, nexpr, niki
char	ch, in[SZ_LINE], sub[SZ_LINE]

bool	imx_isikparam()

begin
	call aclrc (in, SZ_LINE)		# initialize
	call strcpy (expr, in, SZ_LINE)
	nexpr = 0
	niki  = 0

	call aclrc (expr, maxch)
	call aclrc (ikparams, maxch)
	for (ip=1; in[ip] != EOS; ip=ip+1) {
	    # Copy out the sub expression, i.e. up to the comma or EOS.
	    call aclrc (sub, SZ_LINE)
	    op = 1
	    while (in[ip] != EOS && in[ip] != ',' && in[ip] != ';') {
		sub[op] = in[ip]
		ip = ip + 1
		op = op + 1
	    }
	    ch = in[ip]

	    if (imx_isikparam (sub)) {
		if (niki > 0)
		    call strcat (",", ikparams, maxch)
		call strcat (sub, ikparams, maxch)
		niki = niki + 1

	    } else {
		if (nexpr > 0)
		    call strcat (",", expr, maxch)
		call strcat (sub, expr, maxch)
		nexpr = nexpr + 1
	    }

	    if (ch == EOS)
		break
	}
end


# IMX_ISIKPARAM -- See whether the substring refers to an image kernel param.

bool procedure imx_isikparam (str)

char	str[ARB]			# string to check

int	strncmp()

begin
        if (strncmp (str, "extname", 7) == 0 || strncmp (str, "extver", 6) == 0)
            return (TRUE)

        # Check for the "no" versions of selected keywords.
        else if (strncmp (str, "no", 2) == 0) {
            if ((strncmp (str[3], "append",    4) == 0) ||
                (strncmp (str[3], "inherit",   4) == 0) ||
                (strncmp (str[3], "overwrite", 4) == 0) ||
                (strncmp (str[3], "dupname",   4) == 0) ||
                (strncmp (str[3], "expand",    4) == 0))
                    return (TRUE)
        }

        # Other kernel keywords.
        if (strncmp (str, "inherit",   4) == 0 ||
            strncmp (str, "overwrite", 4) == 0 ||
            strncmp (str, "dupname",   4) == 0 ||
            strncmp (str, "append",    4) == 0 ||
            strncmp (str, "noappend",  4) == 0 ||
            strncmp (str, "type",      4) == 0 ||
            strncmp (str, "expand",    4) == 0 ||
            strncmp (str, "phulines",  4) == 0 ||
            strncmp (str, "ehulines",  4) == 0 ||
            strncmp (str, "padlines",  4) == 0 ||
            strncmp (str, "cachesize", 4) == 0)
                return (TRUE)
                
	return (FALSE)
end
