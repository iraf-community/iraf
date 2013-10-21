# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <error.h>
include <syserr.h>
include <ctype.h>
include "imx.h"

define	DEBUG		FALSE


# IMXOPEN -- Open an image template using the enhanced expansion
# capabilities.  This procedure is simply the entry point to the imtopen()
# method in the standard IMT interface.

pointer	procedure imxopen (template)

char	template[ARB]		# image template

int	i, sort, level, ip, ch, expand, nchars, nimages, index, type
int	max_fnt, fnt_len, len, flen
pointer	listp, intmp, fnt, op, exp
char    lfile[SZ_LINE], lexpr[SZ_LINE], likparams[SZ_LINE], lsec[SZ_LINE]
char    lindex[SZ_LINE], lextname[SZ_LINE], lextver[SZ_LINE], elem[SZ_LINE]

pointer	imx_preproc (), imx_imexpand (), imx_fexpand ()
pointer	imx_texpand (), imx_dexpand ()
int	imx_filetype (), imx_parse (), imx_get_element ()
int	fntopnb (), strlen (), strsearch()
int	sum, fntlenb()
bool	envgetb()

define	output 	{Memc[op]=$1;op=op+1}
define	escape 	{output('\\');output($1)}

begin
	# Pre-process the input template.
	intmp = imx_preproc (template)
	    
	if (DEBUG) {
	    call eprintf ("template: '%s'\npreproc: '%s'\n\n")
		call pargstr (template)
		call pargstr (Memc[intmp])
	}


	fnt_len = 0 						# initialize    
	max_fnt = SZ_FNT
	call calloc (fnt, max_fnt, TY_CHAR)

	# Sorting is disabled as input and output templates, derived from the
	# same database but with string editing used to modify the output list,
	# may be sorted differently as sorting is performed upon the edited
	# output list.

	sort = NO

	op = fnt
	ip = intmp

	for (ip=intmp;  Memc[ip] != EOS;  ip=ip+1) {
	    ch = Memc[ip]

	    if (ch == '[') {
		if (ip > 1 && Memc[ip-1] == '!') {
		    # ![ -- Pass a [ to FNT (character class notation).
		    Memc[op-1] = '['

		} else if (ip > 1 && Memc[ip-1] == '\\') {
		    # \[ -- The [ is part of the filename.  Pass it on as an
		    # escape sequence to get by the FNT.

		    output ('[')

		} else {
		    # [ -- Unescaped [.  This marks the beginning of an image
		    # section sequence.  Output `%%[...]%' and escape all
		    # pattern matching metacharacters until a comma template
		    # delimiter is encountered.  Note that a comma within []
		    # is not a template delimiter.

		    output ('%')
		    output ('%')
		    output (CH_DELIM)

		    level = 0
		    for (;  Memc[ip] != EOS;  ip=ip+1) {
			ch = Memc[ip]
			if (ch == ',') {		# ,
			    if (level <= 0)
				break			# exit loop
			    else {
			        escape (ch)
			    }
			} else if (ch == '[') {		# [
			    escape (ch)
			    level = level + 1
			} else if (ch == ']') {		# ]
			    output (ch)
			    level = level - 1
			} else if (ch == '*') {		# *
			    escape (ch)
			} else				# normal chars
			    output (ch)
		    }
		    output ('%')
		    ip = ip - 1
		}

	    } else if (ch == '@') {
		# List file reference.  Output the CH_DELIM code before the @
		# to prevent further translations on the image section names
		# returned from the list file, e.g., "CH_DELIM // @listfile".

		# See if we're asking to expand the contents of the file,
		# e.g. as in "@@listfile" where 'listfile' contains MEFs
		# or tables we later expand.
		expand = NO
	        if (Memc[ip+1] == '@')
		    expand = YES

		# Break out the listfile from the filtering expression.
        
		index = 1
	        nchars = imx_get_element (Memc[ip], index, elem, SZ_LINE)
		ip = ip + strlen(elem) - 1

		nchars = imx_parse (elem, lfile, lindex, lextname, 
				lextver, lexpr, lsec, likparams, SZ_LINE)

		if (DEBUG) {
		    call eprintf ("imtopen:  lfile='%s'  lexpr='%s' ip='%s'\n")
			call pargstr (lfile)
			call pargstr (lexpr)
			call pargstr (Memc[ip])
		}
		    
	    
		exp = NULL
		type = imx_filetype (lfile)
		switch (type) {
		case IMT_IMAGE:
		    exp = imx_imexpand (lfile, lexpr, lindex, lextname, lextver,
				likparams, lsec, nimages)

		case IMT_TABLE:
		case IMT_VOTABLE:
		    exp = imx_texpand (lfile, type, lexpr, lindex, "", nimages)

		case IMT_FILE:
		    if (strsearch (lfile, "//") > 0) {
		        call calloc (exp, SZ_FNAME, TY_CHAR)
		        call strcpy (lfile, Memc[exp], SZ_FNAME)
			nimages = 1

		    } else if (lfile[1] == '@' && strsearch(lfile, "//") == 0) {
		        exp = imx_fexpand (lfile[2], lexpr, lindex, lextname,
				lextver, likparams, lsec, nimages)
#			if (nimages > 0) {
#		            output (CH_DELIM); output ('/'); output ('/')
#			}

		    } else {
		        call calloc (exp, SZ_FNAME, TY_CHAR)
		        call strcpy (lfile, Memc[exp], SZ_FNAME)
			nimages = 1
		    }

		case IMT_DIR:
		    exp = imx_dexpand (lfile, lexpr, lindex, lextname, lextver,
				likparams, lsec, nimages)
		}

		if (DEBUG) {
		    call eprintf ("expand:   exp='%s' len=%d  nim=%d\n")
		        call pargstr (Memc[exp])
			call pargi (strlen(Memc[exp]))
			call pargi (nimages)
		}
		    
		    
		# Copy to the output template string.
		len = strlen (Memc[exp])
		if (nimages > 0) {
		    if ((fnt_len + len) >= max_fnt) {
			max_fnt = max_fnt + len + 1
			if (fnt != NULL)
			    call realloc (fnt, max_fnt, TY_CHAR)
			else
			    call calloc (fnt, max_fnt, TY_CHAR)
			op = fnt
			if (fnt_len > 0)
			    op = fnt + strlen (Memc[fnt])
		    }
		    for (i=0; i < len; i=i+1)
		        output (Memc[exp+i])
		    Memc[op+1] = EOS
		    fnt_len = fnt_len + strlen (Memc[exp])
		}

		if (exp != NULL)
		    call mfree (exp, TY_CHAR)
		nimages = 0

	    } else
		output (ch)
	}
	output ('\0')
	Memc[op] = EOS


	# Clean up the expanded template string in case there were selection
	# filters that rejected images and we have extra commas in the string.
	len = strlen (Memc[fnt])
	if (Memc[fnt+len-1] == ',') {		# kill trailing commas
	    for (ip=fnt+len-1; Memc[ip] == ',' && ip >= fnt; ip=ip-1)
		Memc[ip] = '\0'
	}
	if (Memc[fnt] == ',') {
	    for (ip=fnt; Memc[ip] == ','; )	# skip leading commas
		ip = ip + 1
	    for (op=fnt; Memc[ip] != EOS; ip=ip+1) {
		Memc[op] = Memc[ip]
		op = op + 1
	    }
	    Memc[op] = '\0'
	}

	if (DEBUG) {
	    call eprintf ("imxopen:  fnt='%s'\n")
		call pargstr (Memc[fnt])
	}

	    
	# Open the template string using the filename list.
	listp = fntopnb (Memc[fnt], sort)

	# Clean up.
	call mfree (fnt, TY_CHAR)
	call mfree (intmp, TY_CHAR)

	return (listp)
end
