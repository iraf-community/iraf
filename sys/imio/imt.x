# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

.help imt
.nf ___________________________________________________________________________
IMT -- Image template package.

The image template package is based upon the filename template package, the
main difference being that the IMT package knows about the use of [] in image
names, e.g., for image sections or cluster indices.

	list = imtopenp (clparam)

	 list = imtopen (template)
	       imtclose (list)
  nchars|eof = imtgetim (list, image, maxch)
 nchars|eof = imtrgetim (list, index, image, maxch)
           len = imtlen (list)
		 imtrew (list)

An image template consists of a comma delimited list of one or more patterns.
Each pattern consists of a filename template optionally followed by a cluster
index or image section.

	filename_template [image stuff] , ...

In the simplest case a simple alphanumeric image or file name may be given.
Template expansion is carried out by parsing off the [] image stuff, calling
FNTOPNB to expand the filename template, and then appending the [] string to
each output filename returned by FNTGFNB.  Multiple adjacent [] sequences are
permitted and are treated as one long string.

The [ must be escaped to be included in the filename template.  The escape
will be passed on, causing the [ to be passed through into the file output
filename.  This prevents use of the [chars] character class notation in image
templates; the [] are either interpreted as part of the image specification,
or as part of the filename.
.endhelp _____________________________________________________________________

define	SZ_FNT		16384
define	CH_DELIM	20B		# used to flag image section


# IMTOPENP -- Open an image template obtained as the string value of a CL
# parameter.

pointer	procedure imtopenp (param)

char	param[ARB]		# CL parameter with string value template
pointer	sp, template, imt
pointer	imtopen()
errchk	clgstr

begin
	call smark (sp)
	call salloc (template, SZ_FNT, TY_CHAR)

	call clgstr (param, Memc[template], SZ_FNT)
	imt = imtopen (Memc[template])

	call sfree (sp)
	return (imt)
end


# IMTOPEN -- Open an image template.  The filename template package is
# sophisticated enough to do all the necessary filename editing, etc., so all
# we need do is recast the image notation into a FNT edit operation, e.g.,
# `*.imh[*,-*]' becomes `*.hhh%%?\[\*\,-\*]%', with the ? (CH_DELIM, actually
# an unprintable ascii code) being included to make it easy to locate the
# section string in the filenames returned by FNT.  We then open the resultant
# template and perform the inverse mapping upon the filenames returned by FNT.

pointer	procedure imtopen (template)

char	template[ARB]		# image template

int	sort, level, ip, ch
pointer	sp, listp, fnt, op
define	output {Memc[op]=$1;op=op+1}
int	fntopnb(), strlen()

begin
	call smark (sp)
	call salloc (fnt, max(strlen(template)*2, SZ_FNT), TY_CHAR)

	# Sorting is disabled as input and output templates, derived from the
	# same database but with string editing used to modify the output list,
	# may be sorted differently as sorting is performed upon the edited
	# output list.

	sort = NO

	op = fnt
	for (ip=1;  template[ip] != EOS;  ip=ip+1) {
	    ch = template[ip]

	    if (ch == '[') {
		if (ip > 1 && template[ip-1] == '!') {
		    # ![ -- Pass a [ to FNT (character class notation).
		    Memc[op-1] = '['

		} else if (ip > 1 && template[ip-1] == '\\') {
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
		    for (;  template[ip] != EOS;  ip=ip+1) {
			ch = template[ip]
			if (ch == ',') {		# ,
			    if (level <= 0)
				break			# exit loop
			    else {
				output ('\\')
				output (ch)
			    }
			} else if (ch == '[') {		# [
			    output ('\\')
			    output (ch)
			    level = level + 1
			} else if (ch == ']') {		# ]
			    output (ch)
			    level = level - 1
			} else if (ch == '*') {		# *
			    output ('\\')
			    output (ch)
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

		output (CH_DELIM)
		output ('/')
		output ('/')
		output (ch)

	    } else 
		output (ch)
	}

	Memc[op] = EOS
	listp = fntopnb (Memc[fnt], sort)

	call sfree (sp)
	return (listp)
end


# IMTGETIM -- Get the next image name from the image template.  FNT returns a
# filename with optional appended image section (preceded by the CH_DELIM
# character).  Our job is to escape any [ in the filename part of the image
# name to avoid interpretation of these chars as image section characters by
# IMIO.  The CH_DELIM is deleted and everything following is simply copied
# to the output.

int procedure imtgetim (imt, outstr, maxch)

pointer	imt			# image template descriptor
char	outstr[ARB]		# output string
int	maxch			# max chars out

int	nchars
pointer	sp, buf
int	fntgfnb(), imt_mapname()
errchk	fntgfnb

begin
	call smark (sp)
	call salloc (buf, SZ_PATHNAME, TY_CHAR)

	if (fntgfnb (imt, Memc[buf], SZ_PATHNAME) == EOF) {
	    outstr[1] = EOS
	    call sfree (sp)
	    return (EOF)
	}

	nchars = imt_mapname (Memc[buf], outstr, maxch)
	call sfree (sp)
	return (nchars)
end


# IMTRGETIM -- Like imt_getim, but may be used to randomly access the image
# list.

int procedure imtrgetim (imt, index, outstr, maxch)

pointer	imt			# image template descriptor
int	index			# list element to be returned
char	outstr[ARB]		# output string
int	maxch			# max chars out

int	nchars
pointer	sp, buf
int	fntrfnb(), imt_mapname()
errchk	fntrfnb

begin
	call smark (sp)
	call salloc (buf, SZ_PATHNAME, TY_CHAR)

	if (fntrfnb (imt, index, Memc[buf], SZ_PATHNAME) == EOF) {
	    outstr[1] = EOS
	    call sfree (sp)
	    return (EOF)
	}

	nchars = imt_mapname (Memc[buf], outstr, maxch)
	call sfree (sp)
	return (nchars)
end


# IMTLEN -- Return the number of image names in the expanded list.

int procedure imtlen (imt)

pointer	imt			# image template descriptor
int	fntlenb()

begin
	return (fntlenb (imt))
end

    
# IMTREW -- Rewind the expanded image list.

procedure imtrew (imt)

pointer	imt			# image template descriptor

begin
	call fntrewb (imt)
end

    
# IMTCLOSE -- Close an image template.

procedure imtclose (imt)

pointer	imt			# image template descriptor

begin
	call fntclsb (imt)
end


# IMT_MAPNAME -- Translate the string returned by FNT into an image
# specification suitable for input to IMIO.

int procedure imt_mapname (fnt, outstr, maxch)

char	fnt[ARB]		# FNT string
char	outstr[ARB]		# output string
int	maxch

int	ip, op

begin
	op = 1
	for (ip=1;  fnt[ip] != EOS;  ip=ip+1)
	    if (fnt[ip] == '[') {
		outstr[op] = '\\'
		op = op + 1
		outstr[op] = '['
		op = op + 1

	    } else if (fnt[ip] == CH_DELIM) {
		for (ip=ip+1;  fnt[ip] != EOS;  ip=ip+1) {
		    outstr[op] = fnt[ip]
		    op = op + 1
		    if (op > maxch)
			break
		}
		break

	    } else {
		outstr[op] = fnt[ip]
		op = op + 1
		if (op > maxch)
		    break
	    }
	
	outstr[op] = EOS
	return (op - 1)
end
