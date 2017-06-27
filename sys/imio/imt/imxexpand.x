# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>
include <mach.h>
include <fio.h>
include <finfo.h>
include	<ctype.h>
include	<diropen.h>

include "imx.h"
include <votParse_spp.h>


define      SZ_BUF          8192	# name buffer string


# IMX_IMEXPAND -- Expand a template of FITS files into a list of image
# extensions.

pointer procedure imx_imexpand (input, expr, index, extname, extver, ikparams, 
				section, nimages)

char	input[ARB]			# List of ME file names
char	expr[ARB]			# Filtering expression
char	index[ARB]			# Range list of extension indexes
char	extname[ARB]			# Patterns for extension names
char 	extver[ARB]			# Range list of extension versions
char	ikparams[ARB]			# Image kernel parameters
char	section[ARB]			# Image section parameters
int	nimages				# Number of output images

int	lindex				# List index number?
int	lname				# List extension name?
int	lver				# List extension version?

pointer	in, out				# Pointer to output string
pointer	sp, sif, image, listout
int	list, len, maxch

int	imx_extns(), strlen(), fntgfnb(), fntlenb()
pointer	imx_escape()
bool	imx_sifmatch()

begin
	call smark (sp)
	call salloc (in, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)


	lindex  = YES 			# expansion parameters
	lname   = NO
	lver    = NO
	out	= NULL
	len     = 0
	nimages = 0
	maxch   = SZ_LISTOUT

	call aclrc (Memc[in], SZ_FNAME)
	if (input[1] == '@')
	    call strcpy (input[2], Memc[in], SZ_FNAME)
	else
	    call strcpy (input, Memc[in], SZ_FNAME)

	# Get the list.
	list = imx_extns (Memc[in], "IMAGE", index, extname, extver, 
		    lindex, lname, lver, ikparams, section, expr, YES)

	if (list == NULL || fntlenb (list) == 0) {
	    call calloc (out, SZ_LINE, TY_CHAR)
	    call strcpy (Memc[in], Memc[out], SZ_LINE)
	    if (section[1] != EOS) {
		call strcat ("\\[", Memc[out], maxch)
		call strcat (section, Memc[out], maxch)
		call strcat ("]", Memc[out], maxch)
	    }
	    if (ikparams[1] != EOS) {
		call strcat ("\\[", Memc[out], maxch)
		call strcat (ikparams, Memc[out], maxch)
		call strcat ("]", Memc[out], maxch)
	    }

	    if (index[1] == EOS && imx_sifmatch (Memc[out], expr)) {
	        nimages = 1
	        sif = imx_escape (Memc[out], index, extname, extver, ikparams, 
		    section, expr, maxch)
	    } else
	        call calloc (sif, SZ_LINE, TY_CHAR)
	    call mfree (out, TY_CHAR)
	    return (sif)
	}

	# Format the output and set the number of images.
	call calloc (listout, maxch, TY_CHAR)
	iferr {
	    while (fntgfnb (list, Memc[image], SZ_FNAME) != EOF) {
		nimages = nimages + 1
		if (nimages > 1) {
		    call strcat (",", Memc[listout], maxch)
		    len = len + 1
		}
		if ((len + strlen (Memc[image])) >= maxch) {
		    maxch = maxch + SZ_LISTOUT
		    call realloc (listout, maxch, TY_CHAR)
		}

		call strcat (Memc[image], Memc[listout], maxch)
		len = len + strlen (Memc[image])

#		if (section[1] != EOS) {
#		    call strcat ("[", Memc[listout], maxch)
#		    call strcat (section, Memc[listout], maxch)
#		    call strcat ("]", Memc[listout], maxch)
#		    len = len + strlen (section) + 2
#		}
	    }

	    # Escape the output image specification in a form that is correct
	    # for the filename template interface.

	    out = imx_escape (Memc[listout], index, extname, extver, ikparams, 
		section, expr, maxch)

	} then {
	    call fntclsb (list)
	    call sfree (sp)
	    call error (1, "Output list format is too long")
	}
	call fntclsb (list)
	call sfree (sp)

	return (out)
end


# IMX_FEXPAND -- Expand a template of files into a list of images names.

pointer procedure imx_fexpand (input, expr, index, extname, extver, ikparams, 
				section, nimages)

char	input[ARB]			# List of ME file names
char	expr[ARB]			# Filtering expression
char	index[ARB]			# Range list of extension indexes
char	extname[ARB]			# Patterns for extension names
char 	extver[ARB]			# Range list of extension versions
char	ikparams[ARB]			# Image kernel parameters
char	section[ARB]			# Image section parameters
int	nimages				# Number of output images

pointer	sp, name, exp, lexp, nexp
int	fd, ip, op, len, elen, nlines, nims, maxch, nchars, level
bool	do_proc
char	line[SZ_LINE], buf[SZ_LINE], ch

define  output {buf[op]=$1;op=op+1}


int	open(), getline(), strlen(), stridx()
pointer	imx_imexpand()

begin
	iferr (fd = open (input, READ_ONLY, TEXT_FILE)) {
	    call error (1, "Cannot open @file")
	    return (NULL)
	}

	call smark (sp)
	call salloc (name, SZ_PATHNAME, TY_CHAR)

	maxch = SZ_FNT
	call calloc (exp, maxch, TY_CHAR)
	call aclrc (Memc[exp], maxch)

#call eprintf (
#  "fexpand: index='%s' name='%s' ver='%s' sec='%s' ik='%s' expr='%s'\n")
#    call pargstr (index) ; call pargstr (extname) ; call pargstr (extver) ;
#    call pargstr (section) ; call pargstr (ikparams) ; call pargstr (expr)

	nlines = 0
	nchars = 0
	nimages = 0

	while (getline (fd, line) > 0) {
	    len = strlen (line)
	    line[len] = EOS			# kill newline
	    nlines = nlines + 1

	    call aclrc (Memc[name], SZ_PATHNAME)
	    call sprintf (Memc[name], SZ_PATHNAME, "@%s")
		call pargstr (line)

	    lexp = 0
	    do_proc = (index[1]!=EOS || section[1]!=EOS || 
		       expr[1]!=EOS || extname[1]!=EOS)

	    if (input[1] == '@' || do_proc) {

		# We're either being asked to expand what is presumably a
		# image name in the form of an @@file input, or else we've
		# added image sections, expressions, etc where the correct
		# output specification is the expanded image name.

	        lexp = imx_imexpand (Memc[name], expr, index, extname, extver,
			ikparams, section, nims)

	        elen = 0
	        if (lexp != NULL && Memc[lexp] != EOS)
		    elen = strlen (Memc[lexp])

		# Reallocate space is the output name if needed.
	        #if ((nchars + elen) >= (maxch - SZ_FNAME)) {
	        if ((nchars + elen) >= maxch) {
		    call calloc (nexp, maxch + SZ_FNT, TY_CHAR)
		    call amovc (Memc[exp], Memc[nexp], maxch)
		    call mfree (exp, TY_CHAR)
		    maxch = maxch + SZ_FNT
		    exp = nexp
	        }

		# Create a comma-delimited list.
	        if (nlines > 1) 
		    call strcat (",", Memc[exp], maxch)
	        if (lexp != NULL && Memc[lexp] != EOS) {
	            call strcat (Memc[lexp], Memc[exp], maxch)
	            nchars = nchars + elen + 1
	        }
	        nimages = nimages + nims
	    } else {
	        if (nlines > 1) {
		    call strcat (",", Memc[exp], maxch)
	            nchars = nchars + 1
		}
		if (stridx ('[', line) != 0) {
		    call aclrc (buf, SZ_LINE)
		    op = 1
		    for (ip=1; line[ip] != EOS; ip=ip+1) {
			if (line[ip] == '[') {
			    output ('%')
			    output ('%')
			    output (CH_DELIM)

                    	    level = 0
                    	    for (;  line[ip] != EOS;  ip=ip+1) {
                        	ch = line[ip]
                        	if (ch == ',') {                # ,
                            	    if (level <= 0)
                                	break                   # exit loop
                            	    else {
                                	output ('\\')
                                	output (ch)
                            	    }
                        	} else if (ch == '[') {         # [
                            	    output ('\\')
                            	    output (ch)
                            	    level = level + 1
                        	} else if (ch == ']') {         # ]
                            	    output (ch)
                            	    level = level - 1
                        	} else if (ch == '*') {         # *
                            	    output ('\\')
                            	    output (ch)
                        	} else                          # normal chars
                            	    output (ch)
                    	    }
                    	    output ('%')
                    	    ip = ip - 1

			    break
			}
			buf[op] = line[ip]
			op = op + 1
		    }
	            call strcat (buf, Memc[exp], maxch)
	            nchars = nchars + strlen (buf)

		} else {
	            call strcat (line, Memc[exp], maxch)
	            nchars = nchars + strlen (line)
		}

	        nchars = nchars + len + 1
	        nimages = nimages + 1

		# Reallocate space is the output name if needed.

	        if ((nchars + SZ_LINE) >= maxch) {
		    call calloc (nexp, maxch + SZ_FNT, TY_CHAR)
		    call amovc (Memc[exp], Memc[nexp], maxch)
		    call mfree (exp, TY_CHAR)
		    maxch = maxch + SZ_FNT
		    exp = nexp
	        }
	    }
	    call mfree (lexp, TY_CHAR)
	}

	call close (fd)			# clean up
	call sfree (sp)

	return (exp)
end


# IMX_TEXPAND -- Expand a template of tables into a list of images.

pointer procedure imx_texpand (input, type, expr, index, fmt, nimages)

char	input[ARB]			# Input table name
int	type				# Table type
char	expr[ARB]			# Filtering expression
char	index[ARB]			# Range list of table rows
char	fmt[ARB]			# Requested file format
int	nimages				# Number of output images

char	fname[SZ_PATHNAME]		# File name to open
char	ofname[SZ_PATHNAME]
pointer	sp, exp, nodename
int	ip, vfd, status, delim

pointer imx_votable(), imx_table()
int	vfnopen(), vfnmapu(), strncmp(), ki_gnode()

begin
	call smark (sp)
	call salloc (nodename, SZ_PATHNAME, TY_CHAR)

	exp = NULL			# initialize values
	nimages = 0

	# Get the base filename without the '@' prefix.
	if (input[1] == '@')
	    call strcpy (input[2], fname, SZ_PATHNAME)
	else
	    call strcpy (input, fname, SZ_PATHNAME)

        # Map input VFN to OSFN.
        ip = 1
        if (strncmp (fname, "http://", 7) == 0) {
            call strcpy (fname, ofname, SZ_PATHNAME)
        } else {
            vfd = vfnopen (fname, READ_ONLY)
            status = vfnmapu (vfd, ofname, SZ_PATHNAME)
            call vfnclose (vfd, VFN_NOUPDATE)

            # If the file resides on the local node strip the node name,
            # returning a legal host system filename as the result.
            if (ki_gnode (ofname, Memc[nodename], delim) == 0)
                ip = delim + 1
        }


	# Now process the file.  For a VOTable we parse the file and
	# extract the acref columns as cached image names, for ascii
	# tables we read the URLs directly but likewise returned the
	# cache name.

	if (type == IMT_TABLE)
	    exp = imx_table (ofname[ip], index, nimages)
	else if (type == IMT_VOTABLE)
	    exp = imx_votable (ofname[ip], expr, index, fmt, nimages)

	call sfree (sp)
	return (exp)
end


# IMX_DEXPAND -- Expand a directory into a list of images.

pointer procedure imx_dexpand (input, expr, index, extname, extver, ikparams, 
    sec, nimages)

char	input[ARB]			# List of MEF file names
char	expr[ARB]			# Filtering expression
char	index[ARB]			# Index range
char	extname[ARB]			# Extension name
char	extver[ARB]			# Extension version
char	ikparams[ARB]			# IKI parameters
char	sec[ARB]			# Image section
int	nimages				# Number of output images

pointer	sp, exp, nodename, imname, listout
int     dir, len, llen, nim, ip, delim, vfd, status, maxlen
char    dirname[SZ_PATHNAME], ofname[SZ_PATHNAME], pdir[SZ_PATHNAME]
char    fpath[SZ_PATHNAME], fname[SZ_PATHNAME]
		
pointer imx_imexpand ()
int	vfnopen(), vfnmapu(), ki_gnode(), imx_filetype()
int     strlen(), diropen(), isdirectory(), getline()

begin
	call smark (sp)
	call salloc (nodename, SZ_PATHNAME, TY_CHAR)

	# Get the base filename without the '@' prefix.
	if (input[1] == '@') {
	    if (input[2] == '@')
	        call strcpy (input[3], dirname, SZ_PATHNAME)
	    else
	        call strcpy (input[2], dirname, SZ_PATHNAME)
	} else
	    call strcpy (input, dirname, SZ_PATHNAME)

	# Remove trailing '/' or '$' from dir
	len = strlen (dirname)		
	if (dirname[len] == '/')
	    dirname[len] = EOS

        # Map input VFN to OSFN.
        ip = 1
        vfd = vfnopen (dirname, READ_ONLY)
        status = vfnmapu (vfd, ofname, SZ_PATHNAME)
        call vfnclose (vfd, VFN_NOUPDATE)

        # If the file resides on the local node strip the node name,
        # returning a legal host system filename as the result.
        if (ki_gnode (ofname, Memc[nodename], delim) == 0)
            ip = delim + 1

	call sfree (sp)

        # Otherwise, read through the directory and remove the contents.
        dir = diropen (ofname, SKIP_HIDDEN_FILES)

	maxlen = SZ_LISTOUT
	call calloc (listout, SZ_LISTOUT, TY_CHAR)
	llen = 0
        while (getline (dir, fname) != EOF) {
            len = strlen (fname)
            fname[len] = '\0'
            
            len = strlen (ofname)
	    if (ofname[len] == '/' || ofname[len] == '$')
	        call sprintf (fpath, SZ_PATHNAME, "%s%s")
	    else
	        call sprintf (fpath, SZ_PATHNAME, "%s/%s")
		    call pargstr (dirname)
		    call pargstr (fname)

	    llen = llen + strlen (fpath)

            # We only test plain files, skip directories.
	    if (isdirectory (fpath, pdir, SZ_PATHNAME) > 0)
		next

	    if (imx_filetype (fpath) == IMT_IMAGE) {

		if (input[2] == '@')
		    imname = imx_imexpand (fpath, expr, index, extname, extver, 
		        ikparams, sec, nim)
		else {
		    call calloc (imname, SZ_PATHNAME, TY_CHAR)
		    call strcpy (fpath, Memc[imname], SZ_PATHNAME)
		}

		if (imname != NULL && Memc[imname] != EOS) {
		    nimages = nimages + 1

		    if (nimages > 1) {
		        call strcat (",", Memc[listout], maxlen)
		        llen = llen + 1
		    }
		    if ((llen + strlen (Memc[imname])) >= maxlen) {
		        maxlen = maxlen + SZ_LISTOUT
		        call realloc (listout, maxlen, TY_CHAR)
		    }

		    call strcat (Memc[imname], Memc[listout], maxlen)
		    llen = llen + strlen (Memc[imname])

		    if (sec[1] != EOS) {
		        call strcat ("[", Memc[listout], maxlen)
		        call strcat (sec, Memc[listout], maxlen)
		        call strcat ("]", Memc[listout], maxlen)
		        llen = llen + strlen (sec) + 2
		    }

		    if (imname != NULL)
		        call mfree (imname, TY_CHAR)
		}
	    }
        }

	return (listout)
end


# IMX_VOTABLE -- Read a VOTable, extracting the column of access references
# as the image list.

pointer procedure imx_votable (input, expr, index, fmt, nimages)

char	input[ARB]			# List of ME file names
char	expr[ARB]			# Filtering expression
char	index[ARB]			# Range list of table rows
char	fmt[ARB]			# Requested file format
int	nimages				# Number of output images

pointer	vot, exp, ranges
int	nranges, tfd
char	tfile[SZ_PATHNAME]

int	open()
int	imx_decode_ranges()
pointer	imx_votselect(), votinit()
bool	envgetb()

begin
	# Create a temp file for the parsed access references.
	call mktemp ("tmp$vot", tfile, SZ_PATHNAME)
	iferr (tfd = open (tfile, NEW_FILE, TEXT_FILE)) {
	    nimages = 0
	    return (NULL)
	}

	# Expand the index string into a range structure.
	if (index[1] != EOS) {
	    call calloc (ranges, 3 * SZ_RANGE, TY_INT)
            if (imx_decode_ranges (index, Memi[ranges], SZ_RANGE,
                 nranges, YES) == ERR) {
		    call eprintf ("error parsing range '%s'\n")
		        call pargstr (index)
	    }
	} else
	    ranges = NULL
 
	# Initialize the VOT struct and parse the table.
	vot = votinit (input)

	# Select the column from the VOTable with the access reference.
	exp = imx_votselect (vot, tfd, fmt, ranges, nimages)

	call mfree (ranges, TY_INT)
	call votclose (vot)			# close the files
	call close (tfd)

	return (exp)
end


# IMX_VOTSELECT -- Select the access reference column.

pointer procedure imx_votselect (vot, fd, fmt, ranges, nimages)

pointer	vot					#i VOTable struct pointer
int	fd					#i filename of selected rows
char	fmt[ARB]				#i file format
pointer	ranges					#i ranges struct pointer
int	nimages					#o no. selected images

pointer	exp
int	col, len, clen, maxlen
char 	acref_ucd[SZ_FNAME], imfmt[SZ_FNAME], ucd_col[SZ_FNAME]
char	acref[SZ_LINE], ucd[SZ_FNAME], buf[SZ_LINE], cfname[SZ_PATHNAME]
int	i, rownum, field, acref_col, acfmt_col

int	strcmp(), strsearch(), strlen(), vx_getNext()
bool	imx_in_range()

begin
	# Figure out which table column we want.  Note that we assume there
	# is only one <RESOURCE> element.  The caller may pass in a specific
	# column to be used, otherwise look for for the named UCD.

	col = 0						# FIXME
	call aclrc (ucd_col, SZ_FNAME) 			# FIXME
	call strcpy ("fits", imfmt, SZ_FNAME) 		# FIXME

	call aclrc (acref_ucd, SZ_FNAME)
	if (col > 0) {
	    acref_col = col
	} else {
	    if (ucd_col[1] != EOS)
		call strcpy (ucd_col, acref_ucd, SZ_FNAME)
	    else
		call strcpy (DEF_ACREF_UCD, acref_ucd, SZ_FNAME)

	    # Find the access reference column number.
            i = 0
            for (field=VOT_FIELD(vot); field > 0; field=vx_getNext (field)) {
		call aclrc (ucd, SZ_FNAME)
                call vx_getAttr (field, "ucd", ucd, SZ_FNAME)
		if (strcmp (ucd, acref_ucd) == 0) {
		    acref_col = i
		} else if (strcmp (ucd, DEF_FORMAT_UCD) == 0)
		    acfmt_col = i
	        i = i + 1
	    }
	}

	maxlen = SZ_BUF
	call calloc (exp, maxlen, TY_CHAR)

	# Download the files.
	for (i=0; i < VOT_NROWS(vot); i=i+1) {
	    call vx_getTableCell (VOT_TDATA(vot), i, acfmt_col, imfmt, SZ_FNAME)

	    if (fmt[1] == EOS || (fmt[1] != EOS && strsearch(imfmt, fmt) > 0)) {
	        call vx_getTableCell (VOT_TDATA(vot), i, acref_col, 
		    acref, SZ_LINE)

	        # Do the row selection based on the index string.
		rownum = i + 1
	        if (ranges != NULL && ! imx_in_range (Memi[ranges], rownum))
		    next

	        # Generate a unique cache filename based on the URL.
	        call fcname ("cache$", acref, "url", cfname, SZ_PATHNAME)

	        # Append the cache name to the output string. Reallocate the
	        # string pointer if needed.
	        clen = strlen (cfname)
	        if ((len + clen) >= maxlen) {
		    maxlen = maxlen + SZ_BUF
		    call realloc (exp, maxlen, TY_CHAR)
	        }
	        len = len + clen

	        if (nimages == 0) {
		    call strcpy (cfname, Memc[exp], maxlen)
	        } else {
		    call strcat (",", Memc[exp], maxlen)
	            call strcat (cfname, Memc[exp], maxlen)
	        }
	        call aclrc (buf, SZ_LINE)

		# Write the URL to the download file.
		call fprintf (fd, "%s\n")
		    call pargstr (acref)

		nimages = nimages + 1
	    }
	}

	return (exp)
end


# IMX_TABLE -- Read an ASCII text table of URLs and create the list
# of files to process.  We apply the list index to do row selection
# and return a list of cached filenames.

pointer procedure imx_table (input, index, nimages)

char	input[ARB]			# List of ME file names
char	index[ARB]			# Range list of table rows
int	nimages				# Number of output images

pointer	exp, ranges
int	rownum, nranges, fd, len, clen, maxlen
char	buf[SZ_LINE], cfname[SZ_PATHNAME]

int	open(), getline(), strlen()
int	imx_decode_ranges()
bool	imx_in_range(), envgetb()

begin
	call aclrc (buf, SZ_LINE)
	iferr (fd = open (input, READ_ONLY, TEXT_FILE))
	    call syserr (SYS_FOPEN)

	maxlen = SZ_BUF
	call calloc (exp, maxlen, TY_CHAR)

	call calloc (ranges, 3 * SZ_RANGE, TY_INT)
	if (index[1] != EOS) {
            if (imx_decode_ranges (index, Memi[ranges], SZ_RANGE,
                 nranges, YES) == ERR) {
		    call eprintf ("error parsing range '%s'\n")
		        call pargstr (index)
	    }
	}
 
	len = 0
	nimages = 0
	rownum = 0
	while (getline (fd, buf) != EOF) {

	    # Skip comments and blank lines.
	    if (buf[1] == '\n' || buf[1] == '#')
		next
	    else
		rownum = rownum + 1

	    # Do the row selection based on the index string.
	    if (index[1] != EOS && ! imx_in_range (Memi[ranges], rownum))
		next

	    # Generate a unique cache filename based on the URL.
	    call fcname ("cache$", buf, "url", cfname, SZ_PATHNAME)

	    # Append the cache name to the output string. Reallocate the
	    # string pointer if needed.
	    clen = strlen (cfname)
	    if ((len + clen) >= maxlen) {
		maxlen = maxlen + SZ_BUF
		call realloc (exp, maxlen, TY_CHAR)
	    }
	    len = len + clen

	    if (nimages == 0) {
		call strcpy (cfname, Memc[exp], maxlen)
	    } else {
		call strcat (",", Memc[exp], maxlen)
	        call strcat (cfname, Memc[exp], maxlen)
	    }
	    call aclrc (buf, SZ_LINE)

	    nimages = nimages + 1
	}

	call mfree (ranges, TY_INT)
	call close (fd)

	return (exp)
end


# IMX_EXTNS -- Expand a template of ME files into a list of image extensions.

int procedure imx_extns (files, exttype, index, extname, extver, 
		lindex, lname, lver, ikparams, section, expr, err)

char	files[ARB]		#I List of ME files
char	exttype[ARB]		#I Extension type string
char	index[ARB]		#I Range list of extension indexes
char	extname[ARB]		#I Patterns for extension names
char	extver[ARB]		#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	expr[ARB]		#I Selection expression
char	ikparams[ARB]		#I Image kernel parameters
char	section[ARB]		#I Image section parameters
int	err			#I Print errors?
int	list			#O Image list

int	i, fd, create
pointer	sp, temp, fname, imname, sec, rindex, rextver, ikp, str
int	fntopnb(), fntgfnb()
int	imx_decode_ranges(), nowhite(), open()
errchk	open, imx_extn, delete

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (imname, SZ_FNAME, TY_CHAR)
	call salloc (sec, SZ_FNAME, TY_CHAR)
	call salloc (ikp, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	# Expand parameters.
	list = fntopnb (files, NO)
	call salloc (rindex, 3*SZ_RANGE, TY_INT)
	if (imx_decode_ranges (index, Memi[rindex], SZ_RANGE, i, create) == ERR)
	    call error (1, "Bad index range list")

	rextver = NULL
	if (nowhite (extver, Memc[str], SZ_LINE) > 0) {
	    call salloc (rextver, 3*SZ_RANGE, TY_INT)
	    if (imx_decode_ranges (Memc[str], Memi[rextver], SZ_RANGE, 
		i, create) == ERR)
		    call error (1, "Bad extension version range list")
	}

	call aclrc (Memc[ikp], SZ_LINE)
	i = nowhite (ikparams, Memc[ikp], SZ_LINE)

	# Expand ME files into list of image extensions in a temp file.
	call mktemp ("@tmp$iraf", Memc[temp], SZ_FNAME)
	fd = open (Memc[temp+1], NEW_FILE, TEXT_FILE)
	while (fntgfnb (list, Memc[fname], SZ_FNAME) != EOF) {
	    call imgimage (Memc[fname], Memc[imname], SZ_FNAME)
	    call imgsection (Memc[fname], Memc[sec], SZ_FNAME)

	    call imx_extn (fd, Memc[imname], exttype, expr, rindex, extname, 
		rextver, lindex, lname, lver, Memc[ikp], section, 
		create, err)
	}
	call fntclsb (list)
	call close (fd)

	# Return list.
	list = fntopnb (Memc[temp], NO)
	call delete (Memc[temp+1])
	call sfree (sp)

	return (list)
end


# IMX_EXTN -- Expand a single ME file into a list of image extensions.
# The image extensions are written to the input file descriptor.

procedure imx_extn (fd, fname, exttype, expr, index, extname, extver, lindex, 
		lname, lver, ikparams, section, create, err)

int	fd			#I File descriptor for list 
char	fname[SZ_FNAME]		#I File image name (without kernel or image sec)
char	exttype[SZ_FNAME]	#I File extension type
char	expr[ARB]		#I Selection expression
pointer	index			#I Range list of extension indexes
char	extname[ARB]		#I Pattern for extension names
pointer	extver			#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
char	section[ARB]		#I Image section
int	create			#I Create names from index range?
int	err			#I Print errors?

pointer	sp, image, name, type, str, im
int	i, j, ver

pointer	immap()
int	imx_get_next_number(), errcode(), imgeti(), stridxs(), strcmp()
bool	imx_in_range(), imx_extmatch(), imx_matchexpr(), imx_sifmatch()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (type, SZ_FNAME, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	i = -1
	while (imx_get_next_number (Memi[index], i) != EOF) {
	    j = stridxs ("[", fname)
	    if (j > 0) {
		if (i > 0)
		    break
		call strcpy (fname, Memc[image], SZ_FNAME)
	    } else {
		call sprintf (Memc[image], SZ_FNAME, "%s[%d]")
		    call pargstr (fname)
		    call pargi (i)
	    }

	    if (section[1] != EOS) {
		call strcat ("[", Memc[image], SZ_FNAME)
		call strcat (section, Memc[image], SZ_FNAME)
		call strcat ("]", Memc[image], SZ_FNAME)
	    }

	    # We know the extension doesn't exist, generate the name.
	    if (create == YES) {
		call fprintf (fd, "%s")
		    call pargstr (Memc[image])
		if (section[1] != EOS) {
		    call fprintf (fd, "[%s]")
		        call pargstr (section)
		}
		call fprintf (fd, "\n")
		next
	    }


	    iferr (im = immap (Memc[image], READ_ONLY, 0)) {
		switch (errcode()) {
		case SYS_FXFRFEOF:
		    if (i == 1) {
			if (extname[1] == EOS && imx_sifmatch (fname, expr)) {
	    		    call fprintf (fd, "%s\n")
			        call pargstr (fname)
		 	    next
			} else
			    break
		    }
		    break
		case SYS_IKIEXTN:
		    next
		case SYS_IKIOPEN:
		    switch (i) {
		    case 0:
			next
		    case 1:
			if (err == YES)
			    call erract (EA_WARN)
			break
		    default:
			break
		    }
		default:
		    call erract (EA_ERROR)
		}
	    }


	    # Check the extension type.  [NOT USED]
	    if (exttype[1] != EOS) {
		iferr (call imgstr (im, "xtension", Memc[type], SZ_FNAME))
		    Memc[type] = EOS
		if (Memc[type] != EOS && strcmp (Memc[type], exttype) != 0) {
		    call imunmap (im)
		    next
		}
	    }

#call eprintf("imx_extn: name='%s' ver='%s' expr='%s' sec='%s' iki='%s'\n")
#  call pargstr (extname) ; call pargstr (Memc[extver]) ;
#  call pargstr (expr) ; call pargstr (section) ;
#  call pargstr (ikparams) ;

	    # Check the extension name.
	    if (extname[1] != EOS) {
		iferr (call imgstr (im, "extname", Memc[name], SZ_FNAME))
		    Memc[name] = EOS
		if (!imx_extmatch (Memc[name], extname)) {
		    call imunmap (im)
		    next
		}
	    }

	    # Check the extension version.
	    if (extver != NULL) {
		iferr (ver = imgeti (im, "extver")) {
		    call imunmap (im)
		    next
		}
		if (!imx_in_range (Memi[extver], ver)) {
		    call imunmap (im)
		    next
		}
	    }

	    # Check the selection expression.
	    if (expr[1] != EOS) {
		if (!imx_matchexpr (im, expr)) {
		    call imunmap (im)
		    next
		}
	    }


	    # Set the extension name and version.
	    if (lname == YES) {
		iferr (call imgstr (im, "extname", Memc[name], SZ_LINE))
		    Memc[name] = EOS
	    } else
		Memc[name] = EOS
	    if (lver == YES) {
		iferr (ver = imgeti (im, "extver"))
		    ver = INDEFI
	    } else
		ver = INDEFI

	    # Write the image name.
	    call fprintf (fd, fname)
	    if (j == 0) {
		if (lindex == YES || (Memc[name] == EOS && IS_INDEFI(ver))) {
		    call fprintf (fd, "[%d]")
			call pargi (i)
		}
		if (Memc[name] != EOS) {
		    call fprintf (fd, "[%s")
			call pargstr (Memc[name])
		    if (!IS_INDEFI(ver)) {
			call fprintf (fd, ",%d")
			    call pargi (ver)
		    }
		    if (ikparams[1] != EOS) {
			call fprintf (fd, ",%s")
			    call pargstr (ikparams)
		    }
		    call fprintf (fd, "]")
		} else if (!IS_INDEFI(ver)) {
		    call fprintf (fd, "[extver=%d")
			call pargi (ver)
		    if (ikparams[1] != EOS) {
			call fprintf (fd, ",%s")
			    call pargstr (ikparams)
		    }
		    call fprintf (fd, "]")
		} else if (ikparams[1] != EOS) {
		    call fprintf (fd, "[%s]%%")
			call pargstr (ikparams)
		}
	    }
	    if (section[1] != EOS) {
	        call fprintf (fd, "[%s]")
		    call pargstr (section)
	    }
	    call fprintf (fd, "\n")
		
	    call imunmap (im)
	}

	call sfree (sp)
end


# IMX_DECODE_RANGES -- Parse a string containing a list of integer numbers or
# ranges, delimited by either spaces or commas.  Return as output a list
# of ranges defining a list of numbers, and the count of list numbers.
# Range limits must be positive nonnegative integers.  ERR is returned as
# the function value if a conversion error occurs.  The list of ranges is
# delimited by EOLIST.

int procedure imx_decode_ranges (range_string, ranges, max_ranges, 
		nvalues, create)

char	range_string[ARB]	# Range string to be decoded
int	ranges[3, max_ranges]	# Range array
int	max_ranges		# Maximum number of ranges
int	nvalues			# The number of values in the ranges
int	create			# generate range string?

int	ip, nrange, first, last, step, ctoi()

begin
	create = NO
	if (range_string[1] == '+') {
	    ip = 2
	    create = YES
	} else
	    ip = 1
	nvalues = 0

	do nrange = 1, max_ranges - 1 {
	    # Defaults to all nonnegative integers
	    first = FIRST
	    last = LAST
	    step = STEP

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get first limit.
	    # Must be a number, '-', 'x', or EOS.  If not return ERR.
	    if (range_string[ip] == EOS) {			# end of list
		if (nrange == 1) {
		    # Null string defaults
		    ranges[1, 1] = first
		    ranges[2, 1] = last
		    ranges[3, 1] = step
		    ranges[1, 2] = EOLIST
	    	    nvalues = MAX_INT
		    return (OK)
		} else {
		    ranges[1, nrange] = EOLIST
		    return (OK)
		}
	    } else if (range_string[ip] == '-')
		;
	    else if (range_string[ip] == 'x')
		;
	    else if (IS_DIGIT(range_string[ip])) {		# ,n..
		if (ctoi (range_string, ip, first) == 0)
		    return (ERR)
	    } else
		return (ERR)

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get last limit
	    # Must be '-', or 'x' otherwise last = first.
	    if (range_string[ip] == 'x')
		;
	    else if (range_string[ip] == '-') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, last) == 0)
		        return (ERR)
		} else if (range_string[ip] == 'x')
		    ;
		else
		    return (ERR)
	    } else
		last = first

	    # Skip delimiters
	    while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		ip = ip + 1

	    # Get step.
	    # Must be 'x' or assume default step.
	    if (range_string[ip] == 'x') {
		ip = ip + 1
	        while (IS_WHITE(range_string[ip]) || range_string[ip] == ',')
		    ip = ip + 1
		if (range_string[ip] == EOS)
		    ;
		else if (IS_DIGIT(range_string[ip])) {
		    if (ctoi (range_string, ip, step) == 0)
		        ;
		    if (step == 0)
			return (ERR)
		} else if (range_string[ip] == '-')
		    ;
		else
		    return (ERR)
	    }

	    # Output the range triple.
	    ranges[1, nrange] = first
	    ranges[2, nrange] = last
	    ranges[3, nrange] = step
	    nvalues = nvalues + abs (last-first) / step + 1
	}

	return (ERR)					# ran out of space
end


# IMX_GET_NEXT_NUMBER -- Given a list of ranges and the current file number,
# find and return the next file number.  Selection is done in such a way
# that list numbers are always returned in monotonically increasing order,
# regardless of the order in which the ranges are given.  Duplicate entries
# are ignored.  EOF is returned at the end of the list.

int procedure imx_get_next_number (ranges, number)

int	ranges[ARB]		# Range array
int	number			# Both input and output parameter

int	ip, first, last, step, next_number, remainder

begin
	# If number+1 is anywhere in the list, that is the next number,
	# otherwise the next number is the smallest number in the list which
	# is greater than number+1.

	number = number + 1
	next_number = MAX_INT

	for (ip=1;  ranges[ip] != EOLIST;  ip=ip+3) {
	    first = min (ranges[ip], ranges[ip+1])
	    last = max (ranges[ip], ranges[ip+1])
	    step = ranges[ip+2]
	    if (step == 0)
		call error (1, "Step size of zero in range list")
	    if (number >= first && number <= last) {
		remainder = mod (number - first, step)
		if (remainder == 0)
		    return (number)
		if (number - remainder + step <= last)
		    next_number = number - remainder + step
	    } else if (first > number)
		next_number = min (next_number, first)
	}

	if (next_number == MAX_INT)
	    return (EOF)
	else {
	    number = next_number
	    return (number)
	}
end


# IMX_EXTMATCH -- Match extname against a comma-delimited list of patterns.

bool procedure imx_extmatch (extname, patterns)

char	extname[ARB]		#I Extension name to match
char	patterns[ARB]		#I Comma-delimited list of patterns
bool	stat			#O Match?

int	i, j, k, sz_pat, strlen(), patmake(), patmatch(), nowhite()
pointer	sp, patstr, patbuf

begin
	stat = false

	sz_pat = strlen (patterns)
	if (sz_pat == 0)
	    return (stat)
	sz_pat = sz_pat + SZ_LINE

	call smark (sp)
	call salloc (patstr, sz_pat, TY_CHAR)
	call salloc (patbuf, sz_pat, TY_CHAR)

	i = nowhite (patterns, Memc[patstr], sz_pat)
	if (i == 0)
	    stat = true
	else if (i == 1 && Memc[patstr] == '*')
	    stat = true
	else {
	    i = 1
	    for (j=i;; j=j+1) {
		if (patterns[j] != ',' && patterns[j] != EOS)
		    next
		if (j - i > 0) {
		    if (j-i == 1 && patterns[i] == '*') {
			stat = true
			break
		    }
		    call strcpy (patterns[i], Memc[patstr+1], j-i)
		    Memc[patstr] = '^'
		    Memc[patstr+j-i+1] = '$'
		    Memc[patstr+j-i+2] = EOS
		    k = patmake (Memc[patstr], Memc[patbuf], sz_pat)
		    if (patmatch (extname, Memc[patbuf]) > 0) {
			stat = true
			break
		    }
		}
		if (patterns[j] == EOS)
		    break
		i = j + 1
	    }
	}

	call sfree (sp)
	return (stat)
end


# IMX_IN_RANGE -- Test number to see if it is in range.
# If the number is INDEFI then it is mapped to the maximum integer.

bool procedure imx_in_range (ranges, number)

int     ranges[ARB]             # Range array
int     number                  # Number to be tested against ranges

int     ip, first, last, step, num

begin
        if (IS_INDEFI (number))
            num = MAX_INT
        else
            num = number

        for (ip=1;  ranges[ip] != NULL;  ip=ip+3) {
            first = min (ranges[ip], ranges[ip+1])
            last = max (ranges[ip], ranges[ip+1])
            step = ranges[ip+2]
            if (num >= first && num <= last)
                if (mod (num - first, step) == 0)
                    return (true)
        }

        return (false)
end
