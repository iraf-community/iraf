# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<syserr.h>
include	<imhdr.h>
include	<imset.h>

define	OUTPUTS		"|none|list|file|"
define	NONE		1		# No output
define	LIST		2		# List output
define	FILE		3		# File output

define	SZ_RANGE	100		# Size of range list
define	SZ_LISTOUT	255		# Size of output list


# T_MSCEXTENSIONS -- Expand a template of FITS files into a list of image
# extensions on the standard output and record the number image extensions
# in a parameter.
#
# This differs from IMEXTENSIONS in that extension zero is not returned
# unless it is a simple image and, in that case, the extension is removed.
# Also a parameter is written indicating if the list contains image extensions.

procedure t_mscextensions()

pointer	input			# List of ME file names
int	output			# Output list (none|list|file)
pointer	index			# Range list of extension indexes
pointer	extname			# Patterns for extension names
pointer extver			# Range list of extension versions
int	lindex			# List index number?
int	lname			# List extension name?
int	lver			# List extension version?
pointer	ikparams		# Image kernel parameters

pointer	sp, image, listout
int	list, nimages, fd, imext
int	clgwrd(), btoi(), mscextensions(), stropen()
int	imtgetim(), imtlen()
bool	clgetb()
errchk	stropen, fprintf, strclose

begin
	call smark (sp)
	call salloc (input, SZ_LINE, TY_CHAR)
	call salloc (index, SZ_LINE, TY_CHAR)
	call salloc (extname, SZ_LINE, TY_CHAR)
	call salloc (extver, SZ_LINE, TY_CHAR)
	call salloc (ikparams, SZ_LINE, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Task parameters
	call clgstr ("input", Memc[input], SZ_LINE)
	output = clgwrd ("output", Memc[image], SZ_FNAME, OUTPUTS)
	call clgstr ("index", Memc[index], SZ_LINE)
	call clgstr ("extname", Memc[extname], SZ_LINE)
	call clgstr ("extver", Memc[extver], SZ_LINE)
	lindex = btoi (clgetb ("lindex"))
	lname = btoi (clgetb ("lname"))
	lver = btoi (clgetb ("lver"))
	call clgstr ("ikparams", Memc[ikparams], SZ_LINE)

	# Get the list.
	list = mscextensions (Memc[input], Memc[index], Memc[extname],
	    Memc[extver], lindex, lname, lver, Memc[ikparams], NO, imext)

	# Format the output and set the number of images.
	switch (output) {
	case LIST:
	    call salloc (listout, SZ_LISTOUT, TY_CHAR)
	    iferr {
		fd = stropen (Memc[listout], SZ_LISTOUT, WRITE_ONLY)
		nimages = 0
		while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		    nimages = nimages + 1
		    if (nimages == 1) {
			call fprintf (fd, "%s")
			    call pargstr (Memc[image])
		    } else {
			call fprintf (fd, ",%s")
			    call pargstr (Memc[image])
		    }
		}
		call strclose (fd)
		call printf ("%s\n")
		    call pargstr (Memc[listout])
	    } then {
		call imtclose (list)
		call sfree (sp)
		call error (1, "Output list format is too long")
	    }
	case FILE:
	    while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		call printf ("%s\n")
		    call pargstr (Memc[image])
	    }
	}
	call clputi ("nimages", imtlen (list))
	call clputb ("imext", (imext==YES))

	call imtclose (list)
	call sfree (sp)
end


# MSCEXTENSIONS -- Expand template of files into a list of image extensions.
#
# This differs from IMEXTENSIONS in that extension zero is not returned
# unless it is a simple image and, in that case, the extension is removed.

int procedure mscextensions (files, index, extname, extver, lindex, lname, lver,
	ikparams, err, imext)

char	files[ARB]		#I List of ME files
char	index[ARB]		#I Range list of extension indexes
char	extname[ARB]		#I Patterns for extension names
char	extver[ARB]		#I Range list of extension versions
int	lindex			#I List index number?
int	lname			#I List extension name?
int	lver			#I List extension version?
char	ikparams[ARB]		#I Image kernel parameters
int	err			#I Print errors?
int	imext			#O Image extensions?
int	list			#O Image list

int	i, j, nphu, nimages, fd
pointer	sp, temp, image, im, immap()
int	imextensions(), gstrmatch(), imtopen(), imtgetim(), open()
errchk	imextensions, open, immap, delete

begin
	call smark (sp)
	call salloc (temp, SZ_FNAME, TY_CHAR)
	call salloc (image, SZ_FNAME, TY_CHAR)

	# Get the list.
	list = imextensions (files, index, extname, extver, lindex, lname,
	    lver, ikparams, err)

	# Check and edit the list.
	nphu = 0
	nimages = 0
	call mktemp ("@tmp$iraf", Memc[temp], SZ_FNAME)
	fd = open (Memc[temp+1], NEW_FILE, TEXT_FILE)
	while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
	    if (gstrmatch (Memc[image], "\[0\]", i, j) > 0) {
		call strcpy (Memc[image+j], Memc[image+i-1], SZ_FNAME)
		ifnoerr (im = immap (Memc[image], READ_ONLY, 0)) {
		    call imunmap (im)
		    nphu = nphu + 1
		} else
		    next
	    } else if (gstrmatch (Memc[image], "\[1\]", i, j) > 0) {
		Memc[image+i] = '0'
		iferr {
		    im = immap (Memc[image], READ_ONLY, 0)
		    call imunmap (im)
		    Memc[image+i] = '1'
		} then {
		    nphu = nphu + 1
		    call strcpy (Memc[image+j], Memc[image+i-1], SZ_FNAME)
		}
	    }
	    nimages = nimages + 1
	    call fprintf (fd, "%s\n")
		call pargstr (Memc[image])
	}
	call close (fd)

	# Return new list and extension flag.
	imext = YES
	if (nphu == nimages)
	    imext = NO
	call imtclose (list)
	list = imtopen (Memc[temp])
	call delete (Memc[temp+1])
	call sfree (sp)
	return (list)
end
