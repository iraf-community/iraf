# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

define	OUTPUTS		"|none|list|file|"
define	NONE		1		# No output
define	LIST		2		# List output
define	FILE		3		# File output

define	SZ_LIST		10240		# Size of expanded list
define	SZ_LISTOUT	255		# Size of output list


# T_IMEXTENSIONS -- Expand a template of FITS files into a list of image
# extensions on the standard output and record the number image extensions
# in a parameter.

procedure t_imextensions()

pointer	p_input			# List of ME file names
int	output			# Output list (none|list|file)
pointer	index			# Range list of extension indexes
pointer	extname			# Pattern for extension names
pointer extver			# Range list of extension versions
int	lindex			# List index number?
int	lname			# List extension name?
int	lver			# List extension version?
pointer	ikparams		# Image kernel parameters

size_t	sz_val
pointer	sp, image, listout, list
int	nimages, fd
int	clgwrd(), btoi(), imtgetim(), imtlen(), stropen()
bool	clgetb()
pointer	xt_imextns()
errchk	stropen, fprintf, strclose

begin
	call smark (sp)
	sz_val = SZ_LINE
	call salloc (p_input, sz_val, TY_CHAR)
	call salloc (index, sz_val, TY_CHAR)
	call salloc (extname, sz_val, TY_CHAR)
	call salloc (extver, sz_val, TY_CHAR)
	call salloc (ikparams, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (image, sz_val, TY_CHAR)

	# Task parameters
	call clgstr ("input", Memc[p_input], SZ_LINE)
	output = clgwrd ("output", Memc[image], SZ_FNAME, OUTPUTS)
	call clgstr ("index", Memc[index], SZ_LINE)
	call clgstr ("extname", Memc[extname], SZ_LINE)
	call clgstr ("extver", Memc[extver], SZ_LINE)
	lindex = btoi (clgetb ("lindex"))
	lname = btoi (clgetb ("lname"))
	lver = btoi (clgetb ("lver"))
	call clgstr ("ikparams", Memc[ikparams], SZ_LINE)

	# Get the list.
	list = xt_imextns (Memc[p_input], Memc[index], Memc[extname],
	    Memc[extver], lindex, lname, lver, Memc[ikparams], YES)

	# Format the output and set the number of images.
	switch (output) {
	case LIST:
	    sz_val = SZ_LISTOUT
	    call salloc (listout, sz_val, TY_CHAR)
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

	call imtclose (list)
	call sfree (sp)
end
