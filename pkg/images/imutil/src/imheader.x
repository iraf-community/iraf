# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<imhdr.h>
include	<imset.h>
include	<imio.h>
include	<time.h>

define	SZ_DIMSTR	(IM_MAXDIM*4)
define	SZ_MMSTR	40
define	USER_AREA	Memc[($1+IMU-1)*SZ_STRUCT + 1]
define	LMARGIN		0


# IMHEADER -- Read contents of an image header and print on STDOUT.

procedure t_imheader()

int	list, nimages, errcode
bool	long_format, user_fields
pointer	sp, template, image, errmsg
int	imtopen(), imtgetim(), imtlen(), clgeti(), errget()
bool	clgetb()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (errmsg, SZ_LINE, TY_CHAR)
	call salloc (template, SZ_LINE, TY_CHAR)

	if (clgeti ("$nargs") == 0)
	    call clgstr ("imlist", Memc[template], SZ_LINE)
	else
	    call clgstr ("images", Memc[template], SZ_LINE)

	list = imtopen (Memc[template])
	long_format = clgetb ("longheader")
	user_fields = clgetb ("userfields")
	nimages = 0

	if (imtlen (list) <= 0)
	    call printf ("no images found\n")
	else {
	    while (imtgetim (list, Memc[image], SZ_FNAME) != EOF) {
		nimages = nimages + 1
		if (long_format && nimages > 1)
		    call putci (STDOUT, '\n')
		iferr {
		    call imphdr (STDOUT,Memc[image],long_format,user_fields)
		} then {
		    errcode = errget (Memc[errmsg], SZ_LINE)
		    call eprintf ("%s: %s\n")
			call pargstr (Memc[image])
			call pargstr (Memc[errmsg])
		}
		call flush (STDOUT)
	    }
	}

	call imtclose (list)
	call sfree (sp)
end


# IMPHDR -- Print the contents of an image header.

procedure imphdr (fd, image, long_format, user_fields)

int	fd
char	image[ARB]
bool	long_format
bool	user_fields

int	hi, i
bool	pixfile_ok
pointer	im, sp, ctime, mtime, ldim, pdim, title, lbuf, ip
int	gstrcpy(), stropen(), getline(), strlen(), stridxs(), imstati()
errchk	im_fmt_dimensions, immap, access, stropen, getline
define	done_ 91
pointer	immap()

begin
	# Allocate automatic buffers.
	call smark (sp)
	call salloc (ctime, SZ_TIME,   TY_CHAR)
	call salloc (mtime, SZ_TIME,   TY_CHAR)
	call salloc (ldim,  SZ_DIMSTR, TY_CHAR)
	call salloc (pdim,  SZ_DIMSTR, TY_CHAR)
	call salloc (title, SZ_LINE,   TY_CHAR)
	call salloc (lbuf,  SZ_LINE,   TY_CHAR)

	im = immap (image, READ_ONLY, 0)

	# Format subscript strings, date strings, mininum and maximum
	# pixel values.

	call im_fmt_dimensions (im, Memc[ldim], SZ_DIMSTR, IM_LEN(im,1))
	call im_fmt_dimensions (im, Memc[pdim], SZ_DIMSTR, IM_PHYSLEN(im,1))
	call cnvtime (IM_CTIME(im), Memc[ctime], SZ_TIME)
	call cnvtime (IM_MTIME(im), Memc[mtime], SZ_TIME)

	# Strip any trailing whitespace from the title string.
	ip = title + gstrcpy (IM_TITLE(im), Memc[title], SZ_LINE) - 1
	while (ip >= title && IS_WHITE(Memc[ip]) || Memc[ip] == '\n')
	    ip = ip - 1
	Memc[ip+1] = EOS

	# Begin printing image header.
	call fprintf (fd, "%s%s[%s]: %s\n")
	    call pargstr (IM_NAME(im))
	    call pargstr (Memc[ldim])
	    call pargtype (IM_PIXTYPE(im))
	    call pargstr (Memc[title])

	# All done if not long format.
	if (! long_format)
	    goto done_

	call fprintf (fd, "%*w%s bad pixels, min=%s, max=%s%s\n")
	    call pargi (LMARGIN)
	    if (IM_NBPIX(im) == 0)			# num bad pixels
		call pargstr ("No")
	    else
		call pargl (IM_NBPIX(im))

	    if (IM_LIMTIME(im) == 0) {			# min,max pixel values
		do i = 1, 2
		    call pargstr ("unknown")
		call pargstr ("")
	    } else {
		call pargr (IM_MIN(im))
		call pargr (IM_MAX(im))
		if (IM_LIMTIME(im) < IM_MTIME(im))
		    call pargstr (" (old)")
		else
		    call pargstr ("")
	    }

	call fprintf (fd,
	    "%*w%s storage mode, physdim %s, length of user area %d s.u.\n")
	    call pargi (LMARGIN)
	    call pargstr ("Line")
	    call pargstr (Memc[pdim])
	    call pargi (IM_HDRLEN(im) - LEN_IMHDR)

	call fprintf (fd, "%*wCreated %s, Last modified %s\n")
	    call pargi (LMARGIN)
	    call pargstr (Memc[ctime])			# times
	    call pargstr (Memc[mtime])

	pixfile_ok = (imstati (im, IM_PIXFD) > 0)
	if (!pixfile_ok) {
	    ifnoerr (call imopsf (im))
		pixfile_ok = (imstati (im, IM_PIXFD) > 0)
	    if (pixfile_ok)
		call close (imstati (im, IM_PIXFD))
	}
	if (pixfile_ok)
	    call strcpy ("[ok]", Memc[lbuf], SZ_LINE)
	else
	    call strcpy ("[NO PIXEL FILE]", Memc[lbuf], SZ_LINE)

	call fprintf (fd, "%*wPixel file \"%s\" %s\n")
	    call pargi (LMARGIN)
	    call pargstr (IM_PIXFILE(im))
	    call pargstr (Memc[lbuf])

	# Print the history records.
	if (strlen (IM_HISTORY(im)) > 1) {
	    hi = stropen (IM_HISTORY(im), ARB, READ_ONLY)
	    while (getline (hi, Memc[lbuf]) != EOF) {
		for (i=1;  i <= LMARGIN;  i=i+1)
		    call putci (fd, ' ')
		call putline (fd, Memc[lbuf])
		if (stridxs ("\n", Memc[lbuf]) == 0)
		    call putline (fd, "\n")
	    }
	    call close (hi)
	}

	if (user_fields)
	    call imh_print_user_area (fd, im)

done_
	call imunmap (im)
	call sfree (sp)
end


# IM_FMT_DIMENSIONS -- Format the image dimensions in the form of a subscript,
# i.e., "[nx,ny,nz,...]".

procedure im_fmt_dimensions (im, outstr, maxch, len_axes)

pointer	im
char	outstr[ARB]
int	maxch, i, fd, stropen()
long	len_axes[ARB]
errchk	stropen, fprintf, pargl

begin
	fd = stropen (outstr, maxch, NEW_FILE)

	if (IM_NDIM(im) == 0) {
	    call fprintf (fd, "[0")
	} else {
	    call fprintf (fd, "[%d")
	        call pargl (len_axes[1])
	}

	do i = 2, IM_NDIM(im) {
	    call fprintf (fd, ",%d")
		call pargl (len_axes[i])
	}

	call fprintf (fd, "]")
	call close (fd)
end


# PARGTYPE -- Convert an integer type code into a string, and output the
# string with PARGSTR to FMTIO.

procedure pargtype (dtype)

int	dtype

begin
	switch (dtype) {
	case TY_UBYTE:
	    call pargstr ("ubyte")
	case TY_BOOL:
	    call pargstr ("bool")
	case TY_CHAR:
	    call pargstr ("char")
	case TY_SHORT:
	    call pargstr ("short")
	case TY_USHORT:
	    call pargstr ("ushort")
	case TY_INT:
	    call pargstr ("int")
	case TY_LONG:
	    call pargstr ("long")
	case TY_REAL:
	    call pargstr ("real")
	case TY_DOUBLE:
	    call pargstr ("double")
	case TY_COMPLEX:
	    call pargstr ("complex")
	case TY_POINTER:
	    call pargstr ("pointer")
	case TY_STRUCT:
	    call pargstr ("struct")
	default:
	    call pargstr ("unknown datatype")
	}
end


# IMH_PRINT_USER_AREA -- Print the user area of the image, if nonzero length
# and it contains only ascii values.

procedure imh_print_user_area (out, im)

int	out			# output file
pointer	im			# image descriptor

pointer	sp, lbuf, ip
int	in, ncols, min_lenuserarea, i
int	stropen(), getline(), envgeti()
errchk	stropen, envgeti, getline, putci, putline

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	# Open user area in header.
	min_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU) * SZ_STRUCT - 1
	in = stropen (USER_AREA(im), min_lenuserarea, READ_ONLY)
	ncols = envgeti ("ttyncols") - LMARGIN

	# Copy header records to the output, stripping any trailing
	# whitespace and clipping at the right margin.

	while (getline (in, Memc[lbuf]) != EOF) {
	    for (ip=lbuf;  Memc[ip] != EOS && Memc[ip] != '\n';  ip=ip+1)
		;
	    while (ip > lbuf && Memc[ip-1] == ' ')
		ip = ip - 1
	    if (ip - lbuf > ncols)
		ip = lbuf + ncols 
	    Memc[ip] = '\n'
	    Memc[ip+1] = EOS
	    
	    for (i=1;  i <= LMARGIN;  i=i+1)
		call putci (out, ' ')
	    call putline (out, Memc[lbuf])
	}

	call close (in)
	call sfree (sp)
end
