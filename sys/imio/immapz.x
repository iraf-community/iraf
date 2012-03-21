# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	<imhdr.h>
include	<imio.h>

# IMMAPZ -- Map an imagefile to an image structure.  This is the IMIO internal
# version of the immap procedure, called once the IKI has been initialized.

pointer procedure immapz (imspec, acmode, hdr_arg)

char	imspec[ARB]		# image specification
int	acmode			# image access mode
int	hdr_arg			# length of user fields, or header pointer

pointer	sp, imname, root, cluster, ksection, section, im
char	inname[SZ_PATHNAME]
int	min_lenuserarea, len_imhdr, cl_index, cl_size, i, val
int	btoi(), ctoi(), envfind(), fnroot(), strlen(), envgeti(), strncmp()
errchk	im_make_newcopy, im_init_newimage, malloc

begin
	call smark (sp)
	call salloc (imname, SZ_PATHNAME, TY_CHAR)
	call salloc (cluster, SZ_PATHNAME, TY_CHAR)
	call salloc (ksection, SZ_FNAME, TY_CHAR)
	call salloc (section, SZ_FNAME, TY_CHAR)
	call salloc (root, SZ_FNAME, TY_CHAR)

	# The user or system manager can specify the minimum user area size
	# as an environment variable, if the IRAF default is too small.

	if (envfind ("min_lenuserarea", Memc[section], SZ_FNAME) > 0) {
	    i = 1
	    if (ctoi (Memc[section], i, min_lenuserarea) <= 0)
		min_lenuserarea = MIN_LENUSERAREA
	} else
	    min_lenuserarea = MIN_LENUSERAREA


	# If we're given a URL to an image, cache the file.
	if (strncmp ("http://", imspec, 7) == 0)
	    call fcadd ("cache$", imspec, "", inname, SZ_PATHNAME)
	else if (strncmp ("file:///localhost", imspec, 17) == 0)
	    call strcpy (imspec[18], inname, SZ_PATHNAME)
	else if (strncmp ("file://localhost", imspec, 16) == 0)
	    call strcpy (imspec[17], inname, SZ_PATHNAME)
	else if (strncmp ("file://", imspec, 7) == 0)
	    call strcpy (imspec[7], inname, SZ_PATHNAME)
	else
	    call strcpy (imspec, inname, SZ_PATHNAME)


	# Parse the full image specification into its component parts.
	call imparse (inname, Memc[cluster],SZ_PATHNAME,
	    Memc[ksection],SZ_FNAME, Memc[section],SZ_FNAME, cl_index,cl_size)

	# Allocate buffer for image descriptor/image header.  Note the dual
	# use of the HDR_ARG argument.  In the case of a new copy image,
	# hdr_arg is a pointer to the image to be copied; otherwise is is the
	# length of the user area in CHARS (since the user area is a string
	# buffer).

	if (acmode == NEW_COPY) {
	    len_imhdr = max (LEN_IMHDR + min_lenuserarea / SZ_STRUCT,
		IM_HDRLEN(hdr_arg) + SZ_UAPAD / SZ_STRUCT)
	} else {
	    len_imhdr = LEN_IMHDR +
		max (min_lenuserarea, int(hdr_arg)) / SZ_STRUCT
	}

	call malloc (im, LEN_IMDES + len_imhdr, TY_STRUCT)
	call aclri (Memi[im], LEN_IMDES + min (len_imhdr, LEN_IMHDR + 1))
	IM_LENHDRMEM(im) = len_imhdr

	# Initialize the image descriptor structure.
	IM_ACMODE(im) = acmode
	IM_PFD(im)    = NULL
	IM_HDRLEN(im) = len_imhdr
	IM_UPDATE(im) = btoi (acmode != READ_ONLY)
	IM_UABLOCKED(im) = -1

	# Initialize options.
	IM_VNBUFS(im) = 1
	IM_VCOMPRESS(im) = DEF_COMPRESS
	IM_VADVICE(im) = DEF_ADVICE

	# Initialize the IMIO buffer size defaults.  The builtin defaults
	# are used unless a value is explicitly set in the environment;
	# an IMSET on the open descriptor will override either.

	IM_VBUFSIZE(im) = DEF_FIOBUFSIZE
	ifnoerr (val = envgeti (ENV_BUFSIZE))
	    IM_VBUFSIZE(im) = val / SZB_CHAR
	IM_VBUFFRAC(im) = DEF_FIOBUFFRAC
	ifnoerr (val = envgeti (ENV_BUFFRAC))
	    IM_VBUFFRAC(im) = val
	IM_VBUFMAX(im) = DEF_MAXFIOBUFSIZE
	ifnoerr (val = envgeti (ENV_BUFMAX))
	    IM_VBUFMAX(im) = val

	# Set fast i/o flag to yes initially to force IMOPSF and hence IMSETBUF
	# to be called when the first i/o operation occurs.

	IM_FAST(im) = YES
IM_FAST(im) = NO

	# Set the image name field, used by IMERR everywhere.
	call strcpy (inname, IM_NAME(im), SZ_IMNAME)

	# Initialize the mode dependent fields of the image header.
	if (acmode == NEW_COPY)
	    call im_make_newcopy (im, hdr_arg)
	else if (acmode == NEW_IMAGE)
	    call im_init_newimage (im, IM_HDRLEN(im))

	# Set the following in case it isn't set by the kernel.
	call strcpy ("imhdr", IM_MAGIC(im), SZ_IMMAGIC)

	# Physically open the image and read the header.  Note that IKI_OPEN
	# may realloc the image descriptor if additional space is required,
	# hence the pointer IM may be modified.

	iferr {
	    call iki_open (im, Memc[cluster], Memc[ksection],
		cl_index, cl_size, acmode, hdr_arg)
	} then {
	    call mfree (im, TY_STRUCT)
	    call erract (EA_ERROR)
	}

	# Format a full image name specification if we have a cl_index format
	# image.  IM_NAME is used mainly as an image identifier in error
	# messages, so truncate the string by omitting some of the leading
	# pathname information if the resultant string would be excessively
	# long.

	if (IM_CLSIZE(im) > 1) {
	    call sprintf (Memc[imname], SZ_PATHNAME, "%s[%d/%d]%s%s")
		call pargstr (Memc[cluster])
		call pargi (IM_CLINDEX(im))
		call pargi (IM_CLSIZE(im))
		call pargstr (Memc[ksection])
		call pargstr (Memc[section])

	    if (strlen (Memc[imname]) > SZ_IMNAME) {
		i = fnroot (Memc[cluster], Memc[root], SZ_FNAME)
		call sprintf (Memc[imname], SZ_PATHNAME, "%s[%d/%d]%s%s")
		    call pargstr (Memc[root])
		    call pargi (IM_CLINDEX(im))
		    call pargi (IM_CLSIZE(im))
		    call pargstr (Memc[ksection])
		    call pargstr (Memc[section])
	    }

	    call strcpy (Memc[imname], IM_NAME(im), SZ_IMNAME)
	}

	# Save those image header fields that get modified if an image section
	# is specified.

	IM_NPHYSDIM(im) = IM_NDIM(im)
	IM_SVMTIME(im)  = IM_MTIME(im)
	call amovl (IM_LEN(im,1), IM_SVLEN(im,1), IM_MAXDIM)

	# Process the image section if one was given, i.e., parse the section
	# string and set up a transformation to be applied to logical input
	# vectors.

	if (Memc[section] != EOS) {
	    if (acmode == NEW_COPY || acmode == NEW_IMAGE) {
		call iki_close (im)
		call mfree (im, TY_STRUCT)
		call imerr (IM_NAME(im), SYS_IMSECTNEWIM)
	    }
	    call imisec (im, Memc[section])
	    IM_SECTUSED(im) = YES
	} else {
	    # IM_VOFF is already zero, because of the CALLOC.
	    call amovkl (long(1), IM_VSTEP(im,1), IM_MAXDIM)
	    do i = 1, IM_MAXDIM
		IM_VMAP(im,i) = i
	}

	call sfree (sp)
	return (im)
end
