include	<syserr.h>
include	<error.h>
include	<imhdr.h>
include	<imset.h>
include	<config.h>

# The following is for compiling under V2.11.
define	IM_BUFFRAC	IM_BUFSIZE
include	<imset.h>

define	VERBOSE		false

# These routines maintain an arbitrary number of indexed "open" images which
# must be READ_ONLY.  The calling program may use the returned pointer for
# header accesses but must call xt_opix before I/O.  Subsequent calls to
# xt_opix may invalidate the pointer.  The xt_imunmap call will free memory.

define	MAX_OPENIM	(LAST_FD-16)		# Maximum images kept open
define	MAX_OPENPIX	45			# Maximum pixel files kept open

define	XT_SZIMNAME	299			# Size of IMNAME string
define	XT_LEN		179			# Structure length
define	XT_IMNAME	Memc[P2C($1)]		# Image name
define	XT_ARG		Memi[$1+150]		# IMMAP header argument
define	XT_IM		Memi[$1+151]		# IMIO pointer
define	XT_HDR		Memi[$1+152]		# Copy of IMIO pointer
define	XT_CLOSEFD	Memi[$1+153]		# Close FD?
define	XT_FLAG		Memi[$1+154]		# Flag
define	XT_BUFSIZE	Memi[$1+155]		# Buffer size
define	XT_BUF		Memi[$1+156]		# Data buffer
define	XT_BTYPE	Memi[$1+157]		# Data buffer type
define	XT_VS		Memi[$1+157+$2]		# Start vector (10)
define	XT_VE		Memi[$1+167+$2]		# End vector (10)

# Options
define	XT_MAPUNMAP	1	# Map and unmap images.

# XT_IMMAP -- Map an image and save it as an indexed open image.
# The returned pointer may be used for header access but not I/O.
# The indexed image is closed by xt_imunmap.

pointer procedure xt_immap (imname, acmode, hdr_arg, index, retry)

char	imname[ARB]		#I Image name
int	acmode			#I Access mode
int	hdr_arg			#I Header argument
int	index			#I Save index
int	retry			#I Retry counter
pointer	im			#O Image pointer (returned)

int	i, envgeti()
pointer	xt, xt_opix()
errchk	xt_opix

int	first_time
data	first_time /YES/

include	"xtimmap.com"

begin
	if (acmode != READ_ONLY)
	    call error (1, "XT_IMMAP: Only READ_ONLY allowed")

	# Set maximum number of open images based on retry.
	if (retry > 0)
	    max_openim = min (1024, MAX_OPENIM) / retry
	else
	    max_openim = MAX_OPENIM

	# Initialize once per process.
	if (first_time == YES) {
	    iferr (option = envgeti ("imcombine_option"))
		option = 1
	    min_open = 1
	    nopen = 0
	    nopenpix = 0
	    nalloc = max_openim
	    call calloc (ims, nalloc, TY_POINTER)
	    first_time = NO
	}

	# Free image if needed.
	call xt_imunmap (NULL, index)

	# Allocate structure.
	if (index > nalloc) {
	    i = nalloc
	    nalloc = index + max_openim
	    call realloc (ims, nalloc, TY_STRUCT)
	    call amovki (NULL, Memi[ims+i], nalloc-i)
	}
	call calloc (xt, XT_LEN, TY_STRUCT)
	Memi[ims+index-1] = xt

	# Initialize.
	call strcpy (imname, XT_IMNAME(xt), XT_SZIMNAME)
	XT_ARG(xt) = hdr_arg
	XT_IM(xt) = NULL
	XT_HDR(xt) = NULL

	# Open image.
	last_flag = 0
	im = xt_opix (NULL, index, 0)

	# Make copy of IMIO pointer for header keyword access.
	call malloc (XT_HDR(xt), LEN_IMDES+IM_HDRLEN(im)+1, TY_STRUCT)
	call amovi (Memi[im], Memi[XT_HDR(xt)], LEN_IMDES)
	call amovi (IM_MAGIC(im), IM_MAGIC(XT_HDR(xt)), IM_HDRLEN(im)+1)

	return (XT_HDR(xt))
end


# XT_OPIX -- Open the image for I/O.
# If the image has not been mapped return the default pointer.

pointer	procedure xt_opix (imdef, index, flag)

int	index			#I index
pointer	imdef			#I Default pointer
int	flag			#I Flag

int	i, open(), imstati()
pointer	im, xt, xt1, immap()
errchk	open, immap, imunmap

include	"xtimmap.com"

begin
	# Get index pointer.
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	# Use default pointer if index has not been mapped.
	if (xt == NULL)
	    return (imdef)

	# Close images not accessed during previous line.
	# In normal usage this should only occur once per line over all
	# indexed images.
	if (flag != last_flag) {
	    do i = 1, nalloc {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL || XT_FLAG(xt1) == last_flag)
		    next
		if (VERBOSE) {
		    call eprintf ("%d: xt_opix imunmap %s\n")
			call pargi (i)
			call pargstr (XT_IMNAME(xt1))
		}
		call imunmap (XT_IM(xt1))
		call mfree (XT_BUF(xt1), XT_BTYPE(xt1))
		nopen = nopen - 1
		if (XT_CLOSEFD(xt1) == NO)
		    nopenpix = nopenpix - 1
	    }

	    # Optimize the file I/O.
	    do i = nalloc, 1, -1 {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL)
		    next
		min_open = i
		if (nopenpix < MAX_OPENPIX) {
		    if (XT_CLOSEFD(xt1) == NO)
			next
		    XT_CLOSEFD(xt1) = NO
		    call imseti (im, IM_CLOSEFD, NO)
		    nopenpix = nopenpix + 1
		}
	    }
	    last_flag = flag
	}

	# Return pointer for already opened images.
	im = XT_IM(xt)
	if (im != NULL) {
	    XT_FLAG(xt) = flag
	    return (im)
	}

	# Handle more images than the maximum that can be open at one time.
	if (nopen >= max_openim) {
	    if (option == XT_MAPUNMAP || flag == 0) {
		do i = min_open, nalloc {
		    xt1 = Memi[ims+i-1]
		    if (xt1 == NULL)
			next
		    im = XT_IM(xt1)
		    if (im == NULL)
			next
		    if (VERBOSE) {
			call eprintf ("%d: imunmap %s\n")
			    call pargi (i)
			    call pargstr (XT_IMNAME(xt1))
		    }
		    call imunmap (XT_IM(xt1))
		    nopen = nopen - 1
		    if (XT_CLOSEFD(xt1) == NO)
			nopenpix = nopenpix - 1
		    min_open = i + 1
		    break
		}
		if (index <= min_open)
		    min_open = index
		else {
		    do i = min_open, nalloc {
			xt1 = Memi[ims+i-1]
			if (xt1 == NULL)
			    next
			im = XT_IM(xt1)
			if (im == NULL)
			    next
			min_open = i
			break
		    }
		}
	    } else {
		# Check here because we can't catch error in immap.
		i = open ("dev$null", READ_ONLY, BINARY_FILE)
		call close (i)
		if (i == LAST_FD - 1)
		    call error (SYS_FTOOMANYFILES, "Too many open files")
	    }
	}
	
	# Open image.
	if (VERBOSE) {
	    call eprintf ("%d: xt_opix immap %s\n")
	        call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	im = immap (XT_IMNAME(xt), READ_ONLY, XT_ARG(xt))
	XT_IM(xt) = im
	if (!IS_INDEFI(XT_BUFSIZE(xt)))
	    call imseti (im, IM_BUFSIZE, XT_BUFSIZE(xt))
	else
	    XT_BUFSIZE(xt) = imstati (im, IM_BUFSIZE)
	nopen = nopen + 1
	XT_CLOSEFD(xt) = YES
	if (nopenpix < MAX_OPENPIX) {
	    XT_CLOSEFD(xt) = NO
	    nopenpix = nopenpix + 1
	}
	if (XT_CLOSEFD(xt) == YES)
	    call imseti (im, IM_CLOSEFD, YES)
	XT_FLAG(xt) = flag

	return (im)
end


# XT_CPIX -- Close image.

procedure xt_cpix (index)

int	index			#I index

pointer	xt
errchk	imunmap

include	"xtimmap.com"

begin
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	if (xt == NULL)
	    return

	if (XT_IM(xt) != NULL) {
	    if (VERBOSE) {
		call eprintf ("%d: xt_cpix imunmap %s\n")
		    call pargi (index)
		    call pargstr (XT_IMNAME(xt))
	    }
	    call imunmap (XT_IM(xt))
	    nopen = nopen - 1
	    if (XT_CLOSEFD(xt) == NO)
		nopenpix = nopenpix - 1
	}
	call mfree (XT_BUF(xt), XT_BTYPE(xt))
end


# XT_IMSETI -- Set IMIO value.

procedure xt_imseti (index, param, value)

int	index			#I index
int	param			#I IMSET parameter
int	value			#I Value

pointer	xt
bool	streq()

include	"xtimmap.com"

begin
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	if (xt == NULL) {
	    if (streq (param, "option"))
		option = value
	} else {
	    if (streq (param, "bufsize")) {
		XT_BUFSIZE(xt) = value
		if (XT_IM(xt) != NULL) {
		    call imseti (XT_IM(xt), IM_BUFFRAC, 0)
		    call imseti (XT_IM(xt), IM_BUFSIZE, value)
		}
	    }
	}
end


# XT_IMUNMAP -- Unmap indexed open image.
# The header pointer is set to NULL to indicate the image has been closed.

procedure xt_imunmap (im, index)

int	im			#U IMIO header pointer
int	index			#I index

pointer	xt
errchk	imunmap

include	"xtimmap.com"

begin
	# Check for an indexed image.  If it is not unmap the pointer
	# as a regular IMIO pointer.

	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]
	if (xt == NULL) {
	    if (im != NULL)
		call imunmap (im)
	    return
	}

	# Close indexed image.
	if (XT_IM(xt) != NULL) {
	    if (VERBOSE) {
		call eprintf ("%d: xt_imunmap imunmap %s\n")
		    call pargi (index)
		    call pargstr (XT_IMNAME(xt))
	    }
	    iferr (call imunmap (XT_IM(xt))) {
		XT_IM(xt) = NULL
		call erract (EA_WARN)
	    }
	    nopen = nopen - 1
	    if (XT_CLOSEFD(xt) == NO)
		nopenpix = nopenpix - 1
	    if (index == min_open)
		min_open = 1
	}

	# Free any buffered memory.
	call mfree (XT_BUF(xt), XT_BTYPE(xt))

	# Free header pointer.  Note that if the supplied pointer is not
	# header pointer then it is not set to NULL.
	if (XT_HDR(xt) == im)
	    im = NULL
	call mfree (XT_HDR(xt), TY_STRUCT)

	# Free save structure.
	call mfree (Memi[ims+index-1], TY_STRUCT)
	Memi[ims+index-1] = NULL
end


# XT_MINHDR -- Minimize header assuming keywords will not be accessed.

procedure xt_minhdr (index)

int	index			#I index

pointer	xt
errchk	realloc

include	"xtimmap.com"

begin
	# Check for an indexed image.  If it is not unmap the pointer
	# as a regular IMIO pointer.

	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]
	if (xt == NULL)
	    return

	# Minimize header pointer.
	if (VERBOSE) {
	    call eprintf ("%d: xt_minhdr %s\n")
		call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	call realloc (XT_HDR(xt), IMU+1, TY_STRUCT)
	if (XT_IM(xt) != NULL)
	    call realloc (XT_IM(xt), IMU+1, TY_STRUCT)
end


# XT_REINDEX -- Reindex open images.
# This is used when some images are closed by xt_imunmap.  It is up to
# the calling program to reindex the header pointers and to subsequently
# use the new index values.

procedure xt_reindex ()

int	old, new

include	"xtimmap.com"

begin
	new = 0
	do old = 0, nalloc-1 {
	    if (Memi[ims+old] == NULL)
		next
	    Memi[ims+new] = Memi[ims+old]
	    new = new + 1
	}
	do old = new, nalloc-1
	    Memi[ims+old] = NULL
end



# XT_IMGNL -- Return the next line for the indexed image.
# Possibly unmap another image if too many files are open.
# Buffer data when an image is unmmaped to minimize the mapping of images.
# If the requested index has not been mapped use the default pointer.

int procedure xt_imgnls (imdef, index, buf, v, flag)

pointer	imdef			#I Default pointer
int	index			#I index
pointer	buf			#O Data buffer
long	v[ARB]			#I Line vector
int	flag			#I Flag (=output line)

int	i, j, nc, nl, open(), imgnls(), sizeof(), imloop()
pointer	im, xt, xt1, ptr, immap(), imggss()
errchk	open, immap, imgnls, imggss, imunmap

long	unit_v[IM_MAXDIM]
data	unit_v /IM_MAXDIM * 1/

include	"xtimmap.com"

begin
	# Get index pointer.
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	# Use default pointer if index has not been mapped.
	if (xt == NULL)
	    return (imgnls (imdef, buf, v))

	# Close images not accessed during previous line.
	# In normal usage this should only occur once per line over all
	# indexed images.
	if (flag != last_flag) {
	    do i = 1, nalloc {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL || XT_FLAG(xt1) == last_flag)
		    next
		if (VERBOSE) {
		    call eprintf ("%d: xt_imgnl imunmap %s\n")
			call pargi (i)
			call pargstr (XT_IMNAME(xt1))
		}
		call imunmap (XT_IM(xt1))
		call mfree (XT_BUF(xt1), XT_BTYPE(xt1))
		nopen = nopen - 1
		if (XT_CLOSEFD(xt1) == NO)
		    nopenpix = nopenpix - 1
	    }

	    # Optimize the file I/O.
	    do i = nalloc, 1, -1 {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL)
		    next
		min_open = i
		if (nopenpix < MAX_OPENPIX) {
		    if (XT_CLOSEFD(xt1) == NO)
			next
		    XT_CLOSEFD(xt1) = NO
		    call imseti (im, IM_CLOSEFD, NO)
		    nopenpix = nopenpix + 1
		}
	    }
	    last_flag = flag
	}

	# Use IMIO for already opened images.
	im = XT_IM(xt)
	if (im != NULL) {
	    XT_FLAG(xt) = flag
	    return (imgnls (im, buf, v))
	}

	# If the image is not currently mapped use the stored header.
	im = XT_HDR(xt)

	# Check for EOF.
	i = IM_NDIM(im)
	if (v[i] > IM_LEN(im,i))
	    return (EOF)

	# Check for buffered data.
	if (XT_BUF(xt) != NULL) {
	    if (v[2] >= XT_VS(xt,2) && v[2] <= XT_VE(xt,2)) {
		if (XT_BTYPE(xt) != TY_SHORT)
		    call error (1, "Cannot mix data types")
		nc = IM_LEN(im,1)
		buf = XT_BUF(xt) + (v[2]-XT_VS(xt,2)) * IM_LEN(im,1)
		XT_FLAG(xt) = flag
		if (i == 1)
		    v[1] = nc + 1
		else
		    j = imloop (v, unit_v, IM_LEN(im,1), unit_v, i)
		return (nc)
	    }
	}

	# Handle more images than the maximum that can be open at one time.
	if (nopen >= max_openim) {
	    if (option == XT_MAPUNMAP || v[2] == 0) {
		do i = min_open, nalloc {
		    xt1 = Memi[ims+i-1]
		    if (xt1 == NULL)
			next
		    im = XT_IM(xt1)
		    if (im == NULL)
			next

		    # Buffer some number of lines.
		    nl = XT_BUFSIZE(xt1) / sizeof (TY_SHORT) / IM_LEN(im,1)
		    if (nl > 1) {
			nc = IM_LEN(im,1)
			call amovl (v, XT_VS(xt1,1), IM_MAXDIM)
			call amovl (v, XT_VE(xt1,1), IM_MAXDIM)
			XT_VS(xt1,1) = 1
			XT_VE(xt1,1) = nc
			XT_VE(xt1,2) = min (XT_VS(xt1,2)+(nl-1), IM_LEN(im,2))
			nl = XT_VE(xt1,2) - XT_VS(xt1,2) + 1
			XT_BTYPE(xt1) = TY_SHORT
			call malloc (XT_BUF(xt1), nl*nc, XT_BTYPE(xt1))
			ptr = imggss (im, XT_VS(xt1,1), XT_VE(xt1,1),
			   IM_NDIM(im))
			call amovs (Mems[ptr], Mems[XT_BUF(xt1)], nl*nc)
		    }

		    if (VERBOSE) {
			call eprintf ("%d: xt_imgnl imunmap %s\n")
			    call pargi (i)
			    call pargstr (XT_IMNAME(xt1))
		    }
		    call imunmap (XT_IM(xt1))
		    nopen = nopen - 1
		    if (XT_CLOSEFD(xt1) == NO)
			nopenpix = nopenpix - 1
		    min_open = i + 1
		    break
		}
		if (index <= min_open)
		    min_open = index
		else {
		    do i = min_open, nalloc {
			xt1 = Memi[ims+i-1]
			if (xt1 == NULL)
			    next
			if (XT_IM(xt1) == NULL)
			    next
			min_open = i
			break
		    }
		}
	    } else {
		# Check here because we can't catch error in immap.
		i = open ("dev$null", READ_ONLY, BINARY_FILE)
		call close (i)
		if (i == LAST_FD - 1)
		    call error (SYS_FTOOMANYFILES, "Too many open files")
	    }
	}
	
	# Open image.
	if (VERBOSE) {
	    call eprintf ("%d: xt_imgnl immap %s\n")
	        call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	im = immap (XT_IMNAME(xt), READ_ONLY, XT_ARG(xt))
	XT_IM(xt) = im
	call imseti (im, IM_BUFSIZE, XT_BUFSIZE(xt))
	call mfree (XT_BUF(xt), XT_BTYPE(xt))
	nopen = nopen + 1
	XT_CLOSEFD(xt) = YES
	if (nopenpix < MAX_OPENPIX) {
	    XT_CLOSEFD(xt) = NO
	    nopenpix = nopenpix + 1
	}
	if (XT_CLOSEFD(xt) == YES)
	    call imseti (im, IM_CLOSEFD, YES)
	XT_FLAG(xt) = flag

	return (imgnls (im, buf, v))
end

# XT_IMGNL -- Return the next line for the indexed image.
# Possibly unmap another image if too many files are open.
# Buffer data when an image is unmmaped to minimize the mapping of images.
# If the requested index has not been mapped use the default pointer.

int procedure xt_imgnli (imdef, index, buf, v, flag)

pointer	imdef			#I Default pointer
int	index			#I index
pointer	buf			#O Data buffer
long	v[ARB]			#I Line vector
int	flag			#I Flag (=output line)

int	i, j, nc, nl, open(), imgnli(), sizeof(), imloop()
pointer	im, xt, xt1, ptr, immap(), imggsi()
errchk	open, immap, imgnli, imggsi, imunmap

long	unit_v[IM_MAXDIM]
data	unit_v /IM_MAXDIM * 1/

include	"xtimmap.com"

begin
	# Get index pointer.
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	# Use default pointer if index has not been mapped.
	if (xt == NULL)
	    return (imgnli (imdef, buf, v))

	# Close images not accessed during previous line.
	# In normal usage this should only occur once per line over all
	# indexed images.
	if (flag != last_flag) {
	    do i = 1, nalloc {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL || XT_FLAG(xt1) == last_flag)
		    next
		if (VERBOSE) {
		    call eprintf ("%d: xt_imgnl imunmap %s\n")
			call pargi (i)
			call pargstr (XT_IMNAME(xt1))
		}
		call imunmap (XT_IM(xt1))
		call mfree (XT_BUF(xt1), XT_BTYPE(xt1))
		nopen = nopen - 1
		if (XT_CLOSEFD(xt1) == NO)
		    nopenpix = nopenpix - 1
	    }

	    # Optimize the file I/O.
	    do i = nalloc, 1, -1 {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL)
		    next
		min_open = i
		if (nopenpix < MAX_OPENPIX) {
		    if (XT_CLOSEFD(xt1) == NO)
			next
		    XT_CLOSEFD(xt1) = NO
		    call imseti (im, IM_CLOSEFD, NO)
		    nopenpix = nopenpix + 1
		}
	    }
	    last_flag = flag
	}

	# Use IMIO for already opened images.
	im = XT_IM(xt)
	if (im != NULL) {
	    XT_FLAG(xt) = flag
	    return (imgnli (im, buf, v))
	}

	# If the image is not currently mapped use the stored header.
	im = XT_HDR(xt)

	# Check for EOF.
	i = IM_NDIM(im)
	if (v[i] > IM_LEN(im,i))
	    return (EOF)

	# Check for buffered data.
	if (XT_BUF(xt) != NULL) {
	    if (v[2] >= XT_VS(xt,2) && v[2] <= XT_VE(xt,2)) {
		if (XT_BTYPE(xt) != TY_INT)
		    call error (1, "Cannot mix data types")
		nc = IM_LEN(im,1)
		buf = XT_BUF(xt) + (v[2]-XT_VS(xt,2)) * IM_LEN(im,1)
		XT_FLAG(xt) = flag
		if (i == 1)
		    v[1] = nc + 1
		else
		    j = imloop (v, unit_v, IM_LEN(im,1), unit_v, i)
		return (nc)
	    }
	}

	# Handle more images than the maximum that can be open at one time.
	if (nopen >= max_openim) {
	    if (option == XT_MAPUNMAP || v[2] == 0) {
		do i = min_open, nalloc {
		    xt1 = Memi[ims+i-1]
		    if (xt1 == NULL)
			next
		    im = XT_IM(xt1)
		    if (im == NULL)
			next

		    # Buffer some number of lines.
		    nl = XT_BUFSIZE(xt1) / sizeof (TY_INT) / IM_LEN(im,1)
		    if (nl > 1) {
			nc = IM_LEN(im,1)
			call amovl (v, XT_VS(xt1,1), IM_MAXDIM)
			call amovl (v, XT_VE(xt1,1), IM_MAXDIM)
			XT_VS(xt1,1) = 1
			XT_VE(xt1,1) = nc
			XT_VE(xt1,2) = min (XT_VS(xt1,2)+(nl-1), IM_LEN(im,2))
			nl = XT_VE(xt1,2) - XT_VS(xt1,2) + 1
			XT_BTYPE(xt1) = TY_INT
			call malloc (XT_BUF(xt1), nl*nc, XT_BTYPE(xt1))
			ptr = imggsi (im, XT_VS(xt1,1), XT_VE(xt1,1),
			   IM_NDIM(im))
			call amovi (Memi[ptr], Memi[XT_BUF(xt1)], nl*nc)
		    }

		    if (VERBOSE) {
			call eprintf ("%d: xt_imgnl imunmap %s\n")
			    call pargi (i)
			    call pargstr (XT_IMNAME(xt1))
		    }
		    call imunmap (XT_IM(xt1))
		    nopen = nopen - 1
		    if (XT_CLOSEFD(xt1) == NO)
			nopenpix = nopenpix - 1
		    min_open = i + 1
		    break
		}
		if (index <= min_open)
		    min_open = index
		else {
		    do i = min_open, nalloc {
			xt1 = Memi[ims+i-1]
			if (xt1 == NULL)
			    next
			if (XT_IM(xt1) == NULL)
			    next
			min_open = i
			break
		    }
		}
	    } else {
		# Check here because we can't catch error in immap.
		i = open ("dev$null", READ_ONLY, BINARY_FILE)
		call close (i)
		if (i == LAST_FD - 1)
		    call error (SYS_FTOOMANYFILES, "Too many open files")
	    }
	}
	
	# Open image.
	if (VERBOSE) {
	    call eprintf ("%d: xt_imgnl immap %s\n")
	        call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	im = immap (XT_IMNAME(xt), READ_ONLY, XT_ARG(xt))
	XT_IM(xt) = im
	call imseti (im, IM_BUFSIZE, XT_BUFSIZE(xt))
	call mfree (XT_BUF(xt), XT_BTYPE(xt))
	nopen = nopen + 1
	XT_CLOSEFD(xt) = YES
	if (nopenpix < MAX_OPENPIX) {
	    XT_CLOSEFD(xt) = NO
	    nopenpix = nopenpix + 1
	}
	if (XT_CLOSEFD(xt) == YES)
	    call imseti (im, IM_CLOSEFD, YES)
	XT_FLAG(xt) = flag

	return (imgnli (im, buf, v))
end

# XT_IMGNL -- Return the next line for the indexed image.
# Possibly unmap another image if too many files are open.
# Buffer data when an image is unmmaped to minimize the mapping of images.
# If the requested index has not been mapped use the default pointer.

int procedure xt_imgnlr (imdef, index, buf, v, flag)

pointer	imdef			#I Default pointer
int	index			#I index
pointer	buf			#O Data buffer
long	v[ARB]			#I Line vector
int	flag			#I Flag (=output line)

int	i, j, nc, nl, open(), imgnlr(), sizeof(), imloop()
pointer	im, xt, xt1, ptr, immap(), imggsr()
errchk	open, immap, imgnlr, imggsr, imunmap

long	unit_v[IM_MAXDIM]
data	unit_v /IM_MAXDIM * 1/

include	"xtimmap.com"

begin
	# Get index pointer.
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	# Use default pointer if index has not been mapped.
	if (xt == NULL)
	    return (imgnlr (imdef, buf, v))

	# Close images not accessed during previous line.
	# In normal usage this should only occur once per line over all
	# indexed images.
	if (flag != last_flag) {
	    do i = 1, nalloc {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL || XT_FLAG(xt1) == last_flag)
		    next
		if (VERBOSE) {
		    call eprintf ("%d: xt_imgnl imunmap %s\n")
			call pargi (i)
			call pargstr (XT_IMNAME(xt1))
		}
		call imunmap (XT_IM(xt1))
		call mfree (XT_BUF(xt1), XT_BTYPE(xt1))
		nopen = nopen - 1
		if (XT_CLOSEFD(xt1) == NO)
		    nopenpix = nopenpix - 1
	    }

	    # Optimize the file I/O.
	    do i = nalloc, 1, -1 {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL)
		    next
		min_open = i
		if (nopenpix < MAX_OPENPIX) {
		    if (XT_CLOSEFD(xt1) == NO)
			next
		    XT_CLOSEFD(xt1) = NO
		    call imseti (im, IM_CLOSEFD, NO)
		    nopenpix = nopenpix + 1
		}
	    }
	    last_flag = flag
	}

	# Use IMIO for already opened images.
	im = XT_IM(xt)
	if (im != NULL) {
	    XT_FLAG(xt) = flag
	    return (imgnlr (im, buf, v))
	}

	# If the image is not currently mapped use the stored header.
	im = XT_HDR(xt)

	# Check for EOF.
	i = IM_NDIM(im)
	if (v[i] > IM_LEN(im,i))
	    return (EOF)

	# Check for buffered data.
	if (XT_BUF(xt) != NULL) {
	    if (v[2] >= XT_VS(xt,2) && v[2] <= XT_VE(xt,2)) {
		if (XT_BTYPE(xt) != TY_REAL)
		    call error (1, "Cannot mix data types")
		nc = IM_LEN(im,1)
		buf = XT_BUF(xt) + (v[2]-XT_VS(xt,2)) * IM_LEN(im,1)
		XT_FLAG(xt) = flag
		if (i == 1)
		    v[1] = nc + 1
		else
		    j = imloop (v, unit_v, IM_LEN(im,1), unit_v, i)
		return (nc)
	    }
	}

	# Handle more images than the maximum that can be open at one time.
	if (nopen >= max_openim) {
	    if (option == XT_MAPUNMAP || v[2] == 0) {
		do i = min_open, nalloc {
		    xt1 = Memi[ims+i-1]
		    if (xt1 == NULL)
			next
		    im = XT_IM(xt1)
		    if (im == NULL)
			next

		    # Buffer some number of lines.
		    nl = XT_BUFSIZE(xt1) / sizeof (TY_REAL) / IM_LEN(im,1)
		    if (nl > 1) {
			nc = IM_LEN(im,1)
			call amovl (v, XT_VS(xt1,1), IM_MAXDIM)
			call amovl (v, XT_VE(xt1,1), IM_MAXDIM)
			XT_VS(xt1,1) = 1
			XT_VE(xt1,1) = nc
			XT_VE(xt1,2) = min (XT_VS(xt1,2)+(nl-1), IM_LEN(im,2))
			nl = XT_VE(xt1,2) - XT_VS(xt1,2) + 1
			XT_BTYPE(xt1) = TY_REAL
			call malloc (XT_BUF(xt1), nl*nc, XT_BTYPE(xt1))
			ptr = imggsr (im, XT_VS(xt1,1), XT_VE(xt1,1),
			   IM_NDIM(im))
			call amovr (Memr[ptr], Memr[XT_BUF(xt1)], nl*nc)
		    }

		    if (VERBOSE) {
			call eprintf ("%d: xt_imgnl imunmap %s\n")
			    call pargi (i)
			    call pargstr (XT_IMNAME(xt1))
		    }
		    call imunmap (XT_IM(xt1))
		    nopen = nopen - 1
		    if (XT_CLOSEFD(xt1) == NO)
			nopenpix = nopenpix - 1
		    min_open = i + 1
		    break
		}
		if (index <= min_open)
		    min_open = index
		else {
		    do i = min_open, nalloc {
			xt1 = Memi[ims+i-1]
			if (xt1 == NULL)
			    next
			if (XT_IM(xt1) == NULL)
			    next
			min_open = i
			break
		    }
		}
	    } else {
		# Check here because we can't catch error in immap.
		i = open ("dev$null", READ_ONLY, BINARY_FILE)
		call close (i)
		if (i == LAST_FD - 1)
		    call error (SYS_FTOOMANYFILES, "Too many open files")
	    }
	}
	
	# Open image.
	if (VERBOSE) {
	    call eprintf ("%d: xt_imgnl immap %s\n")
	        call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	im = immap (XT_IMNAME(xt), READ_ONLY, XT_ARG(xt))
	XT_IM(xt) = im
	call imseti (im, IM_BUFSIZE, XT_BUFSIZE(xt))
	call mfree (XT_BUF(xt), XT_BTYPE(xt))
	nopen = nopen + 1
	XT_CLOSEFD(xt) = YES
	if (nopenpix < MAX_OPENPIX) {
	    XT_CLOSEFD(xt) = NO
	    nopenpix = nopenpix + 1
	}
	if (XT_CLOSEFD(xt) == YES)
	    call imseti (im, IM_CLOSEFD, YES)
	XT_FLAG(xt) = flag

	return (imgnlr (im, buf, v))
end

# XT_IMGNL -- Return the next line for the indexed image.
# Possibly unmap another image if too many files are open.
# Buffer data when an image is unmmaped to minimize the mapping of images.
# If the requested index has not been mapped use the default pointer.

int procedure xt_imgnld (imdef, index, buf, v, flag)

pointer	imdef			#I Default pointer
int	index			#I index
pointer	buf			#O Data buffer
long	v[ARB]			#I Line vector
int	flag			#I Flag (=output line)

int	i, j, nc, nl, open(), imgnld(), sizeof(), imloop()
pointer	im, xt, xt1, ptr, immap(), imggsd()
errchk	open, immap, imgnld, imggsd, imunmap

long	unit_v[IM_MAXDIM]
data	unit_v /IM_MAXDIM * 1/

include	"xtimmap.com"

begin
	# Get index pointer.
	xt = NULL
	if (index <= nalloc && index > 0)
	    xt = Memi[ims+index-1]

	# Use default pointer if index has not been mapped.
	if (xt == NULL)
	    return (imgnld (imdef, buf, v))

	# Close images not accessed during previous line.
	# In normal usage this should only occur once per line over all
	# indexed images.
	if (flag != last_flag) {
	    do i = 1, nalloc {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL || XT_FLAG(xt1) == last_flag)
		    next
		if (VERBOSE) {
		    call eprintf ("%d: xt_imgnl imunmap %s\n")
			call pargi (i)
			call pargstr (XT_IMNAME(xt1))
		}
		call imunmap (XT_IM(xt1))
		call mfree (XT_BUF(xt1), XT_BTYPE(xt1))
		nopen = nopen - 1
		if (XT_CLOSEFD(xt1) == NO)
		    nopenpix = nopenpix - 1
	    }

	    # Optimize the file I/O.
	    do i = nalloc, 1, -1 {
		xt1 = Memi[ims+i-1]
		if (xt1 == NULL)
		    next
		im = XT_IM(xt1)
		if (im == NULL)
		    next
		min_open = i
		if (nopenpix < MAX_OPENPIX) {
		    if (XT_CLOSEFD(xt1) == NO)
			next
		    XT_CLOSEFD(xt1) = NO
		    call imseti (im, IM_CLOSEFD, NO)
		    nopenpix = nopenpix + 1
		}
	    }
	    last_flag = flag
	}

	# Use IMIO for already opened images.
	im = XT_IM(xt)
	if (im != NULL) {
	    XT_FLAG(xt) = flag
	    return (imgnld (im, buf, v))
	}

	# If the image is not currently mapped use the stored header.
	im = XT_HDR(xt)

	# Check for EOF.
	i = IM_NDIM(im)
	if (v[i] > IM_LEN(im,i))
	    return (EOF)

	# Check for buffered data.
	if (XT_BUF(xt) != NULL) {
	    if (v[2] >= XT_VS(xt,2) && v[2] <= XT_VE(xt,2)) {
		if (XT_BTYPE(xt) != TY_DOUBLE)
		    call error (1, "Cannot mix data types")
		nc = IM_LEN(im,1)
		buf = XT_BUF(xt) + (v[2]-XT_VS(xt,2)) * IM_LEN(im,1)
		XT_FLAG(xt) = flag
		if (i == 1)
		    v[1] = nc + 1
		else
		    j = imloop (v, unit_v, IM_LEN(im,1), unit_v, i)
		return (nc)
	    }
	}

	# Handle more images than the maximum that can be open at one time.
	if (nopen >= max_openim) {
	    if (option == XT_MAPUNMAP || v[2] == 0) {
		do i = min_open, nalloc {
		    xt1 = Memi[ims+i-1]
		    if (xt1 == NULL)
			next
		    im = XT_IM(xt1)
		    if (im == NULL)
			next

		    # Buffer some number of lines.
		    nl = XT_BUFSIZE(xt1) / sizeof (TY_DOUBLE) / IM_LEN(im,1)
		    if (nl > 1) {
			nc = IM_LEN(im,1)
			call amovl (v, XT_VS(xt1,1), IM_MAXDIM)
			call amovl (v, XT_VE(xt1,1), IM_MAXDIM)
			XT_VS(xt1,1) = 1
			XT_VE(xt1,1) = nc
			XT_VE(xt1,2) = min (XT_VS(xt1,2)+(nl-1), IM_LEN(im,2))
			nl = XT_VE(xt1,2) - XT_VS(xt1,2) + 1
			XT_BTYPE(xt1) = TY_DOUBLE
			call malloc (XT_BUF(xt1), nl*nc, XT_BTYPE(xt1))
			ptr = imggsd (im, XT_VS(xt1,1), XT_VE(xt1,1),
			   IM_NDIM(im))
			call amovd (Memd[ptr], Memd[XT_BUF(xt1)], nl*nc)
		    }

		    if (VERBOSE) {
			call eprintf ("%d: xt_imgnl imunmap %s\n")
			    call pargi (i)
			    call pargstr (XT_IMNAME(xt1))
		    }
		    call imunmap (XT_IM(xt1))
		    nopen = nopen - 1
		    if (XT_CLOSEFD(xt1) == NO)
			nopenpix = nopenpix - 1
		    min_open = i + 1
		    break
		}
		if (index <= min_open)
		    min_open = index
		else {
		    do i = min_open, nalloc {
			xt1 = Memi[ims+i-1]
			if (xt1 == NULL)
			    next
			if (XT_IM(xt1) == NULL)
			    next
			min_open = i
			break
		    }
		}
	    } else {
		# Check here because we can't catch error in immap.
		i = open ("dev$null", READ_ONLY, BINARY_FILE)
		call close (i)
		if (i == LAST_FD - 1)
		    call error (SYS_FTOOMANYFILES, "Too many open files")
	    }
	}
	
	# Open image.
	if (VERBOSE) {
	    call eprintf ("%d: xt_imgnl immap %s\n")
	        call pargi (index)
		call pargstr (XT_IMNAME(xt))
	}
	im = immap (XT_IMNAME(xt), READ_ONLY, XT_ARG(xt))
	XT_IM(xt) = im
	call imseti (im, IM_BUFSIZE, XT_BUFSIZE(xt))
	call mfree (XT_BUF(xt), XT_BTYPE(xt))
	nopen = nopen + 1
	XT_CLOSEFD(xt) = YES
	if (nopenpix < MAX_OPENPIX) {
	    XT_CLOSEFD(xt) = NO
	    nopenpix = nopenpix + 1
	}
	if (XT_CLOSEFD(xt) == YES)
	    call imseti (im, IM_CLOSEFD, YES)
	XT_FLAG(xt) = flag

	return (imgnld (im, buf, v))
end

