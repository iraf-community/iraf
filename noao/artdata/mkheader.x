include	<error.h>
include	<imhdr.h>
include	<imio.h>
include	<ctype.h>

define	LEN_COMMENT	70			# Maximum comment length
define	IDB_RECLEN	80			# Length of FITS record (card)
define	COMMENT		"COMMENT"		# Comment key
define	IS_FITS		(IS_DIGIT($1)||IS_UPPER($1)||($1=='-')||($1=='_'))

# MKH_HEADER -- Append or substitute new image header from an image or file.
# Only the legal FITS cards (ignoring leading whitespace) will be copied
# from a file.

procedure mkh_header (im, fname, append, verbose)

pointer	im			# IMIO pointer
char	fname[ARB]		# Image or data file name
bool	append			# Append to existing header?
bool	verbose			# Verbose?

int	i, j, curlen, buflen, max_lenuserarea
pointer	ua, fd, out
pointer	sp, str

int	open(), getline(), nowhite(), strlen(), stropen()
pointer	immap()
errchk	open, stropen

begin
	if (nowhite (fname, fname, SZ_FNAME) == 0)
	    return

        # Open the user area string for appending.  'buflen' is the malloc-ed
        # buffer length in struct units; IMU is the struct offset to the user
        # area, i.e., the size of that part of the image descriptor preceding
        # the user area.  If the buffer fills we must allow one extra char for
        # the EOS delimiter; since storage for the image descriptor was
        # allocated in struct units the storage allocator will not have
        # allocated space for the extra EOS char.

	ua = IM_USERAREA(im)
	if (!append)
	    Memc[ua] = EOS
	curlen = strlen (Memc[ua])
	buflen = LEN_IMDES + IM_LENHDRMEM(im)
	max_lenuserarea = (buflen - IMU) * SZ_STRUCT - 1

	# Append from an image header.
	ifnoerr (fd = immap (fname, READ_ONLY, 0)) {
	    iferr {
		i = strlen (Memc[IM_USERAREA(fd)]) + strlen (Memc[ua])
		call strcat (Memc[IM_USERAREA(fd)], Memc[ua], max_lenuserarea)
		if (i > max_lenuserarea)
		    call error (1, "Possibly failed to add all the keywords")
	    } then {
		call erract (EA_WARN)

		# Check for truncated card.
		for (i=ua+max_lenuserarea-1;  i > ua;  i=i-1) {
		    if (Memc[i] == '\n') {
			Memc[i+1] = EOS
			break
		    }
		}
	    }
	    call imunmap (fd)

	# Append from a text file.
	} else {
	    fd = open (fname, READ_ONLY, TEXT_FILE)
	    out = stropen (Memc[ua+curlen], max_lenuserarea - curlen, APPEND)

	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)

	    iferr {
		while (getline (fd, Memc[str]) != EOF) {
		    for (i=str; IS_WHITE(Memc[i]); i=i+1)
			;
		    for (j=i; IS_FITS(Memc[j]); j=j+1)
			;
		    for (; j<i+8 && Memc[j]==' '; j=j+1)
			;
		    if (j<i+8 && (Memc[j] != EOS || Memc[j] != '\n'))
			next
		    if (Memc[j] == '=' && Memc[j+1] != ' ')
			next
		    for (; j<i+IDB_RECLEN && Memc[j] != EOS; j=j+1)
			;
		    if (Memc[j-1] == '\n')
			Memc[j-1] = EOS
		    if (j == i + IDB_RECLEN || Memc[j] == '\n')
			Memc[j] = EOS
		    if (strlen (Memc[ua]) + IDB_RECLEN >= max_lenuserarea)
			call error (1,
			    "Possibly failed to add all the keywords")
		    call fprintf (out, "%s%*t\n")
			call pargstr (Memc[i])
			call pargi (IDB_RECLEN+1)
		}
	    } then {
		call erract (EA_WARN)

		# Check for truncated card.
		call close (out)
		for (i=ua+max_lenuserarea-1;  i > ua;  i=i-1) {
		    if (Memc[i] == '\n') {
			Memc[i+1] = EOS
			break
		    }
		}
	    }
	    call close (out)
	    call close (fd)
	    call sfree (sp)
	}
	if (verbose) {
	    if (append) {
		call printf ("%s: Image header from %s appended\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (fname)
	    } else {
		call printf ("%s: Image header from %s substituted\n")
		    call pargstr (IM_HDRFILE(im))
		    call pargstr (fname)
	    }
	}
end


# MKH_COMMENT -- Add comment to header.
# Remove any tab characters.

procedure mkh_comment (im, comment)

pointer	im			# image descriptor
char	comment[ARB]		# comment

begin
	call imputh (im, COMMENT, comment)
end

## MKH_COMMENT -- Add comment to header.
## Remove any tab characters.
#
#define	MAX_COMMENT	72
#
#procedure mkh_comment (im, comment)
#
#pointer	im			# image descriptor
#char	comment[ARB]		# comment
#
#int	i, j, k, stridxs()
#pointer	sp, cmmt
#
#begin
#	if (stridxs ("\t", comment) == 0) {
#	    call imputh (im, COMMENT, comment)
#	    return
#	}
#
#	call smark (sp)
#	call salloc (cmmt, MAX_COMMENT, TY_CHAR)
#	j = 0
#	for (i=1; comment[i]!=EOS; i=i+1) {
#	    if (comment[i] == '\t') {
#		k = min (j + 8 - mod (j, 8), MAX_COMMENT)
#		for (; j < k; j=j+1)
#		    Memc[cmmt+j] = ' '
#	    } else {
#		Memc[cmmt+j] = comment[i]
#		j = j + 1
#	    }
#	    if (j == MAX_COMMENT)
#		break
#	}
#	Memc[cmmt+j] = EOS
#	call imputh (im, COMMENT, Memc[cmmt])
#	call sfree (sp)
#end


# MKH_COMMENT1 -- Make comment out of CL parameter.

procedure mkh_comment1 (im, param, type)

pointer	im			# image descriptor
char	param[ARB]		# parameter name
int	type			# datatype

bool	clgetb()
int	clgeti()
real	clgetr()
double	clgetd()
pointer	sp, comment, str

begin
	call smark (sp)
	call salloc (comment, LEN_COMMENT, TY_CHAR)

	switch (type) {
	case 'b':
	    call sprintf (Memc[comment], LEN_COMMENT, "%9t%s%24t%b")
		call pargstr (param)
	    	call pargb (clgetb (param))
	case 'i':
	    call sprintf (Memc[comment], LEN_COMMENT, "%9t%s%24t%d")
		call pargstr (param)
	    	call pargi (clgeti (param))
	case 'r':
	    call sprintf (Memc[comment], LEN_COMMENT, "%9t%s%24t%g")
		call pargstr (param)
	    	call pargr (clgetr (param))
	case 'd':
	    call sprintf (Memc[comment], LEN_COMMENT, "%9t%s%24t%g")
		call pargstr (param)
	    	call pargd (clgetd (param))
	case 's':
	    call salloc (str, SZ_FNAME, TY_CHAR)
	    call clgstr (param, Memc[str], SZ_FNAME)
	    call sprintf (Memc[comment], LEN_COMMENT, "%9t%s%24t%s")
		call pargstr (param)
	    	call pargstr (Memc[str])
	}

	call mkh_comment (im, Memc[comment])
	call sfree (sp)
end
