include	<imhdr.h>
include	<ctype.h>

define	LEN_UA		20000			# Required minimum user length
define	LEN_COMMENT	70			# Maximum comment length
define	COMMENT		"COMMENT   "		# Comment key
define	IS_FITS		(IS_DIGIT($1)||IS_UPPER($1)||($1=='-')||($1=='_'))

# MKH_HEADER -- Append or substitute new image header from an image or file.
# Only the legal FITS cards (ignoring leading whitespace) will be copied
# from a file.

procedure mkh_header (im, fname, append, verbose)

pointer	im			# IMIO pointer
char	fname[ARB]		# Image or data file name
bool	append			# Append to existing header?
bool	verbose			# Verbose?

int	i, j
pointer	ua, fd
pointer	sp, str

int	open(), getline(), nowhite()
pointer	immap()
errchk	open

begin
	if (nowhite (fname, fname, SZ_FNAME) == 0)
	    return

	ua = IM_USERAREA(im)
	ifnoerr (fd = immap (fname, READ_ONLY, LEN_UA)) {
	    if (append)
		call strcat (Memc[IM_USERAREA(fd)], Memc[ua], LEN_UA)
	    else
		call strcpy (Memc[IM_USERAREA(fd)], Memc[ua], LEN_UA)
	    call imunmap (fd)
	} else {
	    fd = open (fname, READ_ONLY, TEXT_FILE)

	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)

	    if (!append)
		Memc[ua] = EOS
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
		for (; j<i+80 && Memc[j] != EOS; j=j+1)
		    ;
		if (Memc[j-1] != '\n') {
		    Memc[j] = '\n'
		    Memc[j+1] = EOS
		}
		call strcat (Memc[i], Memc[ua], LEN_UA)
	    }
	    call sfree (sp)
	    call close (fd)
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

procedure mkh_comment (im, comment)

pointer	im			# image descriptor
char	comment[ARB]		# comment

#pointer	ua

begin
	#ua = IM_USERAREA(im)
	#call strcat (COMMENT, Memc[ua], LEN_UA)
	#call strcat (comment, Memc[ua], LEN_UA)
	#call strcat ("\n", Memc[ua], LEN_UA)
	call imputh (im, "COMMENT", comment)
end


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
