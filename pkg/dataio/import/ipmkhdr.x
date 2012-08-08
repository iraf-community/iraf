include	<imhdr.h>
include	<ctype.h>
include	"import.h"

define	LEN_COMMENT	70			# Maximum comment length
define	COMMENT		"COMMENT   "		# Comment key
define	IS_FITS		(IS_DIGIT($1)||IS_UPPER($1)||($1=='-')||($1=='_'))

# IP_MKHEADER -- Append or substitute new image header from an image or file.
# Only the legal FITS cards (ignoring leading whitespace) will be copied
# from a file.

procedure ip_mkheader (im, fname)

pointer	im			# IMIO pointer
char	fname[ARB]		# Image or data file name

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
	    call strcpy (Memc[IM_USERAREA(fd)], Memc[ua], LEN_UA)
	    call imunmap (fd)
	} else {
	    fd = open (fname, READ_ONLY, TEXT_FILE)

	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)

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
end
