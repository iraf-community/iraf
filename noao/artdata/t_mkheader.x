include	<error.h>
include	<imhdr.h>
include	<ctype.h>

define	LEN_UA		20000			# Maximum user header

# T_MKHEADER -- Append or substitute new image header from an image or file.
# Only the legal FITS cards (ignoring leading whitespace) will be copied
# from a file.

procedure t_mkheader ()

int	imlist			# List of images
int	flist			# List of data files
bool	append			# Append to existing keywords?
bool	verbose			# Verbose output?

int	stat
pointer	im, sp, image, fname

bool	clgetb()
int	imtopenp(), clpopnu(), clplen(), imtgetim(), clgfil()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	imlist = imtopenp ("images")
	flist = clpopnu ("headers")
	if (clplen (flist) == 0)
	    call error (1, "No header files specified")
	append = clgetb ("append")
	verbose = clgetb ("verbose")

	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    stat = clgfil (flist, Memc[fname], SZ_FNAME)

	    iferr (im = immap (Memc[image], READ_WRITE, LEN_UA)) {
		call erract (EA_WARN)
		next
	    }
	    iferr (call mkh_header (im, Memc[fname], append, verbose))
		call erract (EA_WARN)
	    call imunmap (im)
	}

	call imtclose (imlist)
	call clpcls (flist)
	call sfree (sp)
end
