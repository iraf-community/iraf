include	<error.h>


# T_MKHEADER -- Add / change header parameters using an input data file.
# The file consists of a keyword, a datatype (i,r,s), and a value.

procedure t_mkheader ()

int	imlist			# List of images
int	flist			# List of data files
bool	clobber			# Clobber existing keywords?
bool	verbose			# Verbose output?

int	i, stat, fd, ival
real	rval
pointer	im
pointer	sp, image, fname, key, type, str

bool	clgetb()
int	imtopenp(), clpopnu(), clplen(), imtgetim(), clgfil()
int	open(), fscan(), nscan(), imaccf()
pointer	immap()

begin
	call smark (sp)
	call salloc (image, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (key, SZ_FNAME, TY_CHAR)
	call salloc (type, SZ_FNAME, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)

	imlist = imtopenp ("images")
	flist = clpopnu ("headers")
	if (clplen (flist) == 0)
	    call error (1, "No header files specified")
	clobber = clgetb ("clobber")
	verbose = clgetb ("verbose")

	while (imtgetim (imlist, Memc[image], SZ_FNAME) != EOF) {
	    stat = clgfil (flist, Memc[fname], SZ_FNAME)

	    iferr (im = immap (Memc[image], READ_WRITE, 0)) {
		call erract (EA_WARN)
		next
	    }
	    iferr (fd = open (Memc[fname], READ_ONLY, TEXT_FILE)) {
		call erract (EA_WARN)
		if (stat == EOF)
		    break
		next
	    }

	    while (fscan (fd) != EOF) {
		call gargwrd (Memc[key], SZ_FNAME)
		call gargwrd (Memc[type], SZ_FNAME)
		if (nscan() < 2)
		    next
		i = imaccf (im, Memc[key])
		if (!clobber && i == YES) {
		    if (verbose) {
			call printf ("%s: %8.8s - unchanged\n")
			    call pargstr (Memc[image])
			    call pargstr (Memc[key])
		    }
		    next
		}
		switch (Memc[type]) {
		case 'i':
		    call gargi (ival)
		    if (nscan() == 3)
			call imaddi (im, Memc[key], ival)
		case 'r':
		    call gargr (rval)
		    if (nscan() == 3)
			call imaddr (im, Memc[key], rval)
		default:
		    call gargstr (Memc[str], SZ_LINE)
		    if (nscan() == 3)
			call imastr (im, Memc[key], Memc[str])
		}
		if (nscan() == 3) {
		    if (i == YES) {
		        call printf ("%s: %8.8s - changed\n")
			    call pargstr (Memc[image])
		            call pargstr (Memc[key])
		    } else {
		        call printf ("%s: %8.8s - added\n")
			    call pargstr (Memc[image])
		            call pargstr (Memc[key])
		    }
		}
	    }
	    call close (fd)
	    call imunmap (im)
	}

	call imtclose (imlist)
	call clpcls (flist)
	call sfree (sp)
end
