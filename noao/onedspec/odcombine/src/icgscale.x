# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"icombine.h"


# IC_GSCALE -- Get scale values as directed by CL parameter.
# Only those values which are INDEF are changed.
# The values can be one of those in the dictionary, from a file specified
# with a @ prefix, or from an image header keyword specified by a ! prefix.

int procedure ic_gscale (param, name, dic, in, exptime, values, nimages)

char	param[ARB]		#I CL parameter name
char	name[SZ_FNAME]		#O Parameter value
char	dic[ARB]		#I Dictionary string
pointer	in[nimages]		#I IMIO pointers
real	exptime[nimages]	#I Exposure times
real	values[nimages]		#O Values
int	nimages			#I Number of images

int	type			#O Type of value

int	fd, i, nowhite(), open(), fscan(), nscan(), strdic()
real	rval, imgetr()
pointer	errstr
errchk	open, imgetr

include	"icombine.com"

begin
	call clgstr (param, name, SZ_FNAME)
	if (nowhite (name, name, SZ_FNAME) == 0)
	    type = S_NONE
	else if (name[1] == '@') {
	    type = S_FILE
	    do i = 1, nimages
		if (IS_INDEFR(values[i]))
		    break
	    if (i <= nimages) {
		fd = open (name[2], READ_ONLY, TEXT_FILE)
		i = 0
		while (fscan (fd) != EOF) {
		    call gargr (rval)
		    if (nscan() != 1)
			next
		    if (i == nimages) {
		       call eprintf (
			   "Warning: Ignoring additional %s values in %s\n")
			   call pargstr (param)
			   call pargstr (name[2])
		       break
		    }
		    i = i + 1
		    if (IS_INDEFR(values[i]))
			values[i] = rval
		}
		call close (fd)
		if (i < nimages) {
		    call salloc (errstr, SZ_LINE, TY_CHAR)
		    call sprintf (errstr, SZ_FNAME,
			"Insufficient %s values in %s")
			call pargstr (param)
			call pargstr (name[2])
		    call error (1, errstr)
		}
	    }
	} else if (name[1] == '!') {
	    type = S_KEYWORD
	    do i = 1, nimages {
		if (IS_INDEFR(values[i]))
		    values[i] = imgetr (in[i], name[2])
		if (project) {
		    call amovkr (values, values, nimages)
		    break
		}
	    }
	} else {
	    type = strdic (name, name, SZ_FNAME, dic)
	    if (type == 0)
		call error (1, "Unknown scale, zero, or weight type")
	    if (type==S_EXPOSURE)
		do i = 1, nimages
		    if (IS_INDEFR(values[i]))
			values[i] = max (0.001, exptime[i])
	}

	return (type)
end
