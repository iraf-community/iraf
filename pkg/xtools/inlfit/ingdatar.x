include	<pkg/inlfit.h>

define	NPERLINE	5

# ING_DATA -- List the raw data on the screen.

procedure ing_datar (in, file, x, names, npts, nvars, len_name)

pointer	in			# INLFIT pointer
char	file[ARB]		# Output file name
real	x[ARB]			# Ordinates (npts * nvars)
char	names[ARB]		# Object names
int	npts			# Number of data points
int	nvars			# Number of variables
int	len_name		# Length of the name

int	i, j, fd
pointer	sp, vnames, name
int	open()
int	inlstrwrd()
errchk	open()

begin
	# Open the output file.
	if (file[1] == EOS)
	    return
	fd = open (file, APPEND, TEXT_FILE)

	# Test the number of data points.
	if (npts == 0) {
	    call eprintf ("Incomplete output - no data points for fit\n")
	    return
	}

	# Allocate memory.
	call smark (sp)
	call salloc (vnames, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)

	# Get the variable names.
	call in_gstr (in, INLVLABELS, Memc[vnames], SZ_LINE)

	# Print title.
	do j = 1, nvars + 1 {
	    if (mod (j, NPERLINE) == 1) {
		call fprintf (fd, "\n")
		call fprintf (fd, "#")
	    }
	    if (j == 1) {
		call fprintf (fd, "%14.14s ")
		    call pargstr ("objectid")
	    } else if (inlstrwrd (j-1, Memc[name], SZ_LINE, Memc[vnames]) !=
	        0) {
		call fprintf (fd, "%14.14s ")
		    call pargstr (Memc[name])
	    } else {
		call fprintf (fd, "%12.12s%02.2d ")
		    call pargstr ("var")
		    call pargi (j-1)
	    }
	}
	call fprintf (fd, "\n")
	
	# List the  variables values.
	do i = 1, npts {
	    do j = 1, nvars + 1 {
		if (j == 1) {
		    call fprintf (fd, "\n")
	            call fprintf (fd, "%15.15s")
			call pargstr (names[(i-1)*len_name+1])
	        } else if (mod (j, NPERLINE) == 1) {
		    call fprintf (fd, "\n")
	            call fprintf (fd, "*%14.7g")
		        call pargr (x[(i-1)*nvars+j-1])
		} else {
	            call fprintf (fd, " %14.7g")
		        call pargr (x[(i-1)*nvars+j-1])
		}
	    }
	}
	call fprintf (fd, "\n\n")

	# Free allocated memory and close output file.
	call sfree (sp)
	call close (fd)
end
