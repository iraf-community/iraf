include <pkg/inlfit.h>

# ING_VARIABLES -- Write the variable numbers, names and minimum and maximum
# values to a file.

procedure ing_variablesr (in, file, nvars)

pointer	in			# pointer to the inlfit structure
char	file[ARB]		# output file name
int	nvars			# number of variables

int	i, fd
pointer	sp, labels, pvnames, name, minptr, maxptr
int	open(), inlstrwrd()
pointer	in_getp()

begin
	if (file[1] == EOS)
	    return
	fd = open (file, APPEND, TEXT_FILE)

	call smark (sp)
	call salloc (labels, SZ_LINE, TY_CHAR)
	call salloc (pvnames, SZ_LINE, TY_CHAR)
	call salloc (name, SZ_LINE, TY_CHAR)
	call in_gstr (in, INLVLABELS, Memc[labels], SZ_LINE)
	call strcpy (Memc[labels], Memc[pvnames], SZ_LINE)

	# Print the title string.
	call fprintf (fd, "\n%-10.10s  %-10.10s  %14.14s  %14.14s\n")
	    call pargstr ("number")
	    call pargstr ("variable")
	    call pargstr ("minimum")
	    call pargstr ("maximum")

	# Print the variables.
	minptr = in_getp (in, INLXMIN)
	maxptr = in_getp (in, INLXMAX)
	do i = 1, nvars {
	    if (inlstrwrd (i, Memc[name], SZ_LINE, Memc[pvnames]) == 0) {
		call sprintf (Memc[name], SZ_LINE, "var %d")
		    call pargi (i)
	    }
	    call fprintf (fd, "%-10.2d  %-10.10s  ")
		call pargi (i)
		call pargstr (Memc[name])
	    call fprintf (fd, "%14.7f  %14.7f\n")
		call pargr (Memr[minptr+i-1])
		call pargr (Memr[maxptr+i-1])
	}
	call fprintf (fd, "\n")

	call close (fd)
	call sfree (sp)
end
