# AP_SHOW -- Write the aperture data to a text file.

procedure ap_show (file, image, line, nsum)

char	file[ARB]		# Aperture file
char	image[ARB]		# Image name
int	line			# Image line
int	nsum			# Number of lines to sum

int	fd, open()
real	clgetr()
errchk	open

begin
	# Return if an error opening the output stream.
	fd = open (file, NEW_FILE, TEXT_FILE)

	call fprintf (fd, "image = %s\nline = %d\nnsum = %d\n")
	    call pargstr (image)
	    call pargi (line)
	    call pargi (nsum)
	call fprintf (fd, "width = %g\nradius = %g\nthreshold = %g\n")
	    call pargr (clgetr ("apedit.width"))
	    call pargr (clgetr ("apedit.radius"))
	    call pargr (clgetr ("apedit.threshold"))

	call close (fd)
end
