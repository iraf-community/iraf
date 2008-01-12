
# CCDLOG -- Log information about the processing with the image name.
#
#   1.  If the package "verbose" parameter is set print the string preceded
#	by the image name.
#   2.  If the package "logfile" parameter is not null append the string,
#	preceded by the image name, to the file.

procedure ccdlog (image, str)

char	image[ARB]		# Image name
char	str[ARB]		# Log string

int	fd, open()
bool	clgetb()
pointer	sp, fname
errchk	open

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Write to the standard error output if "verbose".
	if (clgetb ("verbose")) {
	    call eprintf ("%s: %s\n")
		call pargstr (image)
		call pargstr (str)
	}

	# Append to the "logfile" if not null.
	call clgstr ("logfile", Memc[fname], SZ_FNAME)
	call xt_stripwhite (Memc[fname])
	if (Memc[fname] != EOS) {
	    fd = open (Memc[fname], APPEND, TEXT_FILE)
	    call fprintf (fd, "%s: %s\n")
		call pargstr (image)
		call pargstr (str)
	    call close (fd)
	}

	call sfree (sp)
end
