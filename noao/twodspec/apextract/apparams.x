define	PARAMS		"apextract$apparams.dat"
define	LEN_LINE	80

# AP_PARAMS -- Show the parameters.

procedure ap_params (file, image, line, nsum)

char	file[ARB]		# Aperture file
char	image[ARB]		# Image name
int	line			# Image line
int	nsum			# Number of lines to sum

int	in, out, len, nchar
pointer	sp, param, type, format, instr, outstr, str
bool	apgetb()
int	apgeti(), open(), fscan(), nscan(), strlen()
real	apgetr()
errchk	open

begin
	# Open input parameter file and output stream.
	in = open (PARAMS, READ_ONLY, TEXT_FILE)
	out = open (file, APPEND, TEXT_FILE)

	call smark (sp)
	call salloc (param, SZ_LINE, TY_CHAR)
	call salloc (type, 10, TY_CHAR)
	call salloc (format, 10, TY_CHAR)
	call salloc (instr, SZ_LINE, TY_CHAR)
	call salloc (outstr, SZ_LINE, TY_CHAR)
	call salloc (str, SZ_LINE, TY_CHAR)
	Memc[outstr] = EOS

	call fprintf (out, "%32tAPEXTRACT PARAMETERS\n")
	call fprintf (out, "image=%s%27tline=%d%53tnsum=%d\n")
	    call pargstr (image)
	    call pargi (line)
	    call pargi (nsum)
	call fprintf (out, "database=%s%27tlogfile=%s%53tplotfile=%s\n\n")
	    call clgstr ("database", Memc[str], SZ_LINE)
	    call pargstr (Memc[str])
	    call clgstr ("logfile", Memc[str], SZ_LINE)
	    call pargstr (Memc[str])
	    call clgstr ("plotfile", Memc[str], SZ_LINE)
	    call pargstr (Memc[str])

	len = 0
	while (fscan (in) != EOF) {
	    call gargwrd (Memc[param], SZ_LINE)
	    call gargwrd (Memc[type], 10)
	    call gargwrd (Memc[format], 10)
	    call gargi (nchar)
	    if (nscan() < 4)
		nchar = LEN_LINE

	    if (len + nchar > LEN_LINE) {
		call strcat ("\n", Memc[outstr], SZ_LINE)
		call fprintf (out, Memc[outstr])
		Memc[outstr] = EOS
		len = 0
	    }

	    if (nscan() == 1) {
		call sprintf (Memc[outstr], SZ_LINE, "%%%dt%s")
		    call pargi ((LEN_LINE - strlen (Memc[param])) / 2)
		    call pargstr (Memc[param])
	    } else if (nscan() == 4) {
	        call sprintf (Memc[str], SZ_LINE, "%%%dt%s=")
		    call pargi (len+1)
		    call pargstr (Memc[param])
	        call strcat (Memc[str], Memc[outstr], SZ_LINE)

	        call sprintf (Memc[str], SZ_LINE, Memc[format])
	        switch (Memc[type]) {
	        case 'b':
		    call pargb (apgetb (Memc[param]))
	        case 'i':
		    call pargi (apgeti (Memc[param]))
	        case 'r':
		    call pargr (apgetr (Memc[param]))
	        case 's':
		    call apgstr (Memc[param], Memc[instr], SZ_LINE)
		    call pargstr (Memc[instr])
	        }
	        call strcat (Memc[str], Memc[outstr], SZ_LINE)
	    }
	    len = len + nchar
	}
	call strcat ("\n", Memc[outstr], SZ_LINE)
	call fprintf (out, Memc[outstr])

	call close (in)
	call close (out)
	call sfree (sp)
end
