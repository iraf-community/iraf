
include	<imhdr.h>
include	<imset.h>
include	<time.h>

# IMC_LOGSUM -- Log information about sum combining.
#
#   1.  If the package "verbose" parameter is set print to STDOUT.
#   2.  If the package "logfile" parameter is not null append to the file.

procedure imc_logsum (str, log, in, out, nimages)

int	log			# Log file descriptor
char	str[ARB]		# Log string (includes combine option)
pointer	in[nimages]		# Input images
pointer	out			# Output image
int	nimages			# Number of input images

int	i, open()
long	clktime()
bool	clgetb()
real	exposure, darktime, dark, hdmgetr()
pointer	sp, time, fname, t
errchk	open

begin
	call smark (sp)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (t, nimages, TY_REAL)

	# Determine the exposure times.
	exposure = 0.
	darktime = 0.
	do i = 1, nimages {
	    iferr (Memr[t+i-1] = hdmgetr (in[i], "exptime"))
		Memr[t+i-1] = 0.
	    iferr (dark = hdmgetr (in[i], "darktime"))
		dark = Memr[t+i-1]
	    exposure = exposure + Memr[t+i-1]
	    darktime = darktime + dark
	}
	call hdmputr (out, "exptime", exposure)
	call hdmputr (out, "darktime", darktime)

	call cnvdate (clktime(0), Memc[time], SZ_DATE)

	if (clgetb ("verbose")) {
	    call printf ("%s combine: %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	    call printf ("  %20s %6s\n")
	        call pargstr ("Images")
	        call pargstr ("Exp")
	    do i = 1, nimages {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call printf ("  %20s %6.1f\n")
		    call pargstr (Memc[fname])
		    call pargr (Memr[t+i-1])
	    }
	    call printf ("  -------------------- ------\n")
	    call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	    call printf ("  %20s %6.1f\n")
		call pargstr (Memc[fname])
	        call pargr (exposure)
	    call flush (STDOUT)
	}
		    
	# Append to the "logfile" if not null.
	call clgstr ("logfile", Memc[fname], SZ_FNAME)
	call xt_stripwhite (Memc[fname])
	if (Memc[fname] != EOS) {
	    log = open (Memc[fname], APPEND, TEXT_FILE)
	    call fprintf (log, "%s combine: %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	    call fprintf (log, "  %20s %6s\n")
	        call pargstr ("Images")
	        call pargstr ("Exp")
	    do i = 1, nimages {
		call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s %6.1f\n")
		    call pargstr (Memc[fname])
		    call pargr (Memr[t+i-1])
	    }
	    call fprintf (log, "  -------------------- ------\n")
	    call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	    call fprintf (log, "  %20s %6.1f\n")
		call pargstr (Memc[fname])
	        call pargr (exposure)
	    call flush (log)
	    call close (log)
	}

	call sfree (sp)
end
