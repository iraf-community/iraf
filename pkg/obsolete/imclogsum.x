# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<imhdr.h>
include	<imset.h>
include	<time.h>

# IMC_LOGSUM -- Log information about sum combining.

procedure imc_logsum (str, log, in, out, nimages)

int	log			# Log file descriptor
char	str[ARB]		# Log string (includes combine option)
pointer	in[nimages]		# Input images
pointer	out			# Output image
int	nimages			# Number of input images

int	i
long	clktime()
real	exposure, imgetr()
pointer	sp, expname, time, fname, t

begin
	call smark (sp)
	call salloc (expname, SZ_FNAME, TY_CHAR)
	call salloc (time, SZ_DATE, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (t, nimages, TY_REAL)

	# Determine the exposure times.
	call clgstr ("expname", Memc[expname], SZ_FNAME)
	if (Memc[expname] != EOS) {
	    exposure = 0.
	    do i = 1, nimages {
	        iferr (Memr[t+i-1] = imgetr (in[i], Memc[expname]))
		    Memr[t+i-1] = 0.
	        exposure = exposure + Memr[t+i-1]
	    }
	    call imaddr (out, "exptime", exposure)
	}

	# Append to the logfile if not null.
	call cnvdate (clktime(0), Memc[time], SZ_DATE)
	if (log != NULL) {
	    call fprintf (log, "%s imcombine: %s\n")
		call pargstr (Memc[time])
		call pargstr (str)
	    if (Memc[expname] != EOS) {
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
	    } else {
	        call fprintf (log, "  %20s\n")
	            call pargstr ("Images")
	        do i = 1, nimages {
		    call imstats (in[i], IM_IMAGENAME, Memc[fname], SZ_FNAME)
	            call fprintf (log, "  %20s\n")
		        call pargstr (Memc[fname])
	        }
	        call fprintf (log, "  --------------------\n")
	        call imstats (out, IM_IMAGENAME, Memc[fname], SZ_FNAME)
	        call fprintf (log, "  %20s\n")
		    call pargstr (Memc[fname])
	    }
	    call flush (log)
	}

	call sfree (sp)
end
