include	<time.h>

# AP_LOG -- Write to the log file if defined.

procedure ap_log (str)

char	str[ARB]	# String

int	fd, open()
long	clktime()
bool	clgetb()
pointer	sp, logfile, date
errchk	open

begin
	call smark (sp)
	call salloc (logfile, SZ_LINE, TY_CHAR)
	call salloc (date, SZ_DATE, TY_CHAR)
	call cnvdate (clktime(0), Memc[date], SZ_DATE)

	if (clgetb ("apio.verbose")) {
	    call printf ("%s: %s\n")
	        call pargstr (Memc[date])
	        call pargstr (str)
	    call flush (STDOUT)
	}

	call clgstr ("apio.logfile", Memc[logfile], SZ_FNAME)
	if (Memc[logfile] != EOS) {
	    fd = open (Memc[logfile], APPEND, TEXT_FILE)
	    call fprintf (fd, "%s: %s\n")
	        call pargstr (Memc[date])
	        call pargstr (str)
	    call flush (fd)
	    call close (fd)
	}

	call sfree (sp)
end


# AP_GOPEN/AP_GCLOSE -- Open and close the graphics device.
# This includes CLIO to get the graphics device.

procedure ap_gopen (gp)

pointer	gp		# GIO pointer
pointer	gplast		# Last GIO pointer

pointer	sp, str, gopen()
errchk	gopen

data	gplast/NULL/

begin
	if (gplast == NULL) {
	    call smark (sp)
	    call salloc (str, SZ_LINE, TY_CHAR)
	    call clgstr ("apio.graphics", Memc[str], SZ_LINE)
	    gplast = gopen (Memc[str], NEW_FILE, STDGRAPH)
	    call sfree (sp)
	}

	gp = gplast
	return

entry	ap_gclose ()

	if (gplast != NULL) {
	    call gclose (gplast)
	    gplast = NULL
	}
end


# AP_POPEN -- Open the plot device or metacode file.  This includes CLIO
# to get the plot device.  To close use AP_PCLOSE.

procedure ap_popen (gp, fd)

pointer	gp		# GIO pointer
int	fd		# FIO channel for metacode file

int	open()
pointer	sp, str, gopen()
errchk	gopen, open

begin
	call smark (sp)
	call salloc (str, SZ_LINE, TY_CHAR)
	call clgstr ("apio.plots", Memc[str], SZ_LINE)

	gp = NULL
	fd = NULL
	if (Memc[str] != EOS) {
	    fd = open (Memc[str], APPEND, BINARY_FILE)
	    gp = gopen ("stdvdm", NEW_FILE, fd)
	}

	call sfree (sp)
	return

entry ap_pclose (gp, fd)

	if (gp != NULL)
	    call gclose (gp)
	if (fd != NULL)
	    call close (fd)
end
