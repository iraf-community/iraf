include	<time.h>

# AP_LOG -- Verbose, log, and error output.

procedure ap_log (str, log, verbose, err)

char	str[ARB]	# String
int	log		# Write to log if logfile defined?
int	verbose		# Write to stdout if verbose?
int	err		# Write to stdout?

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

	if (err == YES || (verbose == YES && clgetb ("verbose"))) {
	    call printf ("%s: %s\n")
	        call pargstr (Memc[date])
	        call pargstr (str)
	    call flush (STDOUT)
	}

	if (log == YES) {
	    call clgstr ("logfile", Memc[logfile], SZ_FNAME)
	    if (Memc[logfile] != EOS) {
	        fd = open (Memc[logfile], APPEND, TEXT_FILE)
	        call fprintf (fd, "%s: %s\n")
	            call pargstr (Memc[date])
	            call pargstr (str)
	        call flush (fd)
	        call close (fd)
	    }
	}

	call sfree (sp)
end


# AP_GOPEN/AP_GCLOSE -- Open and close the graphics device.
# The device "stdgraph" is used.

procedure ap_gopen (gp)

pointer	gp		# GIO pointer
pointer	gplast		# Last GIO pointer

int	flag
pointer	gopen()
errchk	gopen

data	flag/NO/
common	/apgio/ gplast

begin
	if (flag == NO) {
	    flag = YES
	    call ap_gclose ()
	}

	if (gplast == NULL)
	    gplast = gopen ("stdgraph", NEW_FILE, STDGRAPH)

	gp = gplast
end

procedure ap_gclose ()

int	flag
pointer	gplast

data	flag/NO/
common	/apgio/ gplast

begin
	if (flag == NO) {
	    flag = YES
	    gplast = NULL
	}

	if (gplast != NULL) {
	    call gclose (gplast)
	    gplast = NULL
	}
end


# AP_POPEN -- Open the plot device or metacode file.  This includes CLIO
# to get the plot device.

procedure ap_popen (gp, fd, type)

pointer	gp		# GIO pointer
int	fd		# FIO channel for metacode file
char	type[ARB]	# Plot type

bool	streq(), strne()
int	open(), nowhite(), strncmp()
pointer	sp, str, gopen()
errchk	gopen, open

begin
	call smark (sp)
	call salloc (str, SZ_FNAME, TY_CHAR)
	call clgstr ("plotfile", Memc[str], SZ_LINE)

	gp = NULL
	fd = NULL
	if (nowhite (Memc[str], Memc[str], SZ_FNAME) > 0) {
	    if (strncmp ("debug", Memc[str], 5) == 0) {
		if (streq (type, Memc[str+5]) || streq ("all", Memc[str+5])) {
		    fd = open (Memc[str], APPEND, BINARY_FILE)
		    gp = gopen ("stdvdm", APPEND, fd)
		}
	    } else if (strne ("fits", type)) {
		fd = open (Memc[str], APPEND, BINARY_FILE)
		gp = gopen ("stdvdm", APPEND, fd)
	    }
	}

	call sfree (sp)
end


# AP_PCLOSE -- Close plot file.

procedure ap_pclose (gp, fd)

pointer	gp		# GIO pointer
int	fd		# FIO channel for metacode file

begin
	if (gp != NULL)
	    call gclose (gp)
	if (fd != NULL)
	    call close (fd)
end
