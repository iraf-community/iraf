# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<finfo.h>
include	<error.h>
include	<time.h>


# TOUCH -- Change file access and modification times, creating an empty
# file if necessary.  File times may come from the current system time,
# a user-specified string, or a reference file.

procedure t_touch()

pointer	list
char	fname[SZ_FNAME], time[SZ_FNAME]
char	ref_file[SZ_PATHNAME]
char	at_str[SZ_TIME], mt_str[SZ_TIME]
bool	create, atime, mtime, verbose

long	ref[LEN_FINFO]
long	t_atime, t_mtime, ltime

long	clktime()
int	clpopni(), clgfil()
int	finfo(), dtm_ltime()
bool	clgetb()

errchk	touch

begin
	# Initialize.
	call aclrc (time, SZ_FNAME)
	call aclrc (ref_file, SZ_PATHNAME)

	# Process the parameters.
	list = clpopni ("files")

	create  = clgetb ("create")
	atime   = clgetb ("atime")
	mtime   = clgetb ("mtime")
	verbose = clgetb ("verbose")
	call clgstr ("time", time, SZ_FNAME)

	# Check for error conditions.
	if (!atime && !mtime) {
	    call eprintf (
		"ERROR: Must specify at least one of 'atime' or 'mtime'.\n")
	    call clpcls (list)
	    return
	} 

	# Get the time to be set.
	if (time[1] == EOS) {
	    # No 'time' param, look for a reference file to use.
	    call clgstr ("ref_file", ref_file, SZ_PATHNAME)
	    if (ref_file[1] == EOS) {
		# No 'ref_file' param, use the current system time.
		t_atime = clktime (long(0))
		t_mtime = t_atime

		if (verbose) {
	    	    call cnvtime (t_atime, at_str, SZ_TIME)
	    	    call printf ("Modifying to system time: %s\n\n")
			call pargstr (at_str)
		}

	    } else {
		# Open the reference file and use those times.
		if (finfo (ref_file, ref) == ERR) {
		    call eprintf ("Error opening reference file: '%s'\n")
		        call pargstr (ref_file)
		    return
		}
		t_atime = FI_ATIME(ref)
		t_mtime = FI_MTIME(ref)

		if (verbose) {
	    	    call cnvtime (t_atime, at_str, SZ_TIME)
	    	    call cnvtime (t_mtime, mt_str, SZ_TIME)
	    	    call printf ("Modifying to reference time: ")
		    call printf ("%s (atime)\n%30t%s (mtime)\n\n")
			call pargstr (at_str)
			call pargstr (mt_str)
		}
	    }

	} else {
	    # Parse the time parameter to get the modification time.
	    if (dtm_ltime (time, ltime) == ERR) {
		if (ltime < 0) 
		    call eprintf ("Invalid time string: '%s'\n")
		else
		    call eprintf ("Error parsing time string: '%s'\n")
		        call pargstr (time)
		return
	    }
	    t_atime = ltime
	    t_mtime = ltime

	    if (verbose) {
	        call cnvtime (t_atime, at_str, SZ_TIME)
	    	call printf ("Modifying to user-specified time: %s\n\n")
		    call pargstr (at_str)
	     }
	}

	# Now apply the atime/mtime params to update only what's needed.
	if (!atime) t_atime = NULL
	if (!mtime) t_mtime = NULL


	# Process the list of input files.
	while (clgfil (list, fname, SZ_FNAME) != EOF) {
	    iferr (call touch (fname, create, t_atime, t_mtime, verbose))
		;
	}

	# Clean up and close the list.
	call clpcls (list)
end


# TOUCH -- Touch a file to modify the times.

procedure touch (fname, create, atime, mtime, verbose)

char	fname[ARB]			#i file name to touch
bool	create				#i create file if necessary?
long	atime, mtime			#i access and modify time
bool	verbose				#i verbose output?

char 	dir[SZ_PATHNAME], ip
char 	vfn[SZ_PATHNAME]
int	fd
int	access(), open(), futime()

begin
	if (verbose) {
	    call printf ("%s: ")
		call pargstr (fname)
	}

	# Check first it the file exists.
	if (access (fname, 0, 0) == NO) {
	    if (create) {

		call fnldir (fname, vfn, SZ_PATHNAME)
		call fpathname (vfn, dir, SZ_PATHNAME)
		if (access (dir, READ_WRITE, 0) == NO) {
		    for (ip=1; dir[ip] != '!'; ip=ip+1)
			;
		    call eprintf ("Error: Cannot open directory '%s'\n")
		        call pargstr (dir[ip+1])
		    call erract (EA_ERROR)
		    return;
		}

		# Create a new empty file.
	        iferr (fd = open (fname, NEW_FILE, TEXT_FILE)) {
		    call eprintf ("Error: Cannot touch file '%s'\n")
		        call pargstr (fname)
		    call erract (EA_ERROR)
		    return;
		}
	        call close (fd)
		if (verbose) call printf ("(created) ")

	    } else {
		if (verbose) call printf ("(not created)\n")
	        return
	    }
	}

	# Update the times.
	if (futime (fname, atime, mtime) == ERR)
	    call eprintf ("Error processing file\n")

	else if (verbose) {
	    call printf ("(updated %s%s%s)\n")
		if (atime > 0)
		    call pargstr ("atime") 
		else
		    call pargstr ("")
		if (atime > 0 && mtime > 0)
		    call pargstr ("/")     
		else
		    call pargstr ("")
		if (mtime > 0)
		    call pargstr ("mtime") 
		else
		    call pargstr ("")
	}
end
