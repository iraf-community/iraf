# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <fio.h>
include <fset.h>
include <finfo.h>
include <time.h>
include <syserr.h>



# CHKUPDATE -- Check to see whether the current IRAF system is up to date
# relative to what's available on the NOAO server.


procedure t_chkupdate ()


char	version[SZ_FNAME], baseurl[SZ_FNAME], chkfile[SZ_LINE]
char	netpath[SZ_LINE], arch[SZ_FNAME], tmpfile[SZ_FNAME]
char	host[SZ_LINE], ref_file[SZ_LINE], release[SZ_LINE]
char	buf[SZ_LINE]
pointer	reply
bool	verbose
long	last, reldate, ndays, info[LEN_FINFO]
int	ip, op, fd, nread, interval, tm[LEN_TMSTRUCT]

int	clgeti(), envgets(), access(), open(), url_get()
int	strlen(),  getline(), finfo()
bool	clgetb()
long	clktime(), ctol()

define	done_		99

begin
	# Initialize strings.
	call aclrc (baseurl, SZ_LINE)
	call aclrc (release, SZ_LINE)
	call aclrc (ref_file, SZ_LINE)
	call aclrc (chkfile, SZ_LINE)
	call aclrc (tmpfile, SZ_FNAME)
	call aclrc (arch, SZ_FNAME)
	call aclrc (version, SZ_FNAME)

	call strcpy ("uparm$update", chkfile, SZ_LINE)
	call mktemp ("tmp$url", tmpfile, SZ_FNAME)
	if (envgets ("arch", arch, SZ_FNAME) == ERR) {
	    if (verbose)
	        call eprintf ("Error: cannot get architecture.\n")
	    return 
	}


	# Get the task parameters.
	call clgstr ("ref_file", ref_file, SZ_LINE)
	call clgstr ("release", release, SZ_LINE)
	call clgstr ("baseurl", baseurl, SZ_FNAME)
	verbose = clgetb ("verbose")
	interval = clgeti ("interval")


	# See whether we're doing an update check.  If the interval is less
	# than zero it means the update is disabled.  A zero value means we
	# check at each login, otherwise we've specified the number of days
	# since our last check.  The check time is kept in the uparm$update
	# file which is created the first time we run, and updated after each
	# check.
	if (interval < 0) {
	    return				# nothing to do

	} else if (interval >= 0) {
	    # See if current time is more than N interval days since last
	    # check.
	    if (access (chkfile, 0, 0) == NO) {
	        # No update file found, create one and check for updates.
	        fd = open (chkfile, NEW_FILE, TEXT_FILE)
	        call fprintf (fd, "%d\n")
		    call pargl (clktime (0))
	        call close (fd)
	    } else {
		call aclrc (buf, SZ_LINE)
	        fd = open (chkfile, READ_ONLY, TEXT_FILE)
		if (getline (fd, buf) != EOF) {
		    ip = 1
		    if (ctol (buf, ip, last) > 0) {
			ndays = (clktime(0) - last) / 86400
			if (ndays < interval) {
	        	    call close (fd)
			    return		# too recent
			}
		    }
		}
	        call close (fd)
	    }
	}


	# Transform the CL version string to one we can use in the URL.
	version[1] = 'v' ; op = 2
	for (ip=1; release[ip] != NULL && ip < SZ_LINE; ip=ip+1) {
	    if (release[ip] != '.') {
		version[op] = release[ip]
		op = op + 1
	    }
	}

	# Create the URL to the release timestamp file.
	call sprintf (netpath, SZ_LINE, "%s/%s/releases/%s")
	    call pargstr (baseurl)
	    call pargstr (version)
	    call pargstr (arch[2])


	# Access the URL and get the reply.
	call calloc (reply, SZ_LINE, TY_CHAR)

	if (url_get (netpath, tmpfile, reply) > 0) {
	    call aclrc (buf, SZ_LINE)
	    fd = open (tmpfile, READ_ONLY, TEXT_FILE)
	    if (getline (fd, buf) != EOF) {
		ip = 1
		if (ctol (buf, ip, reldate) > 0) {
	    	    if (access (ref_file, 0, 0) == NO) {
			if (verbose)
			    call eprintf ("Error: no release file\n")
	        	call close (fd)
			goto done_
	    	    }
	    	    if (access (ref_file, 0, 0) == YES &&
		        finfo (ref_file, info) != ERR) {

			call brktime (reldate, tm)
			call printf ("  ***  Checking update status... ")

			# Add a 1-day offset to the release date to ensure
			# the ref_file timestamp is less than the URL file,
			# both of which are created during the packaging of
			# a release.  This avoids a case where we would
			# *always* get an update notification.
			reldate = reldate + 86400

		        if (FI_CTIME(info) < reldate) {

			    call aclrc (release, SZ_LINE)
			    call strcpy (buf[ip+1], release, SZ_LINE)
			    release[strlen(release)] = NULL
			
			    call printf (
			        "IRAF %s update available on %d/%d/%d\n")
				    call pargstr (release)
				    call pargi (TM_MONTH(tm))
				    call pargi (TM_MDAY(tm))
				    call pargi (TM_YEAR(tm))
	        	    call close (fd)
			    goto done_
		        } else {
			    call printf ("Your IRAF system is up to date\n")
		        }
		    }
		}
	    }
	    call close (fd)

	} else 
	    ; 					# got a HTTP error code, quit


	# Write the current check time to the update file to reset the
	# interval.
	fd = open (chkfile, READ_WRITE, TEXT_FILE)
	call fprintf (fd, "%d\n")
	    call pargl (clktime (0))
	call close (fd)

	# Clean up.
done_ 	call mfree (reply, TY_CHAR)
	if (access (tmpfile, 0, 0) == YES) 
	    call delete (tmpfile)
end
