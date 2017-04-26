# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<syserr.h>
include	<config.h>
include	<finfo.h>
include	<ctype.h>

# Override the definition of ONECASE_OUT given in <config.h>.  This is necessary
# for networking until time permits a real fix to the filename mapping code.
# The override is necessary to prevent OSFN_PKFNAME from case-converting
# filenames destined for translation on a remote node.

define	ONECASE_OUT	false

.help file_locking
.nf ___________________________________________________________________________
FILE LOCKING package.  Lock the named OSFN for exclusive write access.
The host OS file locking facilities are used if available, otherwise null
length files are used to implement advisory locks.  Read access may or may
not be excluded while a file is locked, depending on the host system.

	time =	    osfn_lock (osfn)
	nsec =	  osfn_unlock (osfn, time)
	nsec =	osfn_timeleft (osfn, time)
		osfn_initlock (osfn)
		  osfn_rmlock (osfn)

A file is locked with the OSFN_LOCK primitive, which will wait or another
process to unlock the file if it is already locked.  The lock is guaranteed
to remain in place for at least FILELOCK_PERIOD seconds.  If the file does not
exist or cannot be locked ERROR is called.  If the file is already locked but
the lock has expired osfn_lock will break the old lock and return when it has
set a new one.

A lock is removed with OSFN_UNLOCK.  The number of seconds remaining on the
lock at the time it was removed is returned as the function value; this value
should be checked to ensure that the lock was not broken due to a timeout.
ERR is returned if the file was no longer locked or had been locked by another
user when OSFN_UNLOCK was called.  OSFN_RMLOCK is used to delete all lock
files (if any) when the main file is deleted.

The primitive OSFN_TIMELEFT returns the number of seconds remaining on the
lock on file osfn.  ERR is returned if the file is no longer locked or if the
file is currently locked by another user.  OSFN_INITLOCK is called to create
the locking files initially, to avoid having to wait for a timeout when
placing the first lock.
.endhelp ____________________________________________________________________

define	FILEWAIT_PERIOD	5		# wait 5 seconds for file to unlock
define	MAX_DELAY	90		# recover from missing timelock1
define	setlock_	91


# OSFN_LOCK -- Lock the named OSFN, i.e., gain exclusive write access to a file.
# Only the process gaining the lock on a file may write to it, but there is no
# guarantee that another process may not read a locked file.  On some systems
# the file will not actually be locked until it is opened with write permission.
# If multiple files exist in a directory with the same root but different
# extensions, only one can be locked at a time.  An ERROR exit is taken if the
# file is write protected.

long procedure osfn_lock (osfn)

char	osfn[ARB]		# OS pathname of file to be locked
bool	os_has_file_locking
int	nsec, delay, status
long	fi[LEN_FINFO]
pointer	sp, lockfile, timelock1, timelock2, fname
long	clktime()
data	os_has_file_locking /OS_FILELOCKING/
errchk	syserrs

begin
	call smark (sp)
	call salloc (fname, SZ_FNAME, TY_CHAR)

	# Even if file locking is provided by the OS we must determine if the
	# file does not exist or is write protected.  If the file is not write
	# protected but cannot be opened for writing our caller will conclude
	# that the file is locked by another process.

	call zfacss (osfn, READ_WRITE, 0, status)
	if (status == ERR) {
	    call strupk (osfn, Memc[fname], SZ_FNAME)
	    call syserrs (SYS_FNOWRITEPERM, Memc[fname])
	} else if (os_has_file_locking) {
	    call sfree (sp)
	    return (clktime (long(0)))
	}

	call salloc (lockfile,  SZ_PATHNAME, TY_CHAR)
	call salloc (timelock1, SZ_PATHNAME, TY_CHAR)
	call salloc (timelock2, SZ_PATHNAME, TY_CHAR)

	# Host system does not provide file locking; we must do it ourselves
	# using null files as semaphores.  The lock files need not exist
	# when we are first called; they will be automatically generated
	# when timeout occurs.

	# Generate osfn's of the lockfile and timelock files.
	call osfn_mkfnames (osfn, Memc[lockfile], Memc[timelock1],
	    Memc[timelock2], SZ_PATHNAME)

	# The lock is set by deleteing the lockfile.  Usually the file will
	# be deleted on the first try, but if someone else has the file
	# locked or if the lockfile is missing, we have to keep trying.
	# If the lockfile cannot be deleted check that the file itself
	# exists with write permission.  We ASSUME that if we have write
	# permission on the file we are trying to lock, we also have
	# delete permission on the directory in which it is resident.

	delay = 0
	repeat {
	    # Try to set lock.
	    for (nsec=0;  nsec < FILEWAIT_PERIOD;  nsec=nsec+1) {
		call zfdele (Memc[lockfile], status)
		if (status == OK)
		    goto setlock_
	    }
	    delay = delay + nsec

	    # Timeout: if the lock is old, break it and try again to set
	    # new lock.  No need to check status because if we get here we
	    # know osfn exists and we have write+delete perm on directory.
	    # N.B.: this block is subtle; see fio$doc/vfn.hlp for a detailed
	    # discussion of timeout and recovery from lockout.

	    call zfdele (Memc[timelock1], status)
	    if (status == OK) {
		call zfinfo (Memc[timelock2], fi, status)
		if (status == ERR || clktime(FI_CTIME(fi)) >= FILELOCK_PERIOD) {
		    call zfmkcp (osfn, Memc[lockfile], status)
		    call zfdele (Memc[timelock2], status)
		    call zfmkcp (osfn, Memc[timelock2], status)
		    call zfmkcp (osfn, Memc[timelock1], status)
		} else
		    call zfmkcp (osfn, Memc[timelock1], status)
	    } else if (delay >= MAX_DELAY)
		call zfmkcp (osfn, Memc[timelock1], status)
	}

setlock_
	call zfdele (Memc[timelock2], status)
	call zfmkcp (osfn, Memc[timelock2], status)
	call zfinfo (Memc[timelock2], fi, status)

	call sfree (sp)
	return (FI_CTIME(fi))
end


# OSFN_TIMELEFT -- Determine if a file is still locked, and if so, how
# much time remains on the lock.  TIME is the time value returned when
# the file was locked.  All time values are in units of seconds.

int procedure osfn_timeleft (osfn, time)

char	osfn[ARB]		# OS pathname of file to be locked
long	time			# time when lock set

bool	os_has_file_locking
int	time_left, status, file_exists
long	fi[LEN_FINFO]
pointer	sp, lockfile, timelock1, timelock2
long	clktime()
data	os_has_file_locking /OS_FILELOCKING/

begin
	if (os_has_file_locking)
	    return (FILELOCK_PERIOD)

	call smark (sp)
	call salloc (lockfile,  SZ_PATHNAME, TY_CHAR)
	call salloc (timelock1, SZ_PATHNAME, TY_CHAR)
	call salloc (timelock2, SZ_PATHNAME, TY_CHAR)

	call osfn_mkfnames (osfn, Memc[lockfile], Memc[timelock1],
	    Memc[timelock2], SZ_PATHNAME)

	# If the lockfile exists the file is no longer locked.
	call zfacss (Memc[lockfile], 0, 0, file_exists)
	if (file_exists == YES) {
	    call sfree (sp)
	    return (ERR)
	}

	call zfinfo (Memc[timelock2], fi, status)
	call sfree (sp)

	if (status == ERR)
	    return (ERR)
	else if (FI_CTIME(fi) != time)
	    return (ERR)
	else {
	    time_left = max (0, FILELOCK_PERIOD - clktime (time))
	    return (time_left)
	}
end


# OSFN_UNLOCK -- Release the lock on a file and return the number of seconds
# that were left on the lock.  ERR is returned if the file is no longer locked
# or if the lock is not the one originally placed on the file.

int procedure osfn_unlock (osfn, time)

char	osfn[ARB]		# OS pathname of file to be locked
long	time			# time when lock set

bool	os_has_file_locking
int	time_left, status
pointer	sp, lockfile, timelock1, timelock2
int	osfn_timeleft()
data	os_has_file_locking /OS_FILELOCKING/

begin
	if (os_has_file_locking)
	    return (FILELOCK_PERIOD)

	call smark (sp)
	call salloc (lockfile,  SZ_PATHNAME, TY_CHAR)
	call salloc (timelock1, SZ_PATHNAME, TY_CHAR)
	call salloc (timelock2, SZ_PATHNAME, TY_CHAR)

	call osfn_mkfnames (osfn, Memc[lockfile], Memc[timelock1],
	    Memc[timelock2], SZ_PATHNAME)

	time_left = osfn_timeleft (osfn, time)

	if (time_left != ERR)
	    call zfmkcp (osfn, Memc[lockfile], status)

	call sfree (sp)
	return (time_left)
end


# OSFN_RMLOCK -- Remove the locks (delete all lock files) on a file.  Called
# to remove auxiliary lock files when the main file is deleted.

procedure osfn_rmlock (osfn)

char	osfn[ARB]		# OS pathname of main file
bool	os_has_file_locking
int	junk
pointer	sp, lockfile, timelock1, timelock2
data	os_has_file_locking /OS_FILELOCKING/

begin
	if (os_has_file_locking)
	    return

	call smark (sp)
	call salloc (lockfile,  SZ_PATHNAME, TY_CHAR)
	call salloc (timelock1, SZ_PATHNAME, TY_CHAR)
	call salloc (timelock2, SZ_PATHNAME, TY_CHAR)

	call osfn_mkfnames (osfn, Memc[lockfile], Memc[timelock1],
	    Memc[timelock2], SZ_PATHNAME)

	call zfdele (Memc[lockfile],  junk)
	call zfdele (Memc[timelock1], junk)
	call zfdele (Memc[timelock2], junk)

	call sfree (sp)
end


# OSFN_INITLOCK -- Create the locking files for the named file.  Should
# only be called once, generally when the file itself is created.  If we
# are not called OSFN_LOCK will create the files anyhow, but only after
# timing out, which takes a while.

procedure osfn_initlock (osfn)

char	osfn[ARB]		# OS pathname of file to be locked
bool	os_has_file_locking
int	status
pointer	sp, lockfile, timelock1, timelock2
data	os_has_file_locking /OS_FILELOCKING/

begin
	if (os_has_file_locking)
	    return

	call smark (sp)
	call salloc (lockfile,  SZ_PATHNAME, TY_CHAR)
	call salloc (timelock1, SZ_PATHNAME, TY_CHAR)
	call salloc (timelock2, SZ_PATHNAME, TY_CHAR)

	call osfn_mkfnames (osfn, Memc[lockfile], Memc[timelock1],
	    Memc[timelock2], SZ_PATHNAME)

	call zfmkcp (osfn, Memc[lockfile],  status)
	call zfmkcp (osfn, Memc[timelock1], status)
	call zfmkcp (osfn, Memc[timelock2], status)

	if (status == ERR)
	    call syserrs (SYS_FINITLOCK, osfn)

	call sfree (sp)
end


# OSFN_MKFNAMES -- Given the OSFN of the file to be locked, generate and
# return the names of the lockfile and the two timelock files.

procedure osfn_mkfnames (osfn, lockfile, timelock1, timelock2, maxch)

char	osfn[ARB]		# OS filename of file to be locked
char	lockfile[maxch]		# OSFN of locking file
char	timelock1[maxch]	# OSFN of the first timelock file
char	timelock2[maxch]	# OSFN of the second timelock file
int	maxch

char	ch
int	op, last_dot, max_chars
pointer	sp, ip, fname

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	call strupk (osfn, Memc[fname], SZ_PATHNAME)
	ip = fname
	op = 1
	last_dot = 0

	for (ch=Memc[ip];  ch != EOS;  ch=Memc[ip]) {
	    lockfile[op] = ch
	    timelock1[op] = ch
	    timelock2[op] = ch
	    if (ch == '.')
		last_dot = op
	    ip = ip + 1
	    op = op + 1
	}

	if (last_dot > 0)
	    op = last_dot
	max_chars = maxch - op + 1
	    
	call strcpy (LOCKFILE_EXTN,  lockfile[op],  max_chars)
	call osfn_pkfname (lockfile, lockfile, maxch)
	call strcpy (TIMELOCK1_EXTN, timelock1[op], max_chars)
	call osfn_pkfname (timelock1, timelock1, maxch)
	call strcpy (TIMELOCK2_EXTN, timelock2[op], max_chars)
	call osfn_pkfname (timelock2, timelock2, maxch)

	call sfree (sp)
end


# OSFN_PKFNAME -- Convert an unpacked lower case OS filename into a true host
# OS filename.  Convert to the host case if necessary and pack the string.
# Strip any backslash escapes remaining in the filename.

procedure osfn_pkfname (spp_osfn, host_osfn, maxch)

char	spp_osfn[ARB]			# unpacked, mixed or lower case OSFN
char	host_osfn[maxch]		# packed OSFN
int	maxch

int	op, i
int	ch

begin
	if (CASE_INSENSITIVE && ONECASE_OUT) {
	    switch (HOST_CASE) {
	    case 'U', 'u':
		op = 1
		do i = 1, maxch {
		    ch = spp_osfn[i]
		    if (ch == EOS)
			break
		    else if (IS_LOWER (ch))
			host_osfn[op] = TO_UPPER (ch)
		    else if (ch == '\\')
			op = op - 1
		    else
			host_osfn[op] = ch
		    op = op + 1
		}
	    default:
		op = 1
		do i = 1, maxch {
		    ch = spp_osfn[i]
		    if (ch == EOS)
			break
		    else if (IS_UPPER (ch))
			host_osfn[op] = TO_LOWER (ch)
		    else if (ch == '\\')
			op = op - 1
		    else
			host_osfn[op] = ch
		    op = op + 1
		}
	    }

	} else {
	    op = 1
	    do i = 1, maxch {
		ch = spp_osfn[i]
		if (ch == EOS)
		    break
		else if (ch == '\\')
		    op = op - 1
		else
		    host_osfn[op] = ch
		op = op + 1
	    }
	}

	host_osfn[op] = EOS
	call strpak (host_osfn, host_osfn, maxch)
end
