# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<fset.h>
include	"fmset.h"
include	"fmio.h"

.help fcache
.nf -------------------------------------------------------------------------
FCACHE -- FMIO File (Buffer) Cache package.

    This package is used to manage a cache of lfile data buffers maintained on
a FMIO datafile.  The number and size of data buffers is user configurable:
the number of buffers by a call to FM_SETI to set FM_FCACHESIZE, and the buffer
size by a call to FSETI to set the file buffer size.

		  fm_fcinit (fm, cachesize)
                 fm_fcdebug (fm, out, what)
		  fm_fcsync (fm)
		  fm_fcfree (fm)

              fd = fm_getfd (fm, lfile, mode, type)
                   fm_retfd (fm, lfile)
                 fm_lockout (fm, lfile)
                  fm_unlock (fm, lfile)
	   bool = fm_locked (fm, lfile)

The cache is a conventional LRU cache.  FM_GETFD returns the file descriptor
of an open, cached file opened on an lfile, locking the file in the cache in
the process.  A subsequent call to FM_RETFD is required to allow the cache
slot to be reused.  FM_LOCKOUT may be used to lock a particular lfile out of
the cache; FM_UNLOCK releases the lock.
.endhelp --------------------------------------------------------------------

define	LEN_FCACHE	(3+($1)*LEN_FSLOT)	# len = LEN_FCACHE(cachesize)
define	FC_NFILES	Memi[$1]		# number of files in cache
define	FC_REFCNT	Memi[$1+1]		# cache reference count
define	FC_LFSTAT	Memi[$1+2]		# lfile statistics array
define	FC_FS		((($2)-1)*LEN_FSLOT+($1)+3)	# get slot pointer

define	LEN_FSLOT	4
define	FC_NREF		Memi[$1]		# number of refs to this slot
define	FC_LRU		Memi[$1+1]		# LRU count for slot
define	FC_LFILE	Memi[$1+2]		# lfile assigned to slot
define	FC_FD		Memi[$1+3]		# file descriptor


# FM_GETFD -- Map a FIO file descriptor onto an lfile and return the file
# descriptor.  The opened file descriptor remains in the cache after the
# access, until the file ages out of the cache.  

int procedure fm_getfd (fm, lfile, mode, type)

pointer	fm		# FMIO descriptor
int	lfile		# lfile to be opened
int	mode		# file access mode
int	type		# file type

int	acmode, lru, i
pointer	oldest, fc, fs, st

bool	fm_locked()
pointer	fm_findlf()
int	fm_fopen(), fstati()
errchk	fm_fopen, fm_fcinit, syserrs, close
define	ref_	91

begin
	fc = FM_FCACHE(fm)
	if (fc == NULL) {
	    call fm_fcinit (fm, FM_SZFCACHE(fm))
	    fc = FM_FCACHE(fm)
	}

	# Keep debug statistics on lfile accesses.
	st = FC_LFSTAT(fc)
	if (st != NULL)
	    Mems[st+lfile] = Mems[st+lfile] + 1

	fs = fm_findlf (fc, lfile)
	if (fs != NULL) {
	    # If lfile is already in the cache and the new mode is NEW_FILE,
	    # or we need write perm and do not currently have it, close and
	    # reopen the lfile with the desired mode.

	    if (mode == NEW_FILE ||
		(mode != READ_ONLY && fstati (FC_FD(fs), F_WRITE) == NO)) {
ref_
		if (FC_NREF(fs) > 1)
		    call syserrs (SYS_FMFSINUSE, FM_DFNAME(fm))
		if (FC_NREF(fs) == 1)
		    call close (FC_FD(fs))
		if (fm_locked (fm, lfile))
		    call syserrs (SYS_FMLFLOCKED, FM_DFNAME(fm))

		if (mode == WRITE_ONLY || mode == APPEND)
		    acmode = READ_WRITE
		else
		    acmode = mode

		FC_FD(fs) = fm_fopen (fm, lfile, acmode, type)
		FC_NREF(fs) = 1
		FC_LFILE(fs) = lfile
	    }

	    # Reference the cached lfile.
	    FC_REFCNT(fc) = FC_REFCNT(fc) + 1
	    FC_LRU(fs)    = FC_REFCNT(fc)
	    FC_NREF(fs)   = FC_NREF(fs) + 1
	    if (mode == APPEND)
		call seek (FC_FD(fs), EOFL)

	    return (FC_FD(fs))

	} else {
	    # Lfile is not in cache.  Find the oldest slot. 
	    oldest = NULL
	    lru = NULL
	    do i = 1, FC_NFILES(fc) {
		fs = FC_FS(fc,i)
		if (FC_NREF(fs) <= 1)
		    if (lru == NULL || FC_LRU(fs) < lru) {
			lru = FC_LRU(fs)
			oldest = fs
			if (FC_NREF(fs) <= 0)
			    break
		    }
	    }

	    # Abort if all cache slots are busy.
	    if (oldest == NULL)
		call syserrs (SYS_FMFCFULL, FM_DFNAME(fm))

	    # Replace the file in the cache, and return new descriptor.
	    fs = oldest
	    goto ref_
	}
end


# FM_RETFD -- Return a cached file, i.e., decrement the reference count for
# the file so that it may be returned.  If FM_RETFD is called when the file
# is sitting in the cache idle, the file is physically closed (i.e., a GETFD
# followed by one RETFD leaves the file cached and idle, ready for another
# GETFD without losing context, but another RETFD while the file is idle
# closes the file and removes it from the cache).

procedure fm_retfd (fm, lfile)

pointer	fm			#I FMIO descriptor
int	lfile			#I lfile to be returned

pointer	fc, fs
pointer	fm_findlf()
errchk	fm_fcinit

begin
	fc = FM_FCACHE(fm)
	if (fc == NULL) {
	    call fm_fcinit (fm, FM_SZFCACHE(fm))
	    fc = FM_FCACHE(fm)
	}

	fs = fm_findlf (fc, lfile)
	if (fs != NULL) {
	    FC_NREF(fs) = FC_NREF(fs) - 1
	    if (FC_NREF(fs) <= 0) {
		call close (FC_FD(fs))
		FC_NREF(fs) = 0
	    }
	}
end


# FM_LOCKOUT -- Lock an lfile out of the file cache.

procedure fm_lockout (fm, lfile)

pointer	fm		#I FMIO descriptor
int	lfile		#I lfile to be locked out

pointer	fc, fs, lf
pointer	fm_findlf()
errchk	fm_fcinit, close, syserrs

begin
	fc = FM_FCACHE(fm)
	if (fc == NULL) {
	    call fm_fcinit (fm, FM_SZFCACHE(fm))
	    fc = FM_FCACHE(fm)
	}

	# Close lfile if it is already in the cache, but idle.
	fs = fm_findlf (fc, lfile)
	if (fs != NULL) {
	    if (FC_NREF(fs) > 1)
		call syserrs (SYS_FMLOKACTLF, FM_DFNAME(fm))
	    FC_NREF(fs) = FC_NREF(fs) - 1
	    if (FC_NREF(fs) <= 0) {
		call close (FC_FD(fs))
		FC_NREF(fs) = 0
	    }
	}

	# Set the lockout bit in the file table.
	lf = FM_FTABLE(fm) + lfile * LEN_FTE
	LF_FLAGS(lf) = or (LF_FLAGS(lf), LFF_LOCKOUT)
end


# FM_UNLOCK -- Unlock an lfile.

procedure fm_unlock (fm, lfile)

pointer	fm		#I FMIO descriptor
int	lfile		#I lfile to be locked out

pointer	lf

begin
	# Clear the lockout bit in the file table.
	lf = FM_FTABLE(fm) + lfile * LEN_FTE
	LF_FLAGS(lf) = and (LF_FLAGS(lf), not(LFF_LOCKOUT))
end


# FM_LOCKED -- Test the lfile lockout bit.

bool procedure fm_locked (fm, lfile)

pointer	fm		#I FMIO descriptor
int	lfile		#I lfile to be locked out

pointer	lf

begin
	# Test the lockout bit in the file table.
	lf = FM_FTABLE(fm) + lfile * LEN_FTE
	return (and (LF_FLAGS(lf), LFF_LOCKOUT) != 0)
end


# FM_FINDLF -- Search the cache for the given lfile.

pointer procedure fm_findlf (fc, lfile)

pointer	fc		#I file cache descriptor
int	lfile		#I lfile to search for

int	i
pointer	fs

begin
	do i = 1, FC_NFILES(fc) {
	    fs = FC_FS(fc,i)
	    if (FC_NREF(fs) != 0 && FC_LFILE(fs) == lfile)
		return (fs)
	}

	return (NULL)
end


# FM_FCDEBUG -- Print debug info describing the contents and status of the
# file cache.

procedure fm_fcdebug (fm, out, what)

pointer	fm		#I FMIO descriptor
int	out		#I output file
int	what		#I type of debug output

int	nref, i
pointer	fc, fs, st
errchk	fm_fcinit

begin
	fc = FM_FCACHE(fm)
	if (fc == NULL) {
	    call fm_fcinit (fm, FM_SZFCACHE(fm))
	    fc = FM_FCACHE(fm)
	}

	# Print cache status.
	if (and (what, FCD_CACHE) != 0) {
	    call fprintf (out, "#  LRU NREF LFILE  FD\n")
	    do i = 1, FC_NFILES(fc) {
		fs = FC_FS(fc,i)
		call fprintf (out, "%6d %4d %5d %3d\n")
		    call pargi (FC_LRU(fs))
		    call pargi (FC_NREF(fs))
		    if (FC_NREF(fs) > 0) {
			call pargi (FC_LFILE(fs))
			call pargi (FC_FD(fs))
		    } else {
			call pargi (0)
			call pargi (0)
		    }
	    }
	}

	# Print lfile access statistics.
	if (and (what, FCD_LFSTATISTICS) != 0) {
	    st = FC_LFSTAT(fc)
	    if (st != NULL) {
		call fprintf (out, "-------- getfd's per lfile ---------\n")
		do i = 0, FM_NLFILES(fm) {
		    nref = Mems[st+i]
		    if (nref > 0) {
			call fprintf (out, "%4d %4d\n")
			    call pargi (i)
			    call pargi (nref)
		    }
		}
	    }
	}
end


# FM_FCINIT -- Init the file cache.

procedure fm_fcinit (fm, cachesize)

pointer	fm		#I FMIO descriptor
int	cachesize	#I size of cache, file slots

pointer	fc

begin
	if (FM_FCACHE(fm) != NULL)
	    call fm_fcfree (fm)

	call calloc (fc, LEN_FCACHE(cachesize), TY_STRUCT)
	call calloc (FC_LFSTAT(fc), FM_NLFILES(fm)+1, TY_SHORT)
	FC_NFILES(fc) = cachesize
	FC_REFCNT(fc) = 1

	FM_FCACHE(fm) = fc
end


# FM_FCFREE -- Shutdown the file cache and free all resources, ignoring
# all file reference counts.

procedure fm_fcfree (fm)

pointer	fm		#I FMIO descriptor

int	i
pointer	fc, fs

begin
	fc = FM_FCACHE(fm)
	if (fc == NULL)
	    return

	# Update the datafile.
	call fm_fcsync (fm)

	# Close any cached files.
	do i = 1, FC_NFILES(fc) {
	    fs = FC_FS(fc,i)
	    if (FC_NREF(fs) >= 1)
		iferr (call close (FC_FD(fs)))
		    call erract (EA_WARN)
	}

	call mfree (FC_LFSTAT(fc), TY_SHORT)
	call mfree (FM_FCACHE(fm), TY_STRUCT)
end


# FM_FCSYNC -- Sync the file cache and datafile.

procedure fm_fcsync (fm)

pointer	fm		#I FMIO descriptor

int	i
pointer	fc, fs

begin
	fc = FM_FCACHE(fm)
	if (fc != NULL) {
	    do i = 1, FC_NFILES(fc) {
		fs = FC_FS(fc,i)
		if (FC_NREF(fs) >= 1)
		    iferr (call flush (FC_FD(fs)))
			call erract (EA_WARN)
	    }
	}

	call fm_sync (fm)
end
