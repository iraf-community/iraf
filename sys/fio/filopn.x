# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<ctype.h>
include	<syserr.h>
include	<config.h>
include	<fio.h>
include	<fset.h>

# FILOPN -- Open a file on an installed device.  A file descriptor is
# allocated and initialized.  If a new file is being opened, file clobber
# (overwrite) checking is performed.  If the file exists but cannot be
# accessed because it is open by another process, and file waiting is
# enabled (usually in batch mode), the process is blocked until the file
# becomes available.  If one attempts to "open" one of the files "STDIN",
# "STDOUT", "STDERR", etc.  the fd of the appropriate standard file is returned.
# Interrupts are disabled while the VFN database is open to protect the
# database, ensure that that the lock on the mapping file is cleared, and to
# ensure that the mapping file is closed.

int procedure filopn (fname, mode, type, zopen_proc, device)

char	fname[ARB]			# virtual file name
int	mode				# access mode (ro,rw,apnd,newf,temp)
int	type				# text or binary file
extern	zopen_proc(), device()

pointer	vp
bool	standard_device
int	ip, fd, dev_epa, junk, status, vfnmode

pointer	vfnopen()
int	fgetfd(), fstdfile(), vfnadd(), vfnmap(), locpr()
errchk	fwtacc, seek, fclobber, fgetfd
include	<fio.com>
define	cleanup_ 91
define	close_ 92
define	abort_ 93

begin
	for (ip=1;  IS_WHITE (fname[ip]);  ip=ip+1)
	    ;

	# Do not bother to check access mode if reopening a standard
	# stream.  If one attempts to write to STDIN or read from STDOUT,
	# a suitable error message will be generated at that time.  If
	# a standard file such as STDIN is reopened read-write but never
	# written to, however, that is acceptable.

	if (fstdfile (fname[ip], fd) == YES)		# standard stream?
	    return (fd)

	# Determine if "device" is a standard disk device, i.e., the driver
	# TX or BF or the static file driver SF.  Clobber and filewait are
	# only performed for disk files.

	dev_epa = locpr (device)
	standard_device = (dev_epa == zdev[TX_DRIVER] ||
			   dev_epa == zdev[BF_DRIVER] ||
			   dev_epa == zdev[SF_DRIVER])

	# Perform clobber checking and waiting only for standard devices.
	# If clobber is enabled and we are opening a new file, any existing
	# file will be deleted.

	if (standard_device && (mode == NEW_FILE || mode == TEMP_FILE))
	    call fclobber (fname[ip])

	# Allocate and initialize the file descriptor.
	fd = fgetfd (fname[ip], mode, type)
	call fseti (fd, F_DEVICE, dev_epa)
	fp = fiodes[fd]

	# Get OS pathname of file.
	if (standard_device) {

	    # Don't open VFN with write perm if file is readonly, else
	    # lockout may occur on the mapping file.

	    if (FMODE(fp) == READ_ONLY)
		vfnmode = VFN_READ
	    else
		vfnmode = VFN_WRITE

	    call intr_disable()
	    iferr (vp = vfnopen (fname[ip], vfnmode))
		goto abort_

	    if (FMODE(fp) == NEW_FILE) {
		iferr (junk = vfnadd (vp, FPKOSFN(fp), SZ_FFNAME))
		    goto close_
	    } else {
		iferr (status = vfnmap (vp, FPKOSFN(fp), SZ_FFNAME))
		    goto close_
		if (status == ERR)
		    iferr (call syserrs (SYS_FOPEN, fname[ip]))
			goto close_
	    }

	    iferr (call vfnclose (vp, VFN_UPDATE))
		goto abort_
	    call intr_enable()

	} else
	    call strpak (fname[ip], FPKOSFN(fp), SZ_FFNAME)

	# Open file.  If file exists on a standard device but cannot be
	# accessed and "filewait" is enabled, wait for file to become
	# accessible.

	repeat {
	    call zopen_proc (FPKOSFN(fp), FMODE(fp), FCHAN(fp))
	    FDEVOPEN(fp) = locpr (zopen_proc)

	    fp = fiodes[fd]
	    if (FCHAN(fp) == ERR) {
		iferr {
		    if (standard_device) {
			iferr (call fwtacc (fd, fname[ip]))
			    call syserrs (SYS_FOPEN, fname[ip])
		    } else {
			call syserrs (SYS_FOPENDEV, fname[ip])
		    }
		} then
		    goto cleanup_
	    } else
		break
	}

	# Get the device parameters (block size, file size, streamer, etc.)
	iferr (call fgdev_param (fd))
	    goto cleanup_

	iferr {
	    if (mode == APPEND)
		call seek (fd, EOFL)
	    else
		call seek (fd, BOFL)

	    # Save name of temporary file for automatic deletion at program
	    # termination.
	    if (mode == TEMP_FILE)
		call fsvtfn (fname)
	} then
	    goto cleanup_

	return (fd)

cleanup_
	call frtnfd (fd)
	call erract (EA_ERROR)
	return (ERR)


	# Error recovery nasties for when the VFN is open.
close_
	iferr (call vfnclose (vp, VFN_NOUPDATE))
	    ;
abort_
	call frtnfd (fd)
	call intr_enable()
	call erract (EA_ERROR)
	return (ERR)
end
