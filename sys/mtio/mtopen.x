# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<fset.h>
include	<mach.h>
include	"mtio.h"

.help mtopen
.nf ___________________________________________________________________________
MTOPEN -- Open a magtape file, or a regular binary file if the file name is
not "mt" prefixed.  Access modes are restricted to read_only, write_only,
append, and new_tape.  The buffer size argument specifies the size of the FIO
buffer(s) used to access the tape; system dependent defaults are supplied
if the buffer size is given as zero.

The filespec specifies the tape drive, density, and file number on the tape
in a machine and device independent fashion.  The syntax is

	"mt"(drive)"."(800|1600|6250)?("["(fileno)?[,(recno)?]|"eot""]")?
e.g., 
	mta  or  mta.1600  or mta.800[3]  or  mtdraco@a.800[3], etc.

where a "fileno" of 1 refers to the first file on the tape.  If the file
number is absent, the tape is opened positioned to BOF of the "current"
file (the file to which the tape was left positioned when last closed).
The actual number of drives and their densities is machine dependent
and is defined by the device table referenced internally to ZOPNMT.

If the filespec does not have the prefix "mt", we assume that the file is
a regular binary file and try to open that.  If a tape file is specified
then the drive must be allocated before we are called.  We allocate and
initialize an MTIO file descriptor and call FOPNBF to install the magtape
device in FIO and open the device/file.
.endhelp ______________________________________________________________________

int procedure mtopen (filespec, access_mode, fio_buffer_size)

char	filespec[ARB]		# device to be opened
int	access_mode		# desired access mode (read only, etc.)
int	fio_buffer_size		# fio buffer size to be used else 0 for default

bool	first_time
pointer	sp, pkosfn
int	mt, fd, status, nskip, max_bufsize

bool	streq()
int	open(), fopnbf(), fstati()
int	mt_skip_record(), mtfile(), mt_devallocated()
extern	zopnmt(), zardmt(), zawrmt(), zawtmt(), zsttmt(), zclsmt()

errchk	open, fopnbf, fseti, syserrs, mt_parse
errchk	mt_getpos, mt_skip_record, mt_osdev
data	first_time /true/
include	"mtio.com"

begin
	call smark (sp)
	call salloc (pkosfn, SZ_FNAME, TY_CHAR)

	# Runtime initialization of the mtio file descriptor common.
	# Make each file descriptor available for use.

	if (first_time) {
	    call mt_clrcache()
	    do mt = 1, MT_MAXTAPES
		MT_OSCHAN(mt) = NULL
	    first_time = false
	}

	# If regular binary file, we are done.
	if (mtfile (filespec) == NO)
	    return (open (filespec, access_mode, BINARY_FILE))

	# Get mtio file descriptor slot, but do not allocate it until
	# we are ready to open the file.

	for (mt=1;  mt <= MT_MAXTAPES && MT_OSCHAN(mt) != NULL;  mt=mt+1)
	    ;
	if (mt > MT_MAXTAPES)
	    call syserrs (SYS_MTMULTOPEN, filespec)

	# Break mtfilespec into drive name, density, etc.
	call mt_parse (filespec, MT_DRIVE(mt), SZ_DRIVE, MT_DENSITY(mt),
	    MT_FILE(mt), MT_RECORD(mt))

	# Get OS device name from device table (and verify that device exists).
	call mt_osdev (MT_DRIVE(mt), MT_DENSITY(mt), MT_OSDEV(mt), SZ_OSDEV,
	    MT_MAXBUFSIZE(mt))

	# Verify that the device has been allocated.
	if (mt_devallocated (MT_OSDEV(mt)) == NO)
	    call syserrs (SYS_MTNOTALLOC, filespec)

	# Get current tape position.  If the tape is being opened to file zero,
	# e.g., mtX[0], set both the current position and desired position to
	# the same arbitrary file number to try to keep the kernel driver from
	# moving the tape.

	if (MT_FILE(mt) == 0) {
	    MT_OLDFILE(mt) = 1
	    MT_OLDRECORD(mt) = 1

	} else {
	    call mt_getpos (mt, MT_OLDFILE(mt), MT_OLDRECORD(mt))

	    # If tape is opened for writing but no file number is given,
	    # default to EOT.  Defaulting to current file or BOT could result
	    # in destruction of the tape.  Note that this default WILL RESULT
	    # IN TAPE RUNAWAY if used on a blank tape.  Blank tapes must be
	    # explicitly written at file [1], or opened with access mode
	    # NEW_TAPE.

	    if ((access_mode == WRITE_ONLY && MT_FILE(mt) == -1) ||
		(access_mode == APPEND)) {

		MT_FILE(mt) = EOT
		MT_RECORD(mt) = 1

	    } else if (access_mode == NEW_TAPE) {
		MT_FILE(mt) = 1
		MT_RECORD(mt) = 1
	    }
	}

	# Make sure that we are not reopening a drive which is already open.
	for (fd=1;  fd <= MT_MAXTAPES;  fd=fd+1)
	    if (fd != mt && MT_OSCHAN(fd) != NULL)
		if (streq (MT_DRIVE(fd), MT_DRIVE(mt)))
		    call syserrs (SYS_MTMULTOPEN, filespec)

	# If the file position is undefined (<0, e.g., due to an abort), rewind
	# the tape to get to a defined position.

	if (MT_OLDFILE(mt) <= 0) {
	    call strpak (MT_OSDEV(mt), Memc[pkosfn], SZ_FNAME)
	    call zzrwmt (Memc[pkosfn], status)
	    if (status == ERR)
		call syserrs (SYS_MTREW, filespec)

	    MT_OLDFILE(mt) = 1
	    MT_OLDRECORD(mt) = 1

	    # Was file opened to "current record", which is indefinite?
	    if (MT_RECORD(mt) <= 0)
		call syserrs (SYS_MTPOSINDEF, filespec)
	}

	# Initialize the remaining fields in file descriptor and open the
	# device/file.  ZOPNMT will position the tape.  Note that we pass
	# the index of the new mtio descriptor slot to ZOPNMT in the common.
	# This is poor form, but it is portable and safe, since we know
	# that FOPNBF is going to call ZOPNMT.

	switch (access_mode) {
	case READ_ONLY:
	    MT_ACMODE(mt) = READ_ONLY
	case WRITE_ONLY, APPEND, NEW_TAPE:
	    MT_ACMODE(mt) = WRITE_ONLY
	default:
	    call syserrs (SYS_MTACMODE, filespec)
	}

	MT_ATEOT(mt) = NO
	MT_ATEOF(mt) = NO
	MT_NRECORDS(mt) = MT_MAGIC		# used for verification of call
	new_mtchan = mt				# pass mt index in common

	fd = fopnbf (MT_OSDEV(mt), access_mode,
	    zopnmt, zardmt, zawrmt, zawtmt, zsttmt, zclsmt)

	if (fio_buffer_size > 0) {
	    max_bufsize = fstati (fd, F_MAXBUFSIZE)
	    if (max_bufsize > 0 && (fio_buffer_size > max_bufsize) &&
		access_mode != READ_ONLY) {
		call eprintf ("WARNING: max device record size is %d bytes\n")
		    call pargi (max_bufsize * SZB_CHAR)
	    }
	    call fseti (fd, F_BUFSIZE, fio_buffer_size)
	}
	
	# If the user specified a record offset, skip records up to there.
	# Zero means leave positioned to old record.

	if (MT_RECORD(mt) == 0)
	    MT_RECORD(mt) = MT_OLDRECORD(mt)
	if (MT_RECORD(mt) > 1) {
	    nskip = MT_RECORD(mt) - 1
	    MT_RECORD(mt) = 1
	    if (mt_skip_record (fd, nskip) != nskip)
		call syserrs (SYS_MTSKIPREC, filespec)
	}

	call mt_savepos (mt)
	return (fd)
end
