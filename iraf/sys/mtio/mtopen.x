# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<config.h>
include	<syserr.h>
include	<fset.h>
include	<mach.h>
include	"mtio.h"

# MTOPEN -- Open a magtape file, or a regular binary file if the file name is
# not "mt" prefixed.  Access modes are restricted to read_only, write_only,
# append, and new_tape.  The buffer size argument specifies the size of the
# FIO buffer used to access the tape; a system and device dependent default
# is supplied if the buffer size is given as zero.  If the device is a fixed
# block size device the buffer size will be adjusted to an integral multiple
# of the device block size.  For variable size record devices, the buffer size
# determines the size of the tape record on a write, or the maximum record
# size on a read.
# 
# The device to be accessed is specified as follows:
# 
#	[node!] mtX [ '[' file[.record] [:attr-list] ']' ]
# 
# for example,
# 
#	mtexb1[4:nb:se@:ts=1200:so=/dev/ttya8]
# 
# The "mt" prefix is required for the object to be considered a magtape device
# reference.  The device name returned is "mtX" as shown above; there must be
# an entry for device mtX in the tapecap file in DEV.
# 
# The file and record numbers are optional.  Files and records are numbered
# starting with 1.  A sequence such as "mtX[eot]" will cause the tape to be
# positioned to end of tape.  "mtX[0]" causes the tape to be opened at the
# current position, i.e., without being moved.
# 
# The optional attr-list field consists of a sequence of colon-delimited
# tapecap fields.  These will override any values given in the tapecap entry
# for the device.  The syntax for attr-list is the same as in tapecap.
# 
# If the filespec does not have the prefix "mt", we assume that the file is
# a regular binary file and try to open that.  If a tape file is specified
# then the drive must be allocated before we are called.  We allocate and
# initialize an MTIO file descriptor and call FOPNBF to install the magtape
# device in FIO and open the device/file.

int procedure mtopen (mtname, acmode, bufsize)

char	mtname[ARB]		#I device to be opened
int	acmode			#I access mode
int	bufsize			#I fio buffer size (record size) or 0

bool	first_time
pointer	sp, devcap, fname, gty
int	mt, fd, nskip, new_file, new_record

bool	streq()
pointer	mt_gtyopen(), gtycaps()
int	open(), fopnbf(), gtygets(), access()
int	mt_skip_record(), mtfile(), mt_devallocated()
extern	zopnmt(), zardmt(), zawrmt(), zawtmt(), zsttmt(), zclsmt()

errchk	open, fopnbf, fseti, syserrs, mtparse
errchk	mt_getpos, mt_skip_record, mt_gtyopen, gtygets, mt_glock, mtallocate
data	first_time /true/
include	"mtio.com"

begin
	call smark (sp)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)
	call salloc (devcap, SZ_DEVCAP, TY_CHAR)

	# Runtime initialization of the mtio file descriptor common.
	# Make each file descriptor available for use.

	if (first_time) {
	    call mt_clrcache()
	    do mt = 1, MT_MAXTAPES
		MT_OSCHAN(mt) = NULL
	    first_time = false
	}

	# If regular binary file, we are done.
	if (mtfile(mtname) == NO) {
	    call sfree (sp)
	    return (open (mtname, acmode, BINARY_FILE))
	}

	# Get mtio file descriptor slot, but do not allocate it until
	# we are ready to open the file.

	for (mt=1;  mt <= MT_MAXTAPES && MT_OSCHAN(mt) != NULL;  mt=mt+1)
	    ;
	if (mt > MT_MAXTAPES)
	    call syserrs (SYS_MTMULTOPEN, mtname)

	# Break mtname into drive name, file and record number, etc.
	call mtparse (mtname, MT_DEVICE(mt), SZ_DEVICE, 
	    new_file, new_record, Memc[devcap], SZ_DEVCAP)
	if (new_record == ERR)
	    new_record = 1

	# Get tapecap info.
	gty = mt_gtyopen (MT_DEVICE(mt), Memc[devcap])
	MT_DEVCAP(mt) = gtycaps (gty)
	if (gtygets (gty, "dv", MT_IODEV(mt), SZ_IODEV) <= 0) {
	    call eprintf ("missing `dv' parameter in tapecap entry for %s\n")
		call pargstr (mtname)
	    call syserrs (SYS_MTTAPECAP, mtname)
	}
	call ki_xnode (MT_DEVICE(mt), MT_IODEV(mt), SZ_IODEV)

	# If the device has not been allocated, at least write out the
	# lock file.  This will not physically allocate the device, but
	# the lock file is required to be able to access the device.

	call mt_glock (mtname, MT_LKNAME(mt), SZ_LKNAME)
	if (mt_devallocated (MT_IODEV(mt)) == NO)
	    if (access (MT_LKNAME(mt), 0,0) == NO)
		call mtallocate (mtname)

	# Get current tape position.
	call mt_getpos (mtname, mt)

	MT_FILE(mt) = new_file
	MT_RECORD(mt) = new_record
	MT_ATEOF(mt) = NO

	# If tape is opened for writing but no file number is given, default
	# to EOT.  Defaulting to current file or BOT could result in
	# destruction of the tape.  Note that this default WILL RESULT IN TAPE
	# RUNAWAY if used on a blank tape.  Blank tapes must be explicitly
	# written at file [1], or opened with access mode NEW_TAPE.

	if ((acmode == WRITE_ONLY && MT_FILE(mt) == -1) || (acmode == APPEND)) {
	    MT_FILE(mt) = EOT
	    MT_RECORD(mt) = 1
	} else if (acmode == NEW_TAPE) {
	    MT_FILE(mt) = 1
	    MT_RECORD(mt) = 1
	}

	# Make sure that we are not reopening a drive which is already open.
	for (fd=1;  fd <= MT_MAXTAPES;  fd=fd+1)
	    if (fd != mt && MT_OSCHAN(fd) != NULL)
		if (streq (MT_DEVICE(fd), MT_DEVICE(mt)))
		    call syserrs (SYS_MTMULTOPEN, mtname)

	# Initialize the remaining fields in the file descriptor and open the
	# device.  ZOPNMT will position the tape.  Note that we pass the index
	# of the new mtio descriptor slot to ZOPNMT in the common.  This is a
	# bit ugly, but is safe enough, since we know that FOPNBF is going to
	# call ZOPNMT.

	switch (acmode) {
	case READ_ONLY:
	    MT_ACMODE(mt) = READ_ONLY
	case WRITE_ONLY, APPEND, NEW_TAPE:
	    MT_ACMODE(mt) = WRITE_ONLY
	default:
	    call syserrs (SYS_MTACMODE, mtname)
	}

	new_mtchan = mt
	fd = fopnbf (MT_IODEV(mt), acmode,
	    zopnmt, zardmt, zawrmt, zawtmt, zsttmt, zclsmt)

	# Set the file buffer size (record size for variable block devices).
	if (bufsize > 0)
	    call fseti (fd, F_BUFSIZE, bufsize)
	
	# If the user specified a record offset, skip records up to there.
	# Zero means leave positioned to old record.

	if (MT_RECORD(mt) == 0)
	    MT_RECORD(mt) = MT_RECNO(mt)
	if (MT_RECORD(mt) > 1) {
	    nskip = MT_RECORD(mt) - 1
	    MT_RECORD(mt) = 1
	    if (mt_skip_record (fd, nskip) != nskip)
		call syserrs (SYS_MTSKIPREC, mtname)
	}

	call mt_savepos (mt)

	call sfree (sp)
	return (fd)
end
