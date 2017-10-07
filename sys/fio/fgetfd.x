# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <config.h>
include <fio.h>

# FGETFD -- Allocate a file descriptor.  Called by all OPEN routines.
# Search static part of file descriptor storage for an open file descriptor.
# Allocate memory for rest of file descriptor, initialize all fields.

int procedure fgetfd (filename, mode, type)

char    filename[ARB]           # name of file to be assigned a descriptor
int     mode                    # access mode
int     type                    # file type

int     fd
int     fsetfd()
include	<fio.com>

begin
        for (fd=FIRST_FD;  fd <= LAST_FD && fiodes[fd] != NULL;  fd=fd+1)
            ;
        if (fd > LAST_FD)                               # out of descriptors
            call syserr (SYS_FTOOMANYFILES)

        return (fsetfd (fd, filename, mode, type))
end


# FSETFD -- Initialize the file descriptor FD.

int procedure fsetfd (fd, filename, mode, type)

int     fd                      # fd to be initialized
char    filename[ARB]           # name of file to be assigned to FD
int     mode                    # access mode
int     type                    # file type

int	or()
errchk  calloc, filerr, syserr
include <fio.com>
include "mmap.inc"

begin
        # Allocate descriptor.
        call calloc (fp, LEN_FIODES, TY_STRUCT)

        iop[fd]      = NULL
        itop[fd]     = NULL
        otop[fd]     = NULL
        bufptr[fd]   = NULL
        buftop[fd]   = NULL
        boffset[fd]  = 1
        redir_fd[fd] = NULL
        fflags[fd]   = 0
        fiodes[fd]   = fp                               # set ptr to fildes
        FCD(fp)      = FLCD(fp)                         # set ptr to chandes

	# Set the file permission bits for the given mode.  Note that read
	# permission is required in append mode on a binary file since the
	# partial block at the end of the file has to be read in before we
	# can append to it.

        switch (mode) {
        case STRING_FILE, SPOOL_FILE:
            # (neither read or write perm, disable flushnl)
        case READ_ONLY:
            fflags[fd] = FF_READ
            FILSIZE(fp) = -1                            # file size unknown
        case WRITE_ONLY:
            fflags[fd] = FF_WRITE
            FILSIZE(fp) = -1                            # file size unknown
        case READ_WRITE, APPEND:
            fflags[fd] = FF_READ + FF_WRITE
            FILSIZE(fp) = -1
        case NEW_FILE, TEMP_FILE:
	    if (type == STATIC_FILE) {
		fiodes[fd] = NULL
		call mfree (fp, TY_STRUCT)
		call filerr (filename, SYS_FSFOPNF)
	    }
            fflags[fd] = FF_READ + FF_WRITE
            FILSIZE(fp) = 0                             # zero length file
        default:
	    fiodes[fd] = NULL
	    call mfree (fp, TY_STRUCT)
            call filerr (filename, SYS_FILLEGMODE)
        }

        switch (type) {
        case STRING_FILE, SPOOL_FILE:
            # Allocate an (improper) device for string "files".  Since there
            # is no channel for a string file, any improper i/o on a string
            # file will result in an error return.

            FDEV(fp) = TX_DRIVER

	    # Spool files have all read and write permissions turned off
	    # so that they never try to write to a device driver - the file
	    # consists of only the buffered data.  Spool files are considered
	    # to be streaming binary files, so also set the blk size to 0.

	    if (type == SPOOL_FILE) {
		fflags[fd] = 0
		FBLKSIZE(fp) = 0
	    }

        case TEXT_FILE:
            fflags[fd] = or (FF_FLUSH, fflags[fd])
            FDEV(fp) = TX_DRIVER
        case BINARY_FILE:
            FDEV(fp) = BF_DRIVER
        case STATIC_FILE:
            FDEV(fp) = SF_DRIVER
        default:
	    fiodes[fd] = NULL
	    call mfree (fp, TY_STRUCT)
            call filerr (filename, SYS_FILLEGTYPE)
        }

	# A static file is equivalent to a binary file at the VOS level.
	if (mode != STRING_FILE)
            FMODE(fp) = mmap[mode]
	else
	    FMODE(fp) = STRING_FILE

	if (type == STATIC_FILE)
	    FTYPE(fp) = BINARY_FILE
	else
	    FTYPE(fp) = type

        FCHAN(fp) = -1
        FNBUFS(fp) = 1
        FREFCNT(fp) = 1                                 # no. fd active on chan
        call strcpy (filename, FNAME(fp), SZ_FFNAME)

        return (fd)
end
