# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	<fset.h>
include	<fio.h>

# FSTATI -- Get information on the status and characteristics of an open
# file.  Returns an integer value.  See also FSTATL and FSTATS (for long
# integer and string status values).

int procedure fstati (fd, what)

int	fd				#I file descriptor
int	what				#I parameter to be returned

pointer	ffp, pb, pbtop
int	flag, ffd, nchars, seglen, iomode
int	and(), btoi()
bool	envgetb()
long	ffilsz()
include	<fio.com>

begin
	ffp = fiodes[fd]

	switch (what) {
	case F_ASYNC:
	    return (NO)				# async i/o not implemented
	case F_BLKSIZE:
	    return (FBLKSIZE(ffp))
	case F_BUFPTR:
	    return (bufptr[fd])
	case F_BUFSIZE:
	    return (FBUFSIZE(ffp))
	case F_BUFTYPE:
	    return (F_LOCAL)			# global bufs not implemented
	case F_FILESIZE:
	    FILSIZE(ffp) = ffilsz(fd)
	    return (FILSIZE(ffp))
	case F_FIRSTBUFOFF:
	    return (FIRSTBUFOFF(ffp))
	case F_CHANNEL:
	    return (FCHAN(ffp))
	case F_CLOBBER:
	    return (btoi (envgetb ("clobber")))
	case F_CLOSEFD:
	    return (FCLOSEFD(ffp))
	case F_DEVCODE:
	    return (FDEV(ffp))
	case F_DEVICE:
	    return (zdev[FDEV(ffp)])

	case F_EOF:
	    if (FILSIZE(ffp) < 0 || LNOTE(fd) < ffilsz (fd))
		return (NO)
	    else
		return (YES)

	case F_FILEWAIT:
	    return (btoi (envgetb ("filewait")))
	case F_FIODES:
	    return (ffp)
	case F_FLUSHNL:
	    flag = FF_FLUSHNL
	case F_IOMODE:
	    iomode = 0
	    if (and (fflags[fd], FF_RAW) != 0)
		iomode = iomode + IO_RAW
	    if (and (fflags[fd], FF_NDELAY) != 0)
		iomode = iomode + IO_NDELAY
	    return (iomode)
	case F_KEEP:
	    flag = FF_KEEP

	case F_LASTREFFILE:
	    # Return FD of last active file, i.e., the file on which i/o was
	    # most recently done (or on which i/o is in progress).  FIO sets
	    # "fp" in fio.com whenever a file operation takes place.

	    for (ffd=1;  ffd <= LAST_FD;  ffd=ffd+1)
		if (fiodes[ffd] != NULL && fiodes[ffd] == fp)
		    return (ffd)
	    return (NULL)

	case F_MODE:
	    return (FMODE(ffp))

	case F_NBUFS:
	    if (bufptr[fd] == NULL)
		return (0)
	    else
		return (FNBUFS(ffp))

	case F_NCHARS:
	    return (FNCHARS(ffp))
	case F_ONEVERSION:
	    return (btoi (envgetb ("multversions")))
	case F_OPEN:
	    return (YES)
	case F_OPTBUFSIZE:
	    return (FOPTBUFSIZE(ffp))
	case F_RAW:
	    flag = FF_RAW
	case F_READ:
	    flag = FF_READ

	case F_REDIR:
	    if (redir_fd[fd] != 0)
		return (YES)
	    else
		return (NO)

	case F_SZBBLK:
	    return (FNBYTES(ffp))
	case F_TYPE:
	    return (FTYPE(ffp))
	case F_WRITE:
	    flag = FF_WRITE
	case F_MAXBUFSIZE:
	    return (FMAXBUFSIZE(ffp))

	case F_UNREAD:
	    UPDATE_IOP(fd)
	    if (iop[fd] < bufptr[fd] || iop[fd] >= itop[fd])
		nchars = 0
	    else
		nchars = itop[fd] - iop[fd]
	    if (and (FF_PUSHBACK, fflags[fd]) != 0) {
		pbtop = (FPBTOP(ffp) - 1) / SZ_INT + 1
		for (pb=FPBSP(ffp);  pb < pbtop;  pb=pb+4) {
		    seglen = Memi[pb+1] - Memi[pb]
		    if (seglen > 0)
			nchars = nchars + seglen
		}
	    }
	    return (nchars)

	default:
	    return (ERR)
	}

	if (and (flag, fflags[fd]) != 0)		# test a flag bit
	    return (YES)
	else
	    return (NO)
end
