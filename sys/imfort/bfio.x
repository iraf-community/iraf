# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<mach.h>
include <fio.h>
include	"imfort.h"

.help bfio
.nf --------------------------------------------------------------------------
BFIO -- Binary file i/o.

The IMFORT interface needs its own little binary file i/o interface to deal
with the complexities of blocking and deblocking data in hardware disk blocks.
A little buffering is also desirable to reduce the number of disk transfers
required to read through an image.

		bfaloc (fname, nchars, status)
	fp =	bfopen (fname, acmode, advice)

		bfalcx (fname, nchars, status)
	fp =	bfopnx (fname, acmode, advice)
	nc =	bfbsiz (fp)				# get block size
	nc =	bffsiz (fp)				# get file size
	chan =  bfchan (fp)				# get channel
		bfclos (fp, status)

	stat =	bfread (fp, buf, nchars, offset)
	stat =	bfwrit (fp, buf, nchars, offset)
	stat =	bfflsh (fp)

where
	fname	host file name (no virtual filenames here)
	acmode	access mode (READ_ONLY, etc.)
	advice	SEQUENTIAL or RANDOM
	fd	file descriptor, a struct pointer
	buf	char user data buffer
	nchars	amount of data to transfer, SPP chars
	offset	file offset of transfer, SPP chars, 1 indexed
	stat	nchars transferred or ERR

The advice parameter determines the size of the internal buffer allocated
by BFIO.  A small buffer is allocated for random access, a large buffer for
sequential access.  Sequential is usually best.  If advice is a large number
it is taken to be the actual block size in chars.
.endhelp --------------------------------------------------------------------

define	LEN_BFIO	10
define	BF_CHAN		Memi[$1]		# OS channel
define	BF_ACMODE	Memi[$1+1]		# access mode
define	BF_BUFP		Memi[$1+2]		# buffer pointer
define	BF_BUFSIZE	Memi[$1+3]		# buffer length, chars
define	BF_BUFOFFSET	Memi[$1+4]		# file offset of buffer
define	BF_UPDATE	Memi[$1+5]		# write buffer to disk
define	BF_BLKSIZE	Memi[$1+6]		# device block size

define	SZ_RANBUF	512			# SPP chars
define	SZ_SEQBUF	16384
define	READ		0
define	WRITE		1


# BFOPEN -- Fortran callable version of BFOPNX.

int procedure bfopen (fname, acmode, advice)

%	character*(*) fname
int	acmode			# SPP access mode, as in FIO
int	advice			# seq. or random, or bufsize in chars

char	sppname[SZ_PATHNAME]
pointer	bfopnx()

begin
	call f77upk (fname, sppname, SZ_PATHNAME)
	return (bfopnx (sppname, acmode, advice))
end


# BFALOC -- Fortran callable version of BFALCX.

procedure bfaloc (fname, nchars, status)

%	character*(*) fname
int	nchars			# size of file to be allocated
int	status			# receives status

char	sppname[SZ_PATHNAME]

begin
	call f77upk (fname, sppname, SZ_PATHNAME)
	call strpak (fname, sppname, SZ_PATHNAME)
	call zfaloc (sppname, nchars * SZB_CHAR, status)
end


# BFOPNX -- Open a binary file (SPP version).

pointer procedure bfopnx (fname, acmode, advice)

char	fname[ARB]		# HOST filename
int	acmode			# SPP access mode, as in FIO
int	advice			# seq. or random, or bufsize in chars

pointer	bp, fp
long	blksize
char	osfn[SZ_PATHNAME]
int	chan, bufsize
int	bfmode()
errchk	malloc

begin
	# Open or create the file.
	call strpak (fname, osfn, SZ_PATHNAME)
	call zopnbf (osfn, bfmode(acmode), chan)
	if (chan == ERR)
	    return (ERR)

	# Allocate and initialize file descriptor and i/o buffer.
	call malloc (fp, LEN_BFIO, TY_STRUCT)

	# Pick a buffer size.
	if (advice == RANDOM)
	    bufsize = SZ_RANBUF
	else if (advice == SEQUENTIAL)
	    bufsize = SZ_SEQBUF
	else
	    bufsize = advice

	call zsttbf (chan, FSTT_BLKSIZE, blksize)
	blksize = blksize / SZB_CHAR
	bufsize = (bufsize + blksize - 1) / blksize * blksize
	call malloc (bp, bufsize, TY_CHAR)

	BF_CHAN(fp)      = chan
	BF_ACMODE(fp)    = acmode
	BF_BUFP(fp)      = bp
	BF_BUFSIZE(fp)   = bufsize
	BF_BUFOFFSET(fp) = 0
	BF_UPDATE(fp)    = NO
	BF_BLKSIZE(fp)   = blksize

	return (fp)
end


# BFCLOS -- Close a BFIO binary file.

procedure bfclos (fp, status)

pointer	fp			# BFIO file descriptor
int	status
int	bfflsh()

begin
	if (BF_UPDATE(fp) == YES) {
	    status = bfflsh (fp)
	    if (status == ERR)
		return
	}

	call zclsbf (BF_CHAN(fp), status)
	call mfree (BF_BUFP(fp), TY_CHAR)
	call mfree (fp, TY_STRUCT)
end


# BFALCX -- Allocate a fixed size binary file.

procedure bfalcx (fname, nchars, status)

char	fname[ARB]		# HOST filename
int	nchars			# size of file to be allocated
int	status			# receives status

char	osfn[SZ_PATHNAME]

begin
	call strpak (fname, osfn, SZ_PATHNAME)
	call zfaloc (osfn, nchars * SZB_CHAR, status)
end


# BFBSIZ -- Return the device block size in chars.

int procedure bfbsiz (fp)

pointer	fp			# BFIO file descriptor

begin
	return (BF_BLKSIZE(fp))
end


# BFFSIZ -- Return the file size in chars.

int procedure bffsiz (fp)

pointer	fp			# BFIO file descriptor
int	nbytes

begin
	call zsttbf (BF_CHAN(fp), FSTT_FILSIZE, nbytes)
	if (nbytes == ERR)
	    return (ERR)
	else
	    return ((nbytes + SZB_CHAR-1) / SZB_CHAR)
end


# BFCHAN -- Return the channel of the file.

int procedure bfchan (fp)

pointer	fp			# BFIO file descriptor

begin
	return (BF_CHAN(fp))
end


# BFREAD -- Read an arbitrary number of chars from a binary file at an
# arbitrary offset.

int procedure bfread (fp, buf, nchars, offset)

pointer	fp			# BFIO file descriptor
char	buf[ARB]		# user data buffer
int	nchars			# nchars of data to be read
long	offset			# file offset

pointer	bp
long	off, off1, off2
int	ip, op, nleft, chunk
int	bffill()

begin
	off1  = BF_BUFOFFSET(fp)
	off2  = off1 + BF_BUFSIZE(fp)
	off   = offset
	nleft = nchars
	op    = 1
	bp    = BF_BUFP(fp)

	while (nleft > 0) {
	    # Fault in new buffer if file offset falls outside current buffer.
	    if (off1 <= 0 || off < off1 || off >= off2)
		if (bffill (fp, off, nleft, READ) == ERR)
		    return (ERR)
		else {
		    off1 = BF_BUFOFFSET(fp)
		    off2 = off1 + BF_BUFSIZE(fp)
		}

	    # Return as much data as possible from the current buffer and
	    # advance all the pointers when done.

	    ip = off - off1
	    chunk = min (nleft, BF_BUFSIZE(fp) - ip)
	    call amovc (Memc[bp+ip], buf[op], chunk)

	    nleft = nleft - chunk
	    off   = off + chunk
	    op    = op + chunk
	}

	return (nchars)
end


# BFWRIT -- Write an arbitrary number of chars to a binary file at an
# arbitrary offset.

int procedure bfwrit (fp, buf, nchars, offset)

pointer	fp			# BFIO file descriptor
char	buf[ARB]		# user data buffer
int	nchars			# nchars of data to be written
long	offset			# file offset

pointer	bp
long	off, off1, off2
int	ip, op, nleft, chunk
int	bffill()

begin
	off1  = BF_BUFOFFSET(fp)
	off2  = off1 + BF_BUFSIZE(fp)
	off   = offset
	nleft = nchars
	ip    = 1
	bp    = BF_BUFP(fp)

	while (nleft > 0) {
	    # Fault in new buffer if file offset falls outside current buffer.
	    if (off1 <= 0 || off < off1 || off >= off2)
		if (bffill (fp, off, nleft, WRITE) == ERR)
		    return (ERR)
		else {
		    off1 = BF_BUFOFFSET(fp)
		    off2 = off1 + BF_BUFSIZE(fp)
		}

	    # Move as much data as possible into the current buffer and
	    # advance all the pointers when done.

	    op = off - off1
	    chunk = min (nleft, BF_BUFSIZE(fp) - op)
	    call amovc (buf[ip], Memc[bp+op], chunk)
	    BF_UPDATE(fp) = YES

	    nleft = nleft - chunk
	    off   = off + chunk
	    ip    = ip + chunk
	}

	return (nchars)
end


# BFFILL -- Move the BFIO buffer so that it contains the indicated offset.
# Flush the buffer to disk first if it has been written into.

int procedure bffill (fp, offset, nchars, rwflag)

pointer	fp			# BFIO descriptor
long	offset			# desired file offset
int	nchars			# nchars that will be read/written later
int	rwflag			# read or write when we return?

long	bufoff
int	status, bufsize
int	bfflsh()

begin
	if (BF_UPDATE(fp) == YES)
	    if (bfflsh (fp) == ERR)
		return (ERR)

	bufsize = BF_BUFSIZE(fp)
	bufoff = ((offset - 1) / bufsize) * bufsize + 1
	BF_BUFOFFSET(fp) = bufoff

	# If we are being called prior to a write, and the entire buffer
	# is being written into, there is no point in filling the buffer
	# from the file.  Also, if the file is open WRITE_ONLY, we do not
	# read from the file.

	if ((BF_ACMODE(fp) == WO) ||
	    (offset == bufoff && nchars >= bufsize && rwflag == WRITE))
	    return (nchars)

	# Fill the buffer from the file.  BFIO always reads and writes entire
	# file buffers.

	call zardbf (BF_CHAN(fp), Memc[BF_BUFP(fp)], BF_BUFSIZE(fp) * SZB_CHAR,
	    (bufoff - 1) * SZB_CHAR + 1)
	call zawtbf (BF_CHAN(fp), status)

	if (status == ERR)
	    return (ERR)
	else
	    return (status / SZB_CHAR)
end


# BFFLSH -- Flush the BFIO buffer.

int procedure bfflsh (fp)

pointer	fp				# BFIO file descriptor
int	status

begin
	if (BF_UPDATE(fp) == NO)
	    return
	else
	    BF_UPDATE(fp) = NO

	# Flush the buffer to the file.  BFIO always reads and writes entire
	# file buffers.

	call zawrbf (BF_CHAN(fp), Memc[BF_BUFP(fp)], BF_BUFSIZE(fp) * SZB_CHAR,
	    (BF_BUFOFFSET(fp) - 1) * SZB_CHAR + 1)
	call zawtbf (BF_CHAN(fp), status)

	if (status == ERR)
	    return (ERR)
	else
	    return (status / SZB_CHAR)
end


# BFMODE -- Map the IMFORT/BFIO access mode into the file access mode
# expected by the IRAF kernel.

int procedure bfmode (acmode)

int	acmode			# IMFORT access mode

begin
	switch (acmode) {
	case RO:
	    return (READ_ONLY)
	case WO:
	    return (WRITE_ONLY)
	case RW:
	    return (READ_WRITE)
	case NF:
	    return (NEW_FILE)
	default:
	    return (READ_ONLY)
	}
end
