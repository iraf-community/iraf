# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<knet.h>
include	<mach.h>

define	LP_INACTIVE	0
define	LP_READ		1
define	LP_WRITE	2
define	(LPCOM,	common /lprcom/ lp_type, lp_nbytes)

# LPOPEN -- Open the line printer device as a text or binary file.  If opened
# as a text file, we arrange for the chars to be packed upon output, but in all
# cases the printer device appears to be a streaming binary file to FIO.
# If the printer device is opened as a binary file, the data stream is passed
# directly on to the device without modification.  To simplify things a little
# we permit only one printer to be open at a time; this restriction can easily
# be removed should it prove desirable.

int procedure lpopen (device, mode, type)

char	device[ARB]
int	mode, type

int	fd
int	lp_type, lp_nbytes
bool	streq()
int	fopnbf()
extern	zopnlp(), lp_zaread(), lp_zawrite(), lp_zawait(), zsttlp(), zclslp()
LPCOM

begin
	# The TEXT device is special; it has a termcap entry and is used to
	# format text for an ASCII textfile rather than a printer.

	if (streq (device, "text"))
	    fd = STDOUT
	else {
	    lp_type   = type
	    lp_nbytes = ERR
	    fd = fopnbf (device, mode,
		zopnlp, lp_zaread, lp_zawrite, lp_zawait, zsttlp, zclslp)
	}

	return (fd)
end


# LP_ZAREAD -- FIO z-aread routine for the line printer device.  FIO calls
# us with the size of the buffer in bytes.  If the printer is opened as a
# text file, we read a factor of SZB_CHAR less than that from the lowest
# level, then unpack the data inplace in the FIO buffer.

procedure lp_zaread (chan, buf, maxbytes, offset)

int	chan
char	buf[ARB]
int	maxbytes
long	offset			# ignore, since lp is streaming device

int	nbytes
int	lp_type, lp_nbytes
LPCOM

begin
	nbytes = maxbytes
	if (lp_type == TEXT_FILE)
	    nbytes = nbytes / SZB_CHAR

	call zardlp (chan, buf, nbytes, offset)
	call zawtlp (chan, lp_nbytes)

	if (lp_nbytes > 0 && lp_type == TEXT_FILE)
	    call chrupk (buf, 1, buf, 1, lp_nbytes)
end


# LP_ZAWRITE -- FIO z-awrite routine for the line printer device.  FIO calls
# us with the size of the buffer in bytes.  If the printer is opened as a
# text file, we first pack the data inplace in the FIO buffer, then write it
# out to the device.  It is ok to modify the data directly in the FIO buffer
# since the device is a streaming device (no seeks).

procedure lp_zawrite (chan, buf, nbytes, offset)

int	chan
char	buf[ARB]
int	nbytes
long	offset			# ignore, since lp is streaming device

int	nbytes_to_write
int	lp_type, lp_nbytes
LPCOM

begin
	nbytes_to_write = nbytes
	if (lp_type == TEXT_FILE) {
	    nbytes_to_write = nbytes_to_write / SZB_CHAR
	    call chrpak (buf, 1, buf, 1, nbytes_to_write)
	}

	call zawrlp (chan, buf, nbytes_to_write, offset)
	call zawtlp (chan, lp_nbytes)
end


# LP_ZAWAIT -- Wait for i/o to the line printer to complete.  We do not bother
# with truely asynchronous i/o for line printer devices.

procedure lp_zawait (chan, nbytes)

int	chan
int	nbytes
int	lp_type, lp_nbytes
LPCOM

begin
	nbytes = lp_nbytes
end
