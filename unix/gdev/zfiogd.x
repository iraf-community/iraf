# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>
include	<fio.h>

# ZFIOGD -- FIO device driver for the interactive binary graphics devices.
# This code is host system dependent (at least in part) as well as node
# dependent, since the set of graphics devices available on a particular node
# can vary greatly.  The devices for which driver subroutines are linked
# into this particular version of ZFIOGD are listed below.  The individual
# driver subroutines must be named explicitly in the case statements in
# each generic driver subroutine.

define	DEVICES		"|iism70|iism75|imtool|"
define	DELIMCH		','
define	MAXDEV		8
define	MAXBYTES	4000		# fifo transfer size, bytes
define	MAXTRYS		50		# fifo timeout
define	DELAY		10		# fifo i/o interval, msec

define	IISM70		1		# IIS Model 70 image display
define	IISM75		2		# IIS Model 75 image display
define	IMTOOL		3		# Sun IMTOOL display server
define	NDEVICES	3


# ZOPNGD -- Open a binary graphics device.  The format of the DEVINFO string
# is "devname:osdev" (the KI will have already taken care of any node prefix
# by the time we are called).

procedure zopngd (devinfo, mode, chan)

char	devinfo[ARB]		# PACKED device info string
int	mode			# access mode
int	chan			# receives assigned channel

bool	first_time
pointer	sp, info, devname, osdev, fname, pkfname, ip, op
int	dev, oschan1, oschan2, arg1, arg2, i
int	strdic(), ctoi(), gstrcpy()
data	first_time /true/
define	err_ 91

int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	call smark (sp)
	call salloc (info,    SZ_PATHNAME, TY_CHAR)
	call salloc (devname, SZ_FNAME,    TY_CHAR)
	call salloc (osdev,   SZ_FNAME,    TY_CHAR)
	call salloc (fname,   SZ_FNAME,    TY_CHAR)
	call salloc (pkfname, SZ_FNAME,    TY_CHAR)

	if (first_time) {
	    do i = 1, MAXDEV
		gd_dev[i] = NULL
	    first_time = false
	}

	# We are passed a packed string by FIO.
	call strupk (devinfo, Memc[info], SZ_PATHNAME)

	# Extract generic device name.
	op = devname
	for (ip=info;  Memc[ip] != EOS && Memc[ip] != DELIMCH;  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}
	Memc[op] = EOS
	if (Memc[ip] == DELIMCH)
	    ip = ip + 1
	else
	    goto err_

	# Look up the generic device name in the device table.
	dev = strdic (Memc[devname], Memc[osdev], 0, DEVICES)
	if (dev <= 0)
	    goto err_

	# Get the OS device name.
	op = osdev
	for (;  Memc[ip] != EOS && Memc[ip] != DELIMCH;  ip=ip+1) {
	    Memc[op] = Memc[ip]
	    op = op + 1
	}
	Memc[op] = EOS
	if (Memc[ip] == DELIMCH)
	    ip = ip + 1

	# Get optional integer arguments.
	if (ctoi (Memc, ip, arg1) <= 0)
	    arg1 = 0
	if (Memc[ip] == DELIMCH)
	    ip = ip + 1
	if (ctoi (Memc, ip, arg2) <= 0)
	    arg2 = 0

	# Allocate a slot in the GD device table for the device.  We need this
	# to vector to the correct sub-driver when an i/o function is called.

	for (chan=1;  chan <= MAXDEV;  chan=chan+1)
	    if (gd_dev[chan] == NULL)
		break
	if (chan > MAXDEV)
	    goto err_

	# Try to physically open the device.  [ADD NEW DEVICES HERE].

	switch (dev) {
	case IISM70:
	    # Bidirectional i/o stream.
	    call strpak (Memc[osdev], Memc[osdev], SZ_FNAME)
	    call zopm70 (Memc[osdev], mode, oschan1)
	case IISM75:
	    # Bidirectional i/o stream.
	    call strpak (Memc[osdev], Memc[osdev], SZ_FNAME)
	    call zopm75 (Memc[osdev], mode, oschan1)

	case IMTOOL:
	    # Separate output (to device) and input (from device) streams.
	    op = fname + gstrcpy (Memc[osdev], Memc[fname], SZ_FNAME)
	    Memc[op+1] = EOS

	    Memc[op] = 'o'
	    call strpak (Memc[fname], Memc[pkfname], SZ_FNAME)
	    call zopnbf (Memc[pkfname], WRITE_ONLY, oschan1)
	    Memc[op] = 'i'
	    call strpak (Memc[fname], Memc[pkfname], SZ_FNAME)
	    call zopnbf (Memc[pkfname], READ_ONLY, oschan2)

	default:
	    oschan1 = ERR
	}

	if (oschan1 == ERR)
	    goto err_

	gd_dev[chan]    = dev
	gd_oschan1[chan] = oschan1
	gd_oschan2[chan] = oschan2
	gd_status[chan] = OK
	gd_arg1[chan] = arg1
	gd_arg2[chan] = arg2

	call sfree (sp)
	return
err_
	chan = ERR
	call sfree (sp)
end


# ZCLSGD -- Close a binary graphics device.

procedure zclsgd (chan, status)

int	chan			# channel assigned device
int	status			# receives status of close

int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	# [ADD NEW DEVICES HERE].

	if (chan < 1 || chan > MAXDEV) {
	    status = ERR
	    return
	}

	switch (gd_dev[chan]) {
	case IISM70:
	    call zclm70 (gd_oschan1[chan], status)
	case IISM75:
	    call zclm75 (gd_oschan1[chan], status)
	case IMTOOL:
	    call zclsbf (gd_oschan2[chan], status)
	    call zclsbf (gd_oschan1[chan], status)
	default:
	    status = ERR
	}

	gd_dev[chan] = NULL
end


# ZARDGD -- Read from a binary graphics device.

procedure zardgd (chan, buf, maxbytes, offset)

int	chan			# channel assigned device
char	buf[ARB]		# buffer to be filled
int	maxbytes		# max bytes to read
long	offset			# file offset (function code else zero)

int	nread, nleft, ntries, n, op
int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	gd_status[chan] = OK

	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IISM70:
	    call zrdm70 (gd_oschan1[chan], buf, maxbytes, offset)
	case IISM75:
	    call zrdm75 (gd_oschan1[chan], buf, maxbytes, offset)

	case IMTOOL:
	    # Nothing special here, except that we can only move 4096 bytes at
	    # a time through the pipe to the display server.  Some provision
	    # for timeout is necessary in the event that the sender dies during
	    # the transfer.

	    nread  = 0
	    ntries = 0
	    op = 1

	    for (nleft=maxbytes;  nleft > 0;  ) {
		n = min (nleft, MAXBYTES)
		call zardbf (gd_oschan2[chan], buf[op], n, offset)
		call zawtbf (gd_oschan2[chan], n)
		if (n < 0) {
		    nread = ERR
		    break
		}

		nread = nread + n
		op = op + n / SZB_CHAR
		nleft = nleft - n
		if (n == 0)
		    call zwmsec (DELAY)

		ntries = ntries + 1
		if (ntries > MAXTRYS) {
		    nread = ERR
		    break
		}
	    }

	    gd_status[chan] = nread

	default:
	    gd_status[chan] = ERR
	}
end


# ZAWRGD -- Write to a binary graphics device.

procedure zawrgd (chan, buf, nbytes, offset)

int	chan			# channel assigned device
char	buf[ARB]		# buffer containing the data
int	nbytes			# nbytes to be written
long	offset			# file offset (function code else zero)

int	nwrote, nleft, ntries, n, ip
int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	gd_status[chan] = OK

	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IISM70:
	    call zwrm70 (gd_oschan1[chan], buf, nbytes, offset)
	case IISM75:
	    call zwrm75 (gd_oschan1[chan], buf, nbytes, offset)

	case IMTOOL:
	    # Nothing special here, except that we can only move 4096 bytes at
	    # a time through the pipe to the display server.  Some provision 
	    # for timeout is necessary to avoid an infinite loop in the event
	    # that the receiver dies during the transmission.

	    nwrote = 0
	    ntries = 0
	    ip = 1

	    for (nleft=nbytes;  nleft > 0;  ) {
		n = min (nleft, MAXBYTES)
		call zawrbf (gd_oschan1[chan], buf[ip], n, offset)
		call zawtbf (gd_oschan1[chan], n)
		if (n < 0) {
		    nwrote = ERR
		    break
		}

		ip = ip + n / SZB_CHAR
		nwrote = nwrote + n
		nleft = nleft - n
		if (n == 0)
		    call zwmsec (DELAY)

		ntries = ntries + 1
		if (ntries > MAXTRYS) {
		    nwrote = ERR
		    break
		}
	    }

	    gd_status[chan] = nwrote

	default:
	    gd_status[chan] = ERR
	}
end


# ZAWTGD -- Wait for i/o to a binary graphics device.

procedure zawtgd (chan, status)

int	chan			# channel assigned device
int	status			# receives nbytes transferred or ERR

int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	if (gd_status[chan] == ERR) {
	    status = ERR
	    return
	}
	
	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IISM70:
	    call zwtm70 (gd_oschan1[chan], status)
	case IISM75:
	    call zwtm75 (gd_oschan1[chan], status)
	case IMTOOL:
	    status = gd_status[chan]
	default:
	    status = ERR
	}
end


# ZSTTGD -- Get the file status of a binary graphics device.

procedure zsttgd (chan, what, lvalue)

int	chan			# channel assigned device
int	what			# status parameter being queried
long	lvalue			# receives value of parameter

int	gd_dev[MAXDEV], gd_oschan1[MAXDEV], gd_oschan2[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan1, gd_oschan2, gd_status, gd_arg1, gd_arg2

begin
	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IISM70:
	    call zstm70 (gd_oschan1[chan], what, lvalue)
	case IISM75:
	    call zstm75 (gd_oschan1[chan], what, lvalue)

	case IMTOOL:
	    switch (what) {
	    case FSTT_FILSIZE:
		lvalue = gd_arg1[chan] * gd_arg2[chan] * SZB_CHAR
	    case FSTT_BLKSIZE:
		lvalue = gd_arg1[chan] * SZB_CHAR
	    case FSTT_OPTBUFSIZE:
		lvalue = gd_arg1[chan] * SZB_CHAR
	    case FSTT_MAXBUFSIZE:
		lvalue = 32768
	    default:
		lvalue = ERR
	    }
	default:
	    lvalue = ERR
	}
end
