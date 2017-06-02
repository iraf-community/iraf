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

define	DEVICES		"|imtool|"
define	DEF_OSDEV_1	"unix:/tmp/.IMT%d"
define	DEF_OSDEV_2	"fifo:/dev/imt1i:/dev/imt1o"
define	IMTDEV		"IMTDEV"
define	DELIMCH		','

define	SZ_OSDEV	512		# device specification string
define	MAXDEV		8
define	MAXBYTES	4000		# fifo transfer size, bytes
define	MAXTRYS		50		# fifo timeout
define	DELAY		10		# fifo i/o interval, msec

define	IMTOOL		1		# IMTOOL-type display server
define	NDEVICES	1


# ZOPNGD -- Open a binary graphics device.  The format of the DEVINFO string
# is "devname:osdev" (the KI will have already taken care of any network
# node prefix by the time we are called).

procedure zopngd (devinfo, mode, chan)

char	devinfo[ARB]		#I PACKED device info string
int	mode			#I access mode
int	chan			#O receives assigned channel

bool	first_time
pointer	devname, envname
pointer	sp, info, imtdev, osdev, pkfname, ip, op
int	nchars, dev, oschan, arg1, arg2, i
int	strdic(), ctoi()
data	first_time /true/
define	err_ 91

int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	call smark (sp)
	call salloc (info, SZ_OSDEV, TY_CHAR)
	call salloc (osdev, SZ_OSDEV, TY_CHAR)
	call salloc (imtdev, SZ_OSDEV, TY_CHAR)
	call salloc (pkfname, SZ_OSDEV, TY_CHAR)
	call salloc (devname, SZ_FNAME, TY_CHAR)
	call salloc (envname, SZ_FNAME, TY_CHAR)

	if (first_time) {
	    do i = 1, MAXDEV
		gd_dev[i] = NULL
	    first_time = false
	}

	# Parse device specification.
	# -----------------------------
	call strupk (devinfo, Memc[info], SZ_OSDEV)

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

	# Get any optional integer arguments.
	if (ctoi (Memc, ip, arg1) <= 0)
	    arg1 = 0
	if (Memc[ip] == DELIMCH)
	    ip = ip + 1
	if (ctoi (Memc, ip, arg2) <= 0)
	    arg2 = 0

	# Edit device specification as necessary.
	# ------------------------------------------

	# If the generic device is IMTOOL and we have an old style OS device
	# name, convert it to the format required by the ND driver.  If the
	# OS device name is null supply the default value.  If the user has
	# "IMTDEV" defined in their host environment this overrides the value
	# passed in the argument list.

	if (dev == IMTOOL) {
	    call strpak (IMTDEV, Memc[envname], SZ_FNAME)
	    call zgtenv (Memc[envname], Memc[imtdev], SZ_OSDEV, nchars)

	    if (nchars > 0) {
		# Environment override.
		call strupk (Memc[imtdev], Memc[osdev], SZ_OSDEV)

	    } else if (Memc[osdev] == '/') {
		# Old style device name.  Convert to the form "fifo:in:out".
		call strcpy ("fifo:", Memc[imtdev], SZ_OSDEV)
		call strcat (Memc[osdev], Memc[imtdev], SZ_OSDEV)
		call strcat ("i:", Memc[imtdev], SZ_OSDEV)
		call strcat (Memc[osdev], Memc[imtdev], SZ_OSDEV)
		call strcat ("o", Memc[imtdev], SZ_OSDEV)
		call strcpy (Memc[imtdev], Memc[osdev], SZ_OSDEV)
	    }
	}

	# Allocate a slot in the GD device table for the device.  We need this
	# to vector to the correct sub-driver when an i/o function is called.

	for (chan=1;  chan <= MAXDEV;  chan=chan+1)
	    if (gd_dev[chan] == NULL)
		break
	if (chan > MAXDEV)
	    goto err_

	# Try to physically open the device.  [ADD NEW DEVICES HERE].
	switch (dev) {
	case IMTOOL:
	    if (Memc[osdev] == EOS) {
		# Supply default value.
		call strpak (DEF_OSDEV_1, Memc[pkfname], SZ_OSDEV)
		call zopnnd (Memc[pkfname], mode, oschan)
		if (oschan == ERR) {
		    call strpak (DEF_OSDEV_2, Memc[pkfname], SZ_OSDEV)
		    call zopnnd (Memc[pkfname], mode, oschan)
		}
	    } else {
		call strpak (Memc[osdev], Memc[pkfname], SZ_OSDEV)
		call zopnnd (Memc[pkfname], mode, oschan)
	    }

	default:
	    oschan = ERR
	}

	if (oschan == ERR)
	    goto err_

	gd_dev[chan]    = dev
	gd_oschan[chan] = oschan
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

int	chan			#I channel assigned device
int	status			#O receives status of close

int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	# [ADD NEW DEVICES HERE].

	if (chan < 1 || chan > MAXDEV) {
	    status = ERR
	    return
	}

	switch (gd_dev[chan]) {
	case IMTOOL:
	    call zclsnd (gd_oschan[chan], status)
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
int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	gd_status[chan] = OK

	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IMTOOL:
	    # Nothing special here, except that we can only move 4096 bytes at
	    # a time through the pipe to the display server.  Some provision
	    # for timeout is necessary in the event that the sender dies during
	    # the transfer.
	    #
	    # [we don't need all this for the ND driver, but there is still
	    # a 4096 byte limit for fifo's, so leave this in for now.]

	    nread  = 0
	    ntries = 0
	    op = 1

	    for (nleft=maxbytes;  nleft > 0;  ) {
		n = min (nleft, MAXBYTES)
		call zardnd (gd_oschan[chan], buf[op], n, offset)
		call zawtnd (gd_oschan[chan], n)
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
int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	gd_status[chan] = OK

	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
	case IMTOOL:
	    nwrote = 0
	    ntries = 0
	    ip = 1

	    for (nleft=nbytes;  nleft > 0;  ) {
		n = min (nleft, MAXBYTES)
		call zawrnd (gd_oschan[chan], buf[ip], n, offset)
		call zawtnd (gd_oschan[chan], n)
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

int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	if (gd_status[chan] == ERR) {
	    status = ERR
	    return
	}
	
	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
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

int	gd_dev[MAXDEV], gd_oschan[MAXDEV]
int	gd_status[MAXDEV], gd_arg1[MAXDEV], gd_arg2[MAXDEV]
common	/zgdcom/ gd_dev, gd_oschan, gd_status, gd_arg1, gd_arg2

begin
	# [ADD NEW DEVICES HERE].

	switch (gd_dev[chan]) {
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
