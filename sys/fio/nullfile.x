# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	<fio.h>

.help nullfile
.nf ___________________________________________________________________________
NULLFILE -- Text and binary file drivers for the nullfile, "dev$null".
These special drivers behave like regular text or binary drivers but
have the special property that no i/o occurs, i.e., all output is discarded,
making it appear as if the write was successful, and EOF is returned for
all attempts to read from the file.
.endhelp ______________________________________________________________________

define	MAX_NULLFILES		(LAST_FD-FIRST_FD+1)
define	SZ_DEFINBUF		1		# buffer size when reading
define	SZ_DEFOUTBUF		2048		# buffer size when writing

define	NU_INUSE		01B
define	NU_READ			02B
define	NU_WRITE		04B


# ZOPNNU -- Open a nullfile.  Used for both binary and text nullfiles.

procedure zopnnu (osfn, mode, chan)

char	osfn[ARB]		# osfn version of dev$null, presumably
int	mode			# not used
int	chan			# assigned channel (output)

bool	first_time
int	nu
int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count
data	first_time /true/

begin
	# First time initialization.
	if (first_time) {
	    do nu = 1, MAX_NULLFILES
		flags[nu] = 0
	    first_time = false
	}

	# Find open slot.
	for (nu=1;  nu <= MAX_NULLFILES;  nu=nu+1)
	    if (flags[nu] == 0)
		break
	if (nu > MAX_NULLFILES) {
	    chan = ERR
	    return
	}

	switch (mode) {
	case READ_ONLY:
	    flags[nu] = NU_INUSE + NU_READ
	case READ_WRITE:
	    flags[nu] = NU_INUSE + NU_READ + NU_WRITE
	default:
	    flags[nu] = NU_INUSE + NU_WRITE
	}

	count[nu] = 0
	chan = nu
end


# ZCLSNU -- Close a null file.  Used for both text and binary null files.

procedure zclsnu (chan, status)

int	chan
int	status

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] == 0)
	    status = ERR
	else {
	    flags[chan] = 0
	    status = OK
	}
end


# ZSTTNU -- Status of a null file.  Used for both text and binary null files.

procedure zsttnu (chan, param, lvalue)

int	chan
int	param
long	lvalue
int	and()

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	switch (param) {
	case FSTT_BLKSIZE:
	    lvalue = 0
	case FSTT_FILSIZE:
	    lvalue = 0
	case FSTT_OPTBUFSIZE, FSTT_MAXBUFSIZE:
	    if (and (flags[chan], NU_WRITE) != 0)
		lvalue = SZ_DEFOUTBUF
	    else
		lvalue = SZ_DEFINBUF
	}
end


# ZARDNU, ZAWRNU, ZAWTNU -- Binary file i/o to the null file.

procedure zardnu (chan, buf, maxbytes, loffset)

int	chan, maxbytes
char	buf[ARB]
long	loffset

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	count[chan] = 0
end


procedure zawrnu (chan, buf, nbytes, loffset)

int	chan, nbytes
char	buf[ARB]
long	loffset

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	count[chan] = nbytes
end


procedure zawtnu (chan, status)

int	chan, status

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    status = count[chan]
	else
	    status = ERR
end


# ZGETNU, ZPUTNU, ZFLSNU, ZSEKNU, ZNOTNU -- Text file i/o to the null file.

procedure zgetnu (chan, buf, maxch, status)

int	chan, maxch, status
char	buf[ARB]

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    status = 0
	else
	    status = ERR
end


procedure zputnu (chan, buf, nchars, status)

int	chan, nchars, status
char	buf[ARB]

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    status = nchars
	else
	    status = ERR
end


procedure zflsnu (chan, status)

int	chan
int	status

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    status = OK
	else
	    status = ERR
end


procedure zseknu (chan, loffset, status)

int	chan, status
long	loffset

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    status = OK
	else
	    status = ERR
end


procedure znotnu (chan, loffset)

int	chan
long	loffset

int	flags[MAX_NULLFILES]
int	count[MAX_NULLFILES]
common	/znucom/ flags, count

begin
	if (flags[chan] != 0)
	    loffset = 0
	else
	    loffset = ERR
end
