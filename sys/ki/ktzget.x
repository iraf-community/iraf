# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<config.h>
include	"ki.h"

# KT_ZGET -- Get all or part of a line of text from a text device.  We are
# called only if the device does not reside on the local node.

procedure kt_zget (device, chan, obuf, maxch, status)

int	device			# device driver code
int	chan			# channel assigned device
char	obuf[maxch]		# receives text
int	maxch			# max chars to read
int	status			# receives nchars read or ERR

pointer	bd, bp, ip, rp
int	server, nchars, nleft, reclen
int	ki_sendrcv()
long	ki_decode()
include	"kichan.com"
include	"kii.com"

begin
	bd = k_bufp[chan]

	# Text file input is buffered; each input buffer returned by the kernel
	# server may contain any number of lines of text.  Each line of text
	# is preceded by a record header containing the record length and seek
	# offset of the line.

	if (maxch == 1 || B_RP(bd) >= B_ITOP(bd)) {
	    # Refill buffer.

	    server   = k_node[chan]
	    p_arg[1] = k_oschan[chan]

	    # Read only one character if this is a raw mode read.
	    if (maxch == 1)
		p_arg[2] = 1
	    else
		p_arg[2] = SZ_TXBUF

	    if (ki_sendrcv (server, device, TX_GET) == ERR)
		status = ERR
	    else if (p_arg[1] <= 0)
		status = p_arg[1]			# EOF or ERR
	    else {
		nchars = p_arg[1]
		bp = B_BUFPTR(bd)

		# If the record is small it is returned in the string buffer,
		# else it is returned as a second record.  Each line is
		# contained entirely in a single buffer.

		if (nchars <= SZ_SBUF)
		    call amovc (p_sbuf, Memc[bp], nchars)
		else {
		    call ks_aread (server, Memc[bp], nchars)
		    call ks_await (server, status)

		    if (status != nchars) {
			call ki_error (server)
			B_ITOP(bd) = bp
			B_OTOP(bd) = bp
			status = ERR
			return
		    }

		    call chrupk (Memc[bp], 1, Memc[bp], 1, nchars)
		}

		B_RP(bd)   = bp
		B_ITOP(bd) = bp + nchars
		B_OTOP(bd) = bp
		B_CI(bd)   = 0
	    }
	}

	# Return characters from the current record until it is exhausted.
	# When the current record is empty, leave the record pointer pointing
	# to the start of the next record and the character index pointing
	# to the first char of that record.

	rp     = B_RP(bd)
	ip     = R_DATA(rp) + B_CI(bd)
	reclen = ki_decode (R_RECLEN(rp), NCHARS_INT)
	nleft  = R_GETNCHARS (reclen) - B_CI(bd)

	if (maxch >= nleft) {
	    # Return the remainder of the buffer.

	    call amovc (Memc[ip], obuf, nleft)
	    status = nleft
	    B_RP(bd) = B_RP(bd) + reclen
	    B_CI(bd) = 0

	} else {
	    # Return a portion of the data remaining in the buffer.

	    call amovc (Memc[ip], obuf, maxch)
	    status = maxch
	    B_CI(bd) = B_CI(bd) + maxch
	}
end
