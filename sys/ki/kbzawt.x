# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<config.h>
include	"ki.h"

# KB_ZAWT -- Wait for i/o to a binary file.  Called after an asynchronous read
# or write has been issued on a channel.  We are not called unless the device
# does not reside on the local node.  If the last i/o transfer was a read then
# the ZAWT packet will be followed by the data which we must copy into the
# callers buffer, using the pointer saved when the read was posted.  If the
# last i/o operation was a write then we assume that the write was successful
# and merely return the number of bytes written (error checking occurs
# elsewhere).
#
#	 read: sendpkt,			rcvpkt, readdata
#	write: sendpkt, senddata	[nothing]
#
#	       (zard|zawr)                  (zawt)
#
# Note that the ZAWT function is processed locally in the case of a write,
# rather than being passed to the kernel server.  This makes file write
# operations a pipelined operation, significantly increasing the opportunity
# for overlapped execution and increasing the data bandwidth.  If the number
# of bytes transferred to the remote device is not what is requested then the
# remote kernel server will close the entire channel down (not just the
# multiplexed channel for the device), causing a ZAWRKS error in the local
# process.
#
# NOTE -- disregard the above regarding asynchronous reads.  ZARD is currently
# implemented as a synchronous read.  The comments are retained here to show
# how to speed up reads when the i/o system is made fully asynchronous.

procedure kb_zawt (device, chan, status)

int	device			# device driver code
int	chan			# channel assigned device
int	status			# receives nbytes transferred or ERR

include	"kichan.com"

begin
	status = k_status[chan]
end
