# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZRDM70 -- Initiate an asynchronous read from the IIS.

procedure zrdm70 (chan, buf, nbytes, offset)

int	chan			# FCB pointer for device
char	buf[ARB]		# output buffer
size_t	nbytes			# number of  bytes to read
long	offset			# not used for this device

begin
	call zardbf (chan, buf, nbytes, offset)
end
