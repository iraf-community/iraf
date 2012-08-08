# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZWRM70 -- Initiate an asynchronous write to the IIS.

procedure zwrm70 (chan, buf, nbytes, offset)

int	chan			# FCB pointer for device
char	buf[ARB]		# input buffer
int	nbytes			# number of  bytes to write
long	offset			# not used for this device

begin
	call zawrbf (chan, buf, nbytes, offset)
end
