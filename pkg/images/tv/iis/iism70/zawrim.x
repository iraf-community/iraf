# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZAWRIM -- Write data to a binary file display device.

procedure zawrim (chan, buf, nbytes, offset)

int	chan[ARB]
short	buf[ARB]
int	nbytes
long	offset

begin
	call iiswr (chan, buf, nbytes, offset)
end
