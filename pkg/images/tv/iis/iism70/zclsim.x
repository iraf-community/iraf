# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZCLSIM -- Close an image display frame which is addressable as
# a binary file.

procedure zclsim (chan, status)

int	chan[ARB]
int	status

begin
	call iiscls (chan, status)
end
