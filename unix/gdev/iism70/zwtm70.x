# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZWTM70 -- Wait for i/o completion and return the number of bytes read or
# written or ERR.  Repetitive calls return the same value.

procedure zwtm70 (chan, status)

int	chan			# FCB pointer for device
int	status			# nbytes transferred or ERR

begin
	call zawtbf (chan, status)
end
