# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# ZCLM70 -- Close and deallocate the IIS.

procedure zclm70 (chan, status)

int	chan
int	status

begin
	call zclsbf (chan, status)
end
