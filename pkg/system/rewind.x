# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# REWIND -- Rewind a (previously allocated) magtape device.

procedure t_rewind()

char	device[SZ_FNAME]

begin
	call clgstr ("device", device, SZ_FNAME)
	call mtrewind (device)
end
