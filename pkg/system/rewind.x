# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# REWIND -- Rewind a (previously allocated) magtape device.

procedure t_rewind()

char	device[SZ_FNAME]
bool	clgetb()
int	btoi()

begin
	call clgstr ("device", device, SZ_FNAME)
	call mtrewind (device, btoi(clgetb("initcache")))
end
