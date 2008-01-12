# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# TSLEEP -- Suspend execution of the calling task for the specified number
# of seconds.

procedure tsleep (seconds)

int	seconds

begin
	if (seconds > 0)
	    call zwmsec (seconds * 1000)
end
