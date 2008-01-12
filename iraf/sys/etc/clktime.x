# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# CLKTIME -- Get the current clock time (local standard time) in units
# of seconds since 00:00:00 01-Jan-80.  This can be broken down into days,
# hours, seconds, etc. with BRKTIME, or printed as a date/time string with
# CNVTIME.

long procedure clktime (old_time)

long	old_time, new_time
long	cpu_time

begin
	call zgtime (new_time, cpu_time)
	return (new_time - old_time)
end
