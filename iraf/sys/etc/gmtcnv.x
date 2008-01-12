# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GMTCNV.X -- GMT (Greenwich mean time) to LST (local standard time, or clock
# time) conversions.
#
#          gmt = lsttogmt (lst)                 # lst/gmt are in seconds
#          lst = gmttolst (gmt)                 # lst/gmt are in seconds



# GMTTOLST -- Convert a long integer value in GMT seconds to LST seconds.

long procedure gmttolst (gmt)

long	gmt			# GMT in seconds
int	gmtco

begin
	call zgmtco (gmtco)
	return (gmt - gmtco)
end



# LSTTOGMT -- Convert a long integer value in LST seconds to GMT seconds.

long procedure lsttogmt (lst)

long	lst			# LST in seconds
int	gmtco

begin
	call zgmtco (gmtco)
	return (lst + gmtco)
end
