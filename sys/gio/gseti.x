# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GSETI -- Set any GIO parameter of type integer or real.  Precision may be
# lost if the actual parameter is of type real (call GSETR instead in such
# a case).

procedure gseti (gp, param, value)

pointer	gp			# graphics descriptor
int	param			# parameter to be set
int	value			# new value for parameter

begin
	call gsetr (gp, param, real(value))
end
