# Coincidence correction options
define	CC_PHOTO_MODE	1	# Photometer style correction
define	CC_IIDS_MODE	2	# IIDS style
define	CC_POWER_MODE	3	# Power law correction
define	CC_USER_MODE	4	# User supplies a function


# COINCOR -- Coincidence correction for detector deadtime

procedure coincor (input, output, npts, expo, coflag, dt, power, mode)

real	input[npts]
real	output[npts]
real	expo
int	coflag
real	dt
real	power
int	mode, npts

begin
	# Check that exposure time is legit
	if (expo <= 0.0)
	    return

	# Select the method by which the correction is performed
	switch (mode) {
	    case CC_PHOTO_MODE:
	    	# Photoelectric photometer
		call ccphoto (input, output, npts, expo, coflag, dt)

	    case CC_IIDS_MODE:
		# IIDS style correction
		if (coflag == -1) {
		    call cciids (input, output, npts, expo, coflag, dt)
		    if (power != 1.0)
		        call ccpower (output, output, npts, expo, coflag,
			    power)
		} else if ((coflag == 0) && (power != 1.0))
		    call ccpower (input, output, npts, expo, coflag, power)
		else
		    call amovr (input, output, npts)

	    case CC_USER_MODE:
		# Provided by the user
		call ccuser (input, output, npts, expo, coflag, dt)
	}
end

# CCPHOTO -- Photoelectric photometer coincidence correction

procedure ccphoto (input, output, npts, expo, coflag, dt)

real	input[npts], output[npts], expo, dt
int	coflag
int	npts

int	i

begin
	do i = 1, npts
	    output[i] = input[i] * exp (input[i] * dt / expo)
	coflag = 2
end

# CCUSER -- User supplied correction scheme

procedure ccuser (input, output, npts, expo, coflag, dt)

real	input[npts], output[npts], expo, dt
int	coflag
int	npts

begin
	coflag = 3
end

# CCIIDS -- IIDS style correction scheme
# From Instrumentation for Astronomy III (SPIE Vol 172) p.88 by Larry Goad
#
# Note that only the "Detect" mode of observation is supported.

procedure cciids (input, output, npts, expo, coflag, dt)

real	input[npts], output[npts], expo, dt
int	npts, coflag

int	i
real	tsweep, value

begin
	# Allow tsweep to be the deadtime so that a different value
	# may be entered for other instruments.
	# For the IIDS, tsweep = 1.424e-3 sec
	tsweep = dt

	do i = 1, npts {
	    value = 1 - input[i] / expo * tsweep
	    if ((value < 0.) || (value > 1.))
		output[i] = input[i]
	    else
	        output[i] = -expo * log (value)/ tsweep
	}
	coflag = 0
end

# CCPOWER -- Power law correction
# Power law correction from Massey and De Veny, NOAO Newsletter #6.

procedure ccpower (input, output, npts, expo, coflag, power)

real	input[npts], output[npts], expo, power
int	npts, coflag

int	i

begin
	do i = 1, npts
	    if (input[i] > 0.)
	        output[i] = expo * (input[i] / expo) ** power
	    else
		output[i] = input[i]
	coflag = 1
end
