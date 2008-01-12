# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>

define	RADIX		26
define	SZ_SUFFIX	10
define	NTRIES		5
define	MAX_TRIP	5000

# MKTEMP -- Make a unique file name (used to generate temporary file names).
# Format of name is "seedNNNNcc.." where the seed is supplied by the caller,
# NNNN is (normally) the lowest four digits of the process id, and the
# characters "cc..." are the radix 26 representation of a local counter
# (maintained in static storage).  The algorithm used virtually guarantees
# a unique name on the first try.  Logical directory prefixes are allowed.

procedure mktemp (seed, temp_file, maxchars)

char	seed[ARB], temp_file[ARB], suffix[SZ_SUFFIX]
int	maxchars, counter, i, n, op, pid
int	access(), itoc()
data	counter/0/

begin
	call zgtpid (pid)				# get process id

	do i = 1, MAX_TRIP {
	    call strcpy (seed, temp_file, maxchars)
	    op = itoc (mod(pid,10000), suffix, SZ_SUFFIX)
	    call strcat (suffix, temp_file, maxchars)

	    counter = counter + 1
	    op = 1
	    for (n=counter;  n > 0;  n = (n-1) / RADIX) {
		suffix[op] = mod (n-1, RADIX) + 'a'
		op = op + 1
	    }
	    suffix[op] = EOS
	    call strcat (suffix, temp_file, maxchars)

	    if (access (temp_file,0,0) == NO)		# does file exist?
		return
	    else if (mod(i,NTRIES) == 0)
		pid = pid + mod(counter,10)		# not likely to get here
	}

	call filerr (seed, SYS_FMKTEMP)
end
