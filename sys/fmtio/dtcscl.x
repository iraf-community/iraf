# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# DTCSCL -- Scales a double precision real, maintaining maximum precision.
# Called by DTOC and CTOD.

procedure dtcscl (v, e, sense)

double	v		# value to be scaled
int	e		# exponent
int	sense		# sense of scaling (0=apply e to v;  1=calc e)

begin
	if (sense == 0) 		# scale v by 10 ** e
	    v = v * (10.0d0 ** e)

	else {				# scale number to 0.1 <= v < 1.0
	    if (v == 0.0d0)
		e = 0
	    else {
	        e = -1
	        while (v < 0.1d0) {
		    v = v * 10.0d0
		    e = e - 1
		    if (v == 0.0d0) {	# check for underflow to zero
			e = 0
			break
		    }
	        }
	        while (v >= 1.0d0) {
		    v = v / 10.0d0
		    e = e + 1
	        }
	    }
	}
end
