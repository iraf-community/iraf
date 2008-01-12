# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# IMSMPS -- Sample an array of SHORT sized elements.

procedure imsmps (a, b, npix, step)

short	a[ARB], b[npix]
int	npix, step, ip, op

begin
	ip = 1
	do op = 1, npix {
	    b[op] = a[ip]
	    ip = ip + step
	}
end


# IMSMPL -- Sample an array of LONG sized elements.

procedure imsmpl (a, b, npix, step)

long	a[ARB], b[npix]
int	npix, step, ip, op

begin
	ip = 1
	do op = 1, npix {
	    b[op] = a[ip]
	    ip = ip + step
	}
end
