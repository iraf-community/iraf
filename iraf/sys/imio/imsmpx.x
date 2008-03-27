# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.


# IMSMPS -- Sample an array of SHORT sized elements.

procedure imsmps (a, b, npix, step)

short	a[ARB], b[npix]
size_t	npix
long	step

long	ip, op

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
size_t	npix
long	step

long	ip, op

begin
	ip = 1
	do op = 1, npix {
	    b[op] = a[ip]
	    ip = ip + step
	}
end


# IMSMPI -- Sample an array of INT sized elements.

procedure imsmpi (a, b, npix, step)

int	a[ARB], b[npix]
size_t	npix
long	step

long	ip, op

begin
	ip = 1
	do op = 1, npix {
	    b[op] = a[ip]
	    ip = ip + step
	}
end
