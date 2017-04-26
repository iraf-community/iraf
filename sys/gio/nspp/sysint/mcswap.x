# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MCSWAP -- Swap the instructions in a metacode array.

procedure mcswap (a, npix)

int	a[npix]
int	npix
int	i, temp

begin
	do i = 1, npix, 2 {
	    temp = a[i]
	    a[i] = a[i+1]
	    a[i+1] = temp
	}
end
