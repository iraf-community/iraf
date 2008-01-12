int procedure gg_relindef (data, npix)

# GG_RELINDEF -- Replace data values less than or equal to zero by INDEF 
# and return the number or replaced pixels

real	data[ARB]
int	npix

int	i
int	rep

begin
	rep = 0

	do i = 1, npix
	    if (data[i] <= 0.0) {
		data[i] = INDEF
		rep = rep + 1
	    }

	return (rep)
end
