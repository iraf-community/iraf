# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# GETHOST -- Get the network name of the host machine.

procedure gethost (outstr, maxch)

char	outstr[maxch]		# receives host name string
int	maxch

size_t	sz_val

begin
	call zghost (outstr, maxch)
	sz_val = maxch
	call strupk (outstr, outstr, sz_val)
end
