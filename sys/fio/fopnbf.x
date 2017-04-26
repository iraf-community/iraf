# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FOPNBF -- Open a binary file on a device.  A new entry is made in the
# device table if the device has not already been installed.

int procedure fopnbf (fname, mode, zopn, zard, zawr, zawa, zstt, zcls)

char	fname[ARB]
int	mode
extern	zopn(), zard(), zawr(), zawa(), zstt(), zcls()
int	filopn()

begin
	call fdevbf (zard, zawr, zawa, zstt, zcls)
	return (filopn (fname, mode, BINARY_FILE, zopn, zard))
end
