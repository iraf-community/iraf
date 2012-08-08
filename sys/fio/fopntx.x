# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# FOPNTX -- Open a text file on a device.  A new entry is made in the
# device table if the device has not already been installed.

int procedure fopntx (fname,mode,zopn,zget,zput,zfls,zstt,zcls,zsek,znot)

char	fname[ARB]
int	mode
extern	zopn(), zget(), zput(), zfls(), zstt(), zcls(), zsek(), znot()
int	filopn()

begin
	call fdevtx (zget, zput, zfls, zstt, zcls, zsek, znot)
	return (filopn (fname, mode, TEXT_FILE, zopn, zget))
end
