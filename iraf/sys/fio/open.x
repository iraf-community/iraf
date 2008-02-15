# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<knet.h>

# OPEN -- Open a text or binary file on the default device.  If the filename
# is "dev$null" the null file driver is used.  Writes to the null file are
# discarded and reads return EOF.  No device is physically opened, hence
# multiple processes may write to the null file at one time.

int procedure open (fname, mode, type)

char	fname[ARB]			# virtual file name
int	mode				# access mode (ro,rw,apnd,newf,temp)
int	type				# text or binary file

int	fd
pointer	sp, vfn
bool	nullfile, fnullfile()
extern	zopnbf(), zopntx(), zardbf(), zgettx(), zopnsf(), zardsf()
extern	zopnnu(), zardnu(), zgetnu()
int	filopn(), fgetfd(), nowhite()
errchk	syserr, fgetfd, filopn, seek

begin
	call smark (sp)
	call salloc (vfn, SZ_PATHNAME, TY_CHAR)

	# Strip any whitespace at either end of the filename.
	if (nowhite (fname, Memc[vfn], SZ_PATHNAME) == 0)
	    call syserr (SYS_FNOFNAME)

	# Check for the null file.
	nullfile = fnullfile (Memc[vfn])

	# Open the file.
	switch (type) {	
	case TEXT_FILE:
	    if (nullfile)
		fd = filopn (Memc[vfn], mode, type, zopnnu, zgetnu)
	    else
		fd = filopn (Memc[vfn], mode, type, zopntx, zgettx)
	case BINARY_FILE:
	    if (nullfile)
		fd = filopn (Memc[vfn], mode, type, zopnnu, zardnu)
	    else
		fd = filopn (Memc[vfn], mode, type, zopnbf, zardbf)
	case STATIC_FILE:
	    if (nullfile)
		fd = filopn (Memc[vfn], mode, type, zopnnu, zardnu)
	    else
		fd = filopn (Memc[vfn], mode, type, zopnsf, zardsf)
	case SPOOL_FILE:
	    if (nullfile)
		fd = filopn (Memc[vfn], mode, type, zopnnu, zardnu)
	    else {
		fd = fgetfd (Memc[vfn], mode, type)
		call seek (fd, BOFL)
	    }
	default:
	    call syserrs (SYS_FILLEGTYPE, Memc[vfn])
	    fd = ERR
	}

	call sfree (sp)
	return (fd)
end
