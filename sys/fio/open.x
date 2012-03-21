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
char	url[SZ_PATHNAME], cache[SZ_PATHNAME], extn[SZ_PATHNAME]

bool	nullfile, fnullfile()
extern	zopnbf(), zopntx(), zardbf(), zgettx(), zopnsf(), zardsf()
extern	zopnnu(), zardnu(), zgetnu()
int	filopn(), fgetfd(), nowhite(), strncmp()

errchk	syserr, fgetfd, filopn, seek

begin
	call smark (sp)
	call salloc (vfn, SZ_PATHNAME, TY_CHAR)


        # If we're given a URL to a file, cache it.
	call aclrc (Memc[vfn], SZ_PATHNAME)
	call strcpy ("cache$", cache, SZ_PATHNAME)
	call strcpy ("", extn, SZ_PATHNAME)

        if (strncmp ("http:", fname, 5) == 0) {
	    call strcpy (fname, url, SZ_PATHNAME)
	    if (mode == NEW_FILE)
		call syserr (SYS_FNOWRITEPERM)
            call fcadd (cache, url, extn, Memc[vfn], SZ_PATHNAME)

        } else if (strncmp ("file:///localhost", fname, 17) == 0) {
	    # Handle local 'file' URIs
	    if (nowhite (fname[18], Memc[vfn], SZ_PATHNAME) == 0)
	        call syserr (SYS_FNOFNAME)

        } else if (strncmp ("file://localhost", fname, 16) == 0) {
	    # Handle local 'file' URIs
	    if (nowhite (fname[16], Memc[vfn], SZ_PATHNAME) == 0)
	        call syserr (SYS_FNOFNAME)

        } else if (strncmp ("file://", fname, 7) == 0) {
	    # Handle local 'file' URIs
	    if (nowhite (fname[7], Memc[vfn], SZ_PATHNAME) == 0)
	        call syserr (SYS_FNOFNAME)

        } else {
	    # Strip any whitespace at either end of the filename.
	    if (nowhite (fname, Memc[vfn], SZ_PATHNAME) == 0)
	        call syserr (SYS_FNOFNAME)
	}

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
