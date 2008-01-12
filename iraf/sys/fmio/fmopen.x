# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<knet.h>
include	"fmio.h"

# FM_OPEN -- Open or create a FMIO datafile.  Since this is a low level
# interface we do not want to impose a choice of a file extension on the
# datafile, so if a file extension is desired it must be supplied.

pointer procedure fm_open (fname, mode)

char	fname[ARB]		#I datafile filename
int	mode			#I file access mode

int	chan, n
pointer	sp, osfn, fn, fm
errchk	syserrs, calloc, fclobber
int	nowhite()

begin
	call smark (sp)
	call salloc (fn, SZ_PATHNAME, TY_CHAR)
	call salloc (osfn, SZ_PATHNAME, TY_CHAR)

	n = nowhite (fname, Memc[fn], SZ_PATHNAME)

	# Take care to not clobber an existing file.
	if (mode == NEW_FILE)
	    call fclobber (Memc[fn])

	# Open the datafile.
	call fmapfn (Memc[fn], Memc[osfn], SZ_PATHNAME)
	call zopnbf (Memc[osfn], mode, chan)
	if (chan == ERR)
	    call syserrs (SYS_FMOPEN, Memc[fn])

	# Allocate the FMIO descriptor.
	call calloc (fm, LEN_FMDES, TY_STRUCT)

	FM_MODE(fm) = mode
	FM_CHAN(fm) = chan
	FM_MAGIC(fm) = FMIO_MAGIC
	FM_SZFCACHE(fm) = DEF_FCACHESIZE
	call strcpy (Memc[fn], FM_DFNAME(fm), SZ_DFNAME)

	if (mode == NEW_FILE) {
	    # Wait until first i/o operation to finish initialization.
	    FM_DFVERSION(fm) = FMIO_VERSION
	    FM_SZBPAGE(fm)   = DEF_PAGESIZE
	    FM_NLFILES(fm)   = DEF_MAXLFILES
	    FM_PTILEN(fm)    = DEF_MAXPTPAGES
	    FM_ACTIVE(fm)    = NO

	} else {
	    # Open an existing datafile.
	    iferr (call fmio_readheader(fm)) {
		call mfree (fm, TY_STRUCT)
		call erract (EA_ERROR)
	    } else
		FM_ACTIVE(fm) = YES
	}

	call sfree (sp)
	return (fm)
end
