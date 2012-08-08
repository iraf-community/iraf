# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	"fmio.h"

# FM_NEXTLFILE -- Return the next available (empty) lfile.  An error action
# is taken if all lfiles are currently in use.  Deleted lfiles will be reused
# if no unused lfiles are found.

int procedure fm_nextlfile (fm)

pointer	fm			#I FMIO descriptor

pointer	ft, lf
int	nlfiles, flags, fn, i
errchk	syserrs, fmio_bind, fmio_errchk

begin
	call fmio_bind (fm)
	call fmio_errchk (fm)

	fn = FM_FTLASTNF(fm)
	ft = FM_FTABLE(fm)
	nlfiles = FM_NLFILES(fm)

	# Travel once around the file table and abort if all entries are used.
	# New files are normally returned in sequence.  Deleted files can be
	# reused.

	do i = 1, nlfiles {
	    fn = fn + 1
	    if (fn > nlfiles)
		fn = 1
	    lf = ft + fn * LEN_FTE
	    flags = LF_FLAGS(lf)
	    if (and(flags,LFF_ALLOCATED) == 0 || and(flags,LFF_DELETED) != 0)
		break
	}

	if (i > nlfiles)
	    call syserrs (SYS_FMOOF, FM_DFNAME(fm))

	FM_FTLASTNF(fm) = fn
	FM_DHMODIFIED(fm) = YES
	call fmio_tick (fm)

	return (fn)
end
