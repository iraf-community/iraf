# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	"fmio.h"

# FM_COPYO -- Copy an existing, open datafile to a new, open but empty
# datafile, rendering all lfile storage contiguous and omitting deleted
# lfiles.  Lfile numbers are preserved by the copy operation, hence
# deleted lfiles will leave holes (zero length lfiles) in the file table.

procedure fm_copyo (old, new)

pointer	old, new		#I FMIO descriptors of source and destination

pointer	o_ft, o_lf
int	n_szbpage, nlfiles, dpages, npte_perpage, npti, p1, dp, i
errchk	fmio_bind, syserrs, fm_lfcopy
int	fmio_extend()

begin
	call fmio_bind (old)
	call fmio_bind (new)

	# Scan the file table of the old datafile and determine the number of
	# data pages required to store the valid lfiles therein.  Note that
	# the page size may differ in the old and new datafiles.

	o_ft = FM_FTABLE(old)
	n_szbpage = FM_SZBPAGE(new)
	nlfiles = min (FM_NLFILES(old), FM_NLFILES(new))
	dpages = 0

	do i = 0, nlfiles {
	    o_lf = o_ft + i * LEN_FTE
	    if (LF_FSIZE(o_lf) <= 0 || and(LF_FLAGS(o_lf),LFF_ALLOCATED) == 0)
		next
	    dpages = dpages + (LF_FSIZE(o_lf) + n_szbpage-1) / n_szbpage
	}

	# Now allocate enough lfile 0 space in the new datafile to permit
	# contiguous storage of the entire datafile page table.

	npte_perpage = n_szbpage / (SZ_SHORT * SZB_CHAR)
	npti = (dpages + npte_perpage-1) / npte_perpage
	if (npti > FM_PTILEN(new))
	    call syserrs (SYS_FMPTIOVFL, FM_DFNAME(new))

	for (p1=FM_PTINPTI(new)+1;  p1 <= npti;  p1=p1+1) {
	    dp = fmio_extend (new, PT_LFILE, 1)
	    if (dp == ERR)
		call syserrs (SYS_FMCOPYO, FM_DFNAME(new))
	    Memi[FM_PTINDEX(new)+p1-1] = dp
	    FM_PTINPTI(new) = p1
	    FM_DHMODIFIED(new) = YES
	}

	# Copy the lfiles (excluding the page table).
	do i = 1, nlfiles
	    call fm_lfcopy (old, i, new, i)

	call fm_sync (new)
end
