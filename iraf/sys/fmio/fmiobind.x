# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	"fmio.h"

# FMIO_BIND -- Called when a new datafile is being created, to bind the
# current datafile parameters set in the descriptor to the physical datafile.

procedure fmio_bind (fm)

pointer	fm			#I FMIO descriptor

pointer	ft, pti, pt
int	chan, szbpage, ftoff, ftlen, ptioff, ptilen, ptlen

begin
	if (FM_ACTIVE(fm) != NO)
	    return

	# Initialize buffer sizes.
	call fmio_setbuf (fm)

	chan    = FM_CHAN(fm)
	szbpage = FM_SZBPAGE(fm)
	ftoff   = LEN_DHSTRUCT + 1
	ftlen   = (FM_NLFILES(fm) + 1) * LEN_FTE
	ptioff  = ftoff + (FM_NLFILES(fm) + 1) * LEN_FTEX
	ptilen  = FM_PTILEN(fm)
	ptlen	= szbpage / (SZ_SHORT * SZB_CHAR)

	# Determine the byte offset of the first data page.
	FM_DATASTART(fm) = max (1,
	    (((ptioff+ptilen-1) * SZ_INT*SZB_CHAR) + szbpage-1) /
	    szbpage) * szbpage + 1

	# Initialize the file table.
	call calloc (ft, ftlen, TY_STRUCT)
	FM_FTOFF(fm)		= ftoff
	FM_FTLASTNF(fm)		= 0
	FM_FTABLE(fm)		= ft

	# Initialize the page table index.
	call calloc (pti, ptilen, TY_INT)
	FM_PTIOFF(fm)		= ptioff
	FM_PTINPTI(fm)		= 0
	FM_PTINDEX(fm)		= pti

	# Initialize the page table, stored in the data pages.  Note that the
	# page table length must be an integral multiple of the page size.

	call calloc (pt, ptlen, TY_SHORT)
	FM_PTLEN(fm)		= ptlen
	FM_PTNPTE(fm)		= 0
	FM_PTLUPTE(fm)		= 0
	FM_PTABLE(fm)		= pt

	FM_ACTIVE(fm) = YES
	FM_DHMODIFIED(fm) = YES

	call fm_sync (fm)
end
