# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <mach.h>
include "fmio.h"

# FMIO_EXTEND -- Allocate new pages in the datafile.  Allocate the new pages
# and modify the in-core page table and lfile pagemap accordingly.  The PTI is
# not modified, allocating new pages to store the page table itself, until
# fm_sync is called.  The page number of the first page allocated is returned
# as the function value (a contiguous sequence of pages will be allocated).

int procedure fmio_extend (fm, lfile, npages)

pointer fm                      #I FMIO descriptor
int     lfile                   #I lfile getting the new pages
int     npages                  #I number of pages to add

pointer pt, pm, lf
int	npte_perpage, npti
int     inc, np, p1, p2, l1, l2, i
int	krealloc()

begin
        # Extend the global page table.
        p1 = FM_PTNPTE(fm) + 1
        p2 = p1 + npages - 1

	# Make sure we have enough page table index entries for the new pages.
	if (lfile != PT_LFILE) {
	    npte_perpage = FM_SZBPAGE(fm) / (SZB_CHAR*SZ_SHORT)
	    np = p2 + FM_PTILEN(fm)
	    npti = (np + npte_perpage-1) / npte_perpage
	    if (npti > FM_PTILEN(fm)) {
		call fmio_posterr (fm, SYS_FMPTIOVFL, FM_DFNAME(fm))
		return (ERR)
	    }
	}

        # Increase the size of the in-core page table if necessary.
        if (p2 > FM_PTLEN(fm)) {
	    inc = FM_SZBPAGE(fm) / (SZ_SHORT * SZB_CHAR)
            FM_PTLEN(fm) = ((p2 + inc-1) / inc) * inc
            if (krealloc (FM_PTABLE(fm), FM_PTLEN(fm), TY_SHORT) == ERR)
		return (ERR)
        }

        # Add the pages to the global page table.
        FM_PTNPTE(fm) = p2
        pt = FM_PTABLE(fm)
        do i = p1, p2
            Mems[pt+i-1] = lfile

        lf = FM_FTABLE(fm) + lfile * LEN_FTE
        pm = LF_PAGEMAP(lf)

        # Extend the lfile page table if the lfile is active.
        if (pm != NULL) {
            l1 = LF_NPAGES(lf) + 1
            l2 = l1 + npages - 1

            # Increase the size of the lfile pagemap if necessary.
            if (l2 > LF_PMLEN(lf)) {
                LF_PMLEN(lf) = (l2 + INC_PMLEN-1) / INC_PMLEN * INC_PMLEN
                if (krealloc (LF_PAGEMAP(lf), LF_PMLEN(lf), TY_INT) == ERR)
		    return (ERR)
                pm = LF_PAGEMAP(lf)
            }

            # Add the pages to the lfile page table.
            LF_NPAGES(lf) = l2
            do i = l1, l2
                Memi[pm+i-1] = p1 + i - l1
        }

	# Update the FTE for lfile zero (the page table file).
	lf = FM_FTABLE(fm)
	LF_FSIZE(lf) = FM_PTNPTE(fm) * SZ_SHORT * SZB_CHAR

        FM_DHMODIFIED(fm) = YES
        return (p1)
end
