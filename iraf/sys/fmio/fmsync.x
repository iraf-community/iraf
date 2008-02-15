# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<knet.h>
include	"fmio.h"

define	SZB_SHORT	(SZB_CHAR*SZ_SHORT)

# FM_SYNC -- Update any buffered portions of the datafile on disk which have
# been modified since the last update.

procedure fm_sync (fm)

pointer	fm			#I FMIO descriptor

pointer	sp, ip, op, dhbuf, pgbuf, pti, dh, ft, pt
int	maxpages, nbytes, npti, p1, p2, d1, d2, dp, i
int	szbpage, chan, buflen, status, npte_perpage
int	fmio_extend()
long	clktime()

begin
	if (FM_MODE(fm) == READ_ONLY)
	    return

	call smark (sp)
	chan = FM_CHAN(fm)
	szbpage = FM_SZBPAGE(fm)
	npte_perpage = szbpage / SZB_SHORT

	call intr_disable()

	# Get more page table space (pages).  During a normal file extend
	# occurring while writing to a file, PTEs for *data* pages are added
	# to the incore global page table and to the PT for the lfile.
	# The additional pages needed to store the PT and PTI as they grow
	# are however not allocated during i/o.  We wait and do this at sync
	# time to provide maximal separation of the data pages and PT pages,
	# rendering both as contiguous as possible.

	# Check for page table index overflow (datafile too large).
	npti = (FM_PTNPTE(fm) + npte_perpage-1) / npte_perpage
	if (npti > FM_PTILEN(fm)) {
	    call fmio_posterr (fm, SYS_FMPTIOVFL, FM_DFNAME(fm))

	    # Truncate the page table to try to recover the datafile with
	    # some loss of data.

	    npti = FM_PTILEN(fm)
	    FM_PTNPTE(fm) = npti * npte_perpage - (npti - FM_PTINPTI(fm))
	}

	# Allocate the page table pages.
	for (p1=FM_PTINPTI(fm)+1;  p1 <= npti;  p1=p1+1) {
	    dp = fmio_extend (fm, PT_LFILE, 1)
	    if (dp != ERR) {
		Memi[FM_PTINDEX(fm)+p1-1] = dp
		FM_PTINPTI(fm) = p1
		FM_DHMODIFIED(fm) = YES
	    } else
		call fmio_posterr (fm, SYS_FMPTIOVFL, FM_DFNAME(fm))
	}

	# Update the datafile header area.
	if (FM_DHMODIFIED(fm) != NO) {
	    # Allocate a buffer to hold the encoded datafile header area.
	    buflen = (FM_DATASTART(fm) - 1) / (SZ_STRUCT * SZB_CHAR)
	    call salloc (dhbuf, buflen, TY_STRUCT)

	    # Encode and output the datafile header.
	    call salloc (dh, LEN_DHSTRUCT, TY_STRUCT)

	    DH_MAGIC(dh)	 = FMIO_MAGIC
	    DH_DFVERSION(dh)	 = FM_DFVERSION(fm)
	    DH_SZBPAGE(dh)	 = szbpage
	    DH_NLFILES(dh)	 = FM_NLFILES(fm)
	    DH_FTOFF(dh)	 = FM_FTOFF(fm)
	    DH_FTLASTNF(dh)	 = FM_FTLASTNF(fm)
	    DH_PTIOFF(dh)	 = FM_PTIOFF(fm)
	    DH_PTILEN(dh)	 = FM_PTILEN(fm)
	    DH_PTINPTI(dh)	 = FM_PTINPTI(fm)
	    DH_PTLEN(dh)	 = FM_PTLEN(fm)
	    DH_PTNPTE(dh)	 = FM_PTNPTE(fm)
	    DH_DATASTART(dh)	 = FM_DATASTART(fm)

	    call miipak32 (Memi[dh], Memi[dhbuf], LEN_DHSTRUCT, TY_STRUCT)

	    # Output the file table.
	    ft = FM_FTABLE(fm)
	    op = dhbuf + FM_FTOFF(fm) - 1
	    do i = 0, FM_NLFILES(fm) {
		ip = ft + i * LEN_FTE
		FT_FSIZE(op) = LF_FSIZE(ip)
		FT_FLAGS(op) = and (LFF_SAVE, LF_FLAGS(ip))
		op = op + LEN_FTEX
	    }

	    op = dhbuf + FM_FTOFF(fm) - 1
	    call miipak32 (Memi[op], Memi[op],
		(FM_NLFILES(fm) + 1) * LEN_FTEX, TY_INT)

	    # Output the page table index.
	    call miipak32 (Memi[FM_PTINDEX(fm)], Memi[dhbuf+FM_PTIOFF(fm)-1],
		FM_PTILEN(fm), TY_INT)

	    # Update the whole thing on disk.
	    call zawrbf (chan, Memi[dhbuf], FM_DATASTART(fm)-1, 1)
	    call zawtbf (chan, status)
	    if (status == ERR)
		call fmio_posterr (fm, SYS_FMWRERR, FM_DFNAME(fm))

	    FM_DHMODIFIED(fm) = NO
	}

	# Don't cache these pointers before calling fmio_extend!
	pt  = FM_PTABLE(fm)
	pti = FM_PTINDEX(fm)

	# Update the page table itself, stored in the data pages.
	if (FM_PTNPTE(fm) > FM_PTLUPTE(fm)) {

	    # Determine the max transfer size.
	    maxpages = FM_MAXBUFSIZE(fm) / szbpage
	    if (maxpages <= 0)
		maxpages = DEF_BIGBUFNP
	    
	    # Get an output temporary buffer if we have to swap on output.
	    if (BYTE_SWAP2 == YES)
		call salloc (pgbuf, maxpages * szbpage / SZB_SHORT, TY_SHORT)

	    # Determine the PT page containing the LUPTE+1.
	    p1 = FM_PTLUPTE(fm) / npte_perpage + 1

	    # Update that and all following PT pages.
	    npti = FM_PTINPTI(fm)
	    for (;  p1 <= npti;  p1=p2) {
		# Get a contiguous range of page table pages.
		d1 = Memi[pti+p1-1]
		for (p2=p1+1;  p2 <= npti;  p2=p2+1) {
		    d2 = Memi[pti+p2-1]
		    if (d2-d1 != p2-p1 || p2-p1 >= maxpages)
			break
		}

		# Swap the data and set IP for the output transfer.
		ip = pt + (p1 - 1) * npte_perpage
		nbytes = (min(npti+1,p2) - p1) * szbpage
		if (BYTE_SWAP2 == YES) {
		    call bswap2 (Mems[ip], 1, Mems[pgbuf], 1, nbytes)
		    ip = pgbuf
		}

		# Update the pages.
		call zawrbf (chan, Mems[ip], nbytes,
		    (d1-1) * szbpage + FM_DATASTART(fm))
		call zawtbf (chan, status)
		if (status < nbytes)
		    call fmio_posterr (fm, SYS_FMWRERR, FM_DFNAME(fm))
	    }

	    FM_PTLUPTE(fm) = FM_PTNPTE(fm)
	}

	FM_LSYNCTIME(fm) = clktime(0)
	call intr_enable()

	call sfree (sp)
end
