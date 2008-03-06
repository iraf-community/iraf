# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"fmset.h"
include	"fmio.h"

# FM_DEBUG -- Print debug info on the contents of a datafile.
#
# Flags:
#	FMD_HEADER		general header parameters
#	FMD_FTABLE		summarize file table contents
#	FMD_PTINDEX		print page table index
#	FMD_PTABLE		print page table
#	FMD_ALL			print everything

procedure fm_debug (fm, out, what)

pointer	fm			#I FMIO descriptor
int	out			#I output file
int	what			#I what to print

pointer	ft, lf
bool	deleted
int	nlfiles, nlfdeleted, nlfinuse
long	szbpage, filesize, freespace, i, c_15
int	spaceinuse
int	modi()
long	clktime(), modl()
errchk	fmio_bind

define	end_header_	91
define	end_ftable_	92
define	end_ptindex_	93
define	end_ptable_	94

begin
	c_15 = 15

	call fmio_bind (fm)

	ft = FM_FTABLE(fm)
	nlfiles = FM_NLFILES(fm)
	szbpage = FM_SZBPAGE(fm)

	# Print header and summary information?
	if (and (what, FMD_HEADER) == 0)
	    goto end_header_

	# Scan the file table and compute some important statistics.
	spaceinuse = 0
	nlfdeleted = 0
	nlfinuse = 0

	do i = 0, nlfiles {
	    lf = ft + i * LEN_FTE
	    deleted = (and (LF_FLAGS(lf), LFF_DELETED) != 0)
	    if (deleted)
		nlfdeleted = nlfdeleted + 1
	    if (!deleted) {
		if (LF_FSIZE(lf) > 0)
		    spaceinuse = spaceinuse + LF_FSIZE(lf)
		if (and (LF_FLAGS(lf), LFF_ALLOCATED) != 0)
		    nlfinuse = nlfinuse + 1
	    }
	}

	filesize = FM_PTNPTE(fm) * szbpage + FM_DATASTART(fm) - 1
	freespace = max (0, filesize - spaceinuse - (FM_DATASTART(fm)-1))

	call fprintf (out,
	"FMIO V%d.%d: datafile=%s, pagesize=%d, nlfiles=%d\n")
	    call pargi (FM_DFVERSION(fm) / 100)
	    call pargi (modi(FM_DFVERSION(fm),100))
	    call pargstr (FM_DFNAME(fm))
	    call pargl (szbpage)
	    call pargi (nlfiles)

	call fprintf (out,
	    "nlfinuse=%d, nlfdeleted=%d, nlffree=%d, ftoff=%d, ftlastnf=%d\n")
	    call pargi (nlfinuse)
	    call pargi (nlfdeleted)
	    call pargi (nlfiles - nlfinuse)
	    call pargl (FM_FTOFF(fm))
	    call pargi (FM_FTLASTNF(fm))

	call fprintf (out,
	    "headersize=%d, filesize=%d, freespace=%d bytes (%d%%)\n")
	    call pargl (FM_DATASTART(fm) - 1)
	    call pargl (filesize)
	    call pargl (freespace)
	    if (freespace <= 0)
		call pargi (0)
	    else
		call pargl (freespace * 100 / filesize)

	call fprintf (out,
	    "fm=%xX, chan=%d, mode=%d, time since last sync=%d seconds\n")
	    call pargp (fm)
	    call pargi (FM_CHAN(fm))
	    call pargi (FM_MODE(fm))
	    call pargl (clktime (FM_LSYNCTIME(fm)))

	call fprintf (out,
	    "datastart=%d, devblksize=%d, optbufsize=%d, maxbufsize=%d\n")
	    call pargl (FM_DATASTART(fm))
	    call pargl (FM_DEVBLKSIZE(fm))
	    call pargz (FM_OPTBUFSIZE(fm))
	    call pargz (FM_MAXBUFSIZE(fm))

	call fprintf (out, "ptioff=%d, ptilen=%d, npti=%d, ")
	    call pargl (FM_PTIOFF(fm))
	    call pargl (FM_PTILEN(fm))
	    call pargl (FM_PTINPTI(fm))
	call fprintf (out, "ptlen=%d, npte=%d, lupte=%d\n")
	    call pargz (FM_PTLEN(fm))
	    call pargl (FM_PTNPTE(fm))
	    call pargl (FM_PTLUPTE(fm))

end_header_

	# Print file table?
	if (and (what, FMD_FTABLE) == 0)
	    goto end_ftable_

	call fprintf (out,
	    "====================== file table =======================\n")
	do i = 0, nlfiles {
	    lf = ft + i * LEN_FTE
	    if (LF_FSIZE(lf) == 0)
		next
	    call fprintf (out, " %4d size=%d")
		call pargl (i)
		call pargl (LF_FSIZE(lf))
	    if (i == 0)
		call fprintf (out, " [page table]")
	    if (LF_PAGEMAP(lf) != NULL) {
		call fprintf (out, " npages=%d pmlen=%d")
		    call pargl (LF_NPAGES(lf))
		    call pargz (LF_PMLEN(lf))
	    }
	    if (and (LF_FLAGS(lf), LFF_ALLOCATED) != 0)
		call fprintf (out, " allocated")
	    if (and (LF_FLAGS(lf), LFF_DELETED) != 0)
		call fprintf (out, " deleted")
	    if (and (LF_FLAGS(lf), LFF_TEXTFILE) != 0)
		call fprintf (out, " textfile")
	    call fprintf (out, "\n")
	}

end_ftable_

	# Print page table index?
	if (and (what, FMD_PTINDEX) == 0)
	    goto end_ptindex_

	call fprintf (out,
	    "=================== page table index ====================\n")
	do i = 0, FM_PTINPTI(fm) - 1 {
	    call fprintf (out, " %4d")
		call pargl (Meml[FM_PTINDEX(fm)+i])
	    if (modl (i+1, c_15) == 0)
		call fprintf (out, "\n")
	}
	if (modl (FM_PTINPTI(fm), c_15) != 0)
	    call fprintf (out, "\n")

end_ptindex_

	# Print page table?
	if (and (what, FMD_PTABLE) == 0)
	    goto end_ptable_

	call fprintf (out,
	    "====================== page table =======================\n")
	do i = 0, FM_PTNPTE(fm) - 1 {
	    call fprintf (out, " %4d")
		call pargs (Mems[FM_PTABLE(fm)+i])
	    if (modl (i+1, c_15) == 0)
		call fprintf (out, "\n")
	}
	if (modl (FM_PTINPTI(fm), c_15) != 0)
	    call fprintf (out, "\n")

end_ptable_

	call flush (out)
end
