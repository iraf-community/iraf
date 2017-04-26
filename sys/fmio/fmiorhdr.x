# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<mach.h>
include	<knet.h>
include	"fmio.h"

# FMIO_READHEADER -- Read the header of a FMIO datafile and set up the FMIO
# runtime descriptor.

procedure fmio_readheader (fm)

pointer	fm			#I FMIO descriptor

pointer	sp, buf, dh, pti, pt, ft, ip, op
int	offset, buflen, npti, p1, p2, d1, d2, b_off1, b_off2, i
int	status, chan, nbytes, nwords, maxpages, szbpage
errchk	syserrs, calloc, malloc, realloc
long	clktime()

begin
	call smark (sp)
	call salloc (dh, LEN_DHSTRUCT, TY_STRUCT)

	chan = FM_CHAN(fm)

	# Make a guess at the size of buffer needed to hold the header.
	buflen = DEF_DFHDRLEN
	call malloc (buf, buflen, TY_STRUCT)

	# Read the full datafile header area into BUF.
	repeat {
	    # Get the raw data.
	    nbytes = buflen * (SZ_STRUCT * SZB_CHAR)
	    call zardbf (chan, Memi[buf], nbytes, 1)
	    call zawtbf (chan, status)
	    if (status == ERR)
		call syserrs (SYS_FMRERR, FM_DFNAME(fm))

	    # Extract the datafile header struct.
	    call miiupk32 (Memi[buf], Memi[dh], LEN_DHSTRUCT, TY_STRUCT)
	    if (DH_MAGIC(dh) != FMIO_MAGIC)
		call syserrs (SYS_FMBADMAGIC, FM_DFNAME(fm))

	    # Repeat if the full header was not read.
	    if (DH_DATASTART(dh)-1 > nbytes) {
		buflen = DH_DATASTART(dh)-1 / (SZ_STRUCT * SZB_CHAR)
		call realloc (buf, buflen, TY_STRUCT)
	    } else if (status < DH_DATASTART(dh)-1) {
		call syserrs (SYS_FMTRUNC, FM_DFNAME(fm))
	    } else
		break
	}

	# Compute region of file in buffer.
	b_off1 = 1
	b_off2 = b_off1 + buflen * SZ_STRUCT * SZB_CHAR

	# Copy general header fields.
	FM_DFVERSION(fm)	= DH_DFVERSION(dh)
	FM_SZBPAGE(fm)		= DH_SZBPAGE(dh)
	FM_NLFILES(fm)		= DH_NLFILES(dh)
	FM_DATASTART(fm)	= DH_DATASTART(dh)
	FM_LSYNCTIME(fm)	= clktime(0)
	FM_DHMODIFIED(fm)	= NO

	# Initialize buffer sizes.
	szbpage = FM_SZBPAGE(fm)
	call fmio_setbuf (fm)
	if (FM_SZBPAGE(fm) != szbpage)
	    call syserrs (SYS_FMBLKCHSZ, FM_DFNAME(fm))

	# Initialize the file table.
	call calloc (ft, (FM_NLFILES(fm) + 1) * LEN_FTE, TY_STRUCT)
	FM_FTOFF(fm)		= DH_FTOFF(dh)
	FM_FTLASTNF(fm)		= DH_FTLASTNF(dh)
	FM_FTABLE(fm)		= ft

	ip = buf + FM_FTOFF(fm) - 1
	call miiupk32 (Memi[ip], Memi[ip],
	    (FM_NLFILES(fm) + 1) * LEN_FTEX, TY_INT)

	do i = 0, FM_NLFILES(fm) {
	    op = ft + i * LEN_FTE
	    LF_FSIZE(op) = FT_FSIZE(ip)
	    LF_FLAGS(op) = FT_FLAGS(ip)
	    ip = ip + LEN_FTEX
	}

	# Read the page table index.
	FM_PTIOFF(fm)		= DH_PTIOFF(dh)
	FM_PTILEN(fm)		= DH_PTILEN(dh)
	FM_PTINPTI(fm)		= DH_PTINPTI(dh)

	ip = buf + FM_PTIOFF(fm) - 1
	call malloc (pti, FM_PTILEN(fm), TY_INT)
	call miiupk32 (Memi[ip], Memi[pti], FM_PTILEN(fm), TY_INT)
	FM_PTINDEX(fm)		= pti

	# Now read the page table itself, stored in the data pages.
	FM_PTLEN(fm)		= DH_PTLEN(dh)
	FM_PTNPTE(fm)		= DH_PTNPTE(dh)
	FM_PTLUPTE(fm)		= DH_PTNPTE(dh)

	call malloc (pt, FM_PTLEN(fm), TY_SHORT)
	FM_PTABLE(fm)		= pt

	maxpages = FM_MAXBUFSIZE(fm) / FM_SZBPAGE(fm)
	if (maxpages <= 0)
	    maxpages = DEF_BIGBUFNP

	op = pt
	npti = FM_PTINPTI(fm)
	for (p1=1;  p1 <= npti;  p1=p2) {
	    # Get a contiguous range of page table pages.
	    d1 = Memi[pti+p1-1]
	    for (p2=p1+1;  p2 <= npti;  p2=p2+1) {
		d2 = Memi[pti+p2-1]
		if (d2-d1 != p2-p1 || p2-p1 >= maxpages)
		    break
	    }

	    # Read in the pages.
	    nbytes = (p2 - p1) * FM_SZBPAGE(fm)
	    nwords = nbytes / (SZB_CHAR * SZ_SHORT)
	    offset = (d1-1) * FM_SZBPAGE(fm) + FM_DATASTART(fm)

	    # Check to see if data is in BUF before reading datafile.
	    if (offset >= b_off1 && (offset+nbytes) <= b_off2)
		call bytmov (Memi[buf], offset, Mems[op], 1, nbytes)
	    else {
		call zardbf (chan, Mems[op], nbytes, offset)
		call zawtbf (chan, status)
		if (status < nbytes)
		    call syserrs (SYS_FMTRUNC, FM_DFNAME(fm))
	    }

	    op = op + nwords
	}

	# Swap the data.
	if (BYTE_SWAP2 == YES)
	    call bswap2 (Mems[pt], 1, Mems[pt], 1, npti * FM_SZBPAGE(fm))

	call mfree (buf, TY_STRUCT)
	call sfree (sp)
end
