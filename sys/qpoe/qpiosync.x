# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<fset.h>
include	"qpoe.h"
include	"qpio.h"

# QPIO_SYNC -- Update an event list on disk, i.e., flush the bucket buffer if
# it has been written into, and update the event list header.  No QPIO state
# parameters are modified, e.g., the i/o pointer IO_EVI is not affected,
# nor are the contents of the bucket currently being filled; partially filled
# buckets can be synced if desired.

procedure qpio_sync (io)

pointer	io			#I QPIO descriptor

pointer	sp, eh
int	szb_page, off, flen
int	fstati()
errchk	qpio_wbucket

begin
	if (IO_MODE(io) == READ_ONLY)
	    return

	# Flush the bucket buffer.
	if (IO_EVI(io) > 1)
	    call qpio_wbucket (io, IO_EVI(io))

	call smark (sp)
	szb_page = QP_FMPAGESIZE(IO_QP(io))

	# Update the event list header (stored in a full datafile page).
	call salloc (eh, szb_page / (SZ_STRUCT*SZB_CHAR), TY_STRUCT)
	call aclri (Memi[eh], szb_page / (SZ_STRUCT*SZB_CHAR))

	EH_FBOFF(eh)		= szb_page + 1
	EH_NEVENTS(eh)		= IO_NEVENTS(io)
	EH_EVENTLEN(eh)		= IO_EVENTLEN(io)
	EH_SZBBUCKET(eh)	= IO_SZBBUCKET(io)
	EH_BUCKETLEN(eh)	= IO_BUCKETLEN(io)
	EH_EVMINOFF(eh)		= IO_EVMINOFF(io)
	EH_EVMAXOFF(eh)		= IO_EVMAXOFF(io)
	EH_INDEXLEN(eh)		= IO_INDEXLEN(io)
	EH_YOFFVOFF(eh)		= IO_YOFFVOFF(io)
	EH_YOFFVLEN(eh)		= IO_YOFFVLEN(io)
	EH_YLENVOFF(eh)		= IO_YLENVOFF(io)
	EH_YLENVLEN(eh)		= IO_YLENVLEN(io)
	EH_IXXOFF(eh)		= IO_IXXOFF(io)
	EH_IXYOFF(eh)		= IO_IXYOFF(io)
	EH_IXXTYPE(eh)		= IO_IXXTYPE(io)
	EH_IXYTYPE(eh)		= IO_IXYTYPE(io)

	# Output MINEV and MAXEV event structs following the header struct,
	# but in the header page.

	if (IO_MINEVL(io) != NULL) {
	    off = LEN_EHDES
	    call amovs (Mems[IO_MINEVL(io)], Memi[eh+off], IO_EVENTLEN(io))
	    EH_MINEVLOFF(eh) = off
	}

	if (IO_MAXEVL(io) != NULL) {
	    off = LEN_EHDES + (IO_EVENTLEN(io) * SZ_SHORT / SZ_STRUCT)
	    call amovs (Mems[IO_MAXEVL(io)], Memi[eh+off], IO_EVENTLEN(io))
	    EH_MAXEVLOFF(eh) = off
	}

	# Write the header page to the lfile.
	call fm_lfawrite (IO_CHAN(io), Memi[eh], szb_page, 1)
	call fm_lfawait (IO_CHAN(io), szb_page)
	flen = fstati (IO_FD(io), F_FILESIZE)
	if (szb_page / SZB_CHAR > flen)
	    call fseti (IO_FD(io), F_FILESIZE, szb_page / SZB_CHAR)

	call sfree (sp)
end
