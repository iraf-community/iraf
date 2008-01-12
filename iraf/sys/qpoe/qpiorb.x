# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpio.h"

# QPIO_RBUCKET -- Load the bucket containing the specified event into the
# QPIO descriptor, returning EOF if the numbered event does not exist.

int procedure qpio_rbucket (io, evi)

pointer	io			#I QPIO descriptor
int	evi			#I bucket number desired

int	ev1, ev2, nb
int	offset, bkno, status

begin
	# Event does not exist?
	if (evi < 1 || evi > IO_NEVENTS(io))
	    return (EOF)

	# Bucket already loaded?
	bkno = EVI_TO_BUCKET(io,evi)
	if (bkno == IO_BKNO(io))
	    return (bkno)

	# Determine range of events in bucket.
	ev1 = BUCKET_TO_EVI(io,bkno)
	ev2 = min (IO_NEVENTS(io), ev1 + IO_BUCKETLEN(io) - 1)

	# Physically read the bucket.
	nb = IO_SZBBUCKET(io)
	offset = (bkno - 1) * nb + IO_FBOFF(io)
	call fm_lfaread (IO_CHAN(io), Mems[IO_BP(io)], nb, offset)
	call fm_lfawait (IO_CHAN(io), status)
	if (status < nb)
	    return (EOF)

	# Update the bucket descriptor.
	IO_BKNO(io) = bkno
	IO_BKFIRSTEV(io) = ev1
	IO_BKLASTEV(io)  = ev2

	return (bkno)
end
