include "cqdef.h"
include "cq.h"


# CQ_GNRECORD -- Get next record from the results descriptor.

int procedure cq_gnrecord (res, buf, maxch, recptr)

pointer	res			#I the results descriptor
char	buf[ARB]		#O the output record buffer
int	maxch			#I the maximum buffer size 
int	recptr			#U the current record pointer

int	nchars
int	getline()

begin
	# The record is outside the record data range.
	if (recptr < 0)
	    return (BOF)
	if (recptr >= CQ_RNRECS(res))
	    return (EOF)

	# Use file mechanism to extract record. Could also use buffer pointer
	# and offsets 

	switch (CQ_RTYPE(res)) {

	# Don't worry about maxch at the moment. Just assume that the
	# buffer is at least SZ_LINE long. Can use recsize to return
	# a buffer, SZ_LINE is the default. May need to use getlline 
	# in future.

	case CQ_STEXT, CQ_BTEXT:
	    call seek (CQ_RFD(res), Meml[CQ_RINDEX(res)+recptr])
	    nchars = getline (CQ_RFD(res), buf)
	    recptr = recptr + 1
	    return (nchars)

	default:
	    return (EOF)
	}
end


# CQ_GRECORD -- Get a specified record from the results descriptor.

int procedure cq_grecord (res, buf, maxch, recptr)

pointer	res			#I the results descriptor
char	buf[ARB]		#O the output record buffer
int	maxch			#I the maximum buffer size 
int	recptr			#I the record to be extracted

int	nchars
int	getline()

begin
	# Check for out-of-bounds record requests.
	if (recptr < 1)
	    return (BOF)
	if (recptr > CQ_RNRECS(res))
	    return (EOF)

	# Use file mechanism to extract record. Could also use buffer pointer
	# and offsets 

	switch (CQ_RTYPE(res)) {

	# Don't worry about maxch at the moment. Just assume that the
	# buffer is at least SZ_LINE long. Can use recsize to return
	# a buffer, SZ_LINE is the default. May need to use getlline
	# in future.

	case CQ_STEXT, CQ_BTEXT:
	    call seek (CQ_RFD(res), Meml[CQ_RINDEX(res)+recptr-1])
	    nchars = getline (CQ_RFD(res), buf)
	    return (nchars)

	default:
	    return (EOF)
	}
end
