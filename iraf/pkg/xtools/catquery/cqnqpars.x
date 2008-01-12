include "cqdef.h"


# CQ_NQPARS -- Return the number of query parameters.  Do we really need
# a special routine ?

int procedure cq_nqpars (cq)

pointer	cq				#I the catalog descriptor

begin
	if (CQ_CAT(cq) == NULL)
	    return (0)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (0)

	return (CQ_NQPARS(CQ_CAT(cq)))
end
