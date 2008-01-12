include	"cqdef.h"

# CQ_LOCATE -- Locate a catalog by name. Return 0 if the catalog is not found.

int procedure cq_locate (cq, name)

pointer	cq				#I the catalog descriptor
char	name[ARB]			#I the catalog name

int	i
bool	streq()

begin
	do i = 1, CQ_NRECS(cq) {
	    if (streq (name, CQ_NAME(cq, i)))
		return (i)
	}

	return (0)
end


# CQ_LOCATEN -- Locate a catalog by number and retrieve its name. Return 0 if
# the catalog is not found.

int procedure cq_locaten (cq, catno, name, maxch)

pointer	cq				#I the catalog descriptor
int	catno				#I the catalog sequence record number
char	name[ARB]			#O the output catalog name
int	maxch				#I the maximum size of the catalog name

begin
	if (catno > 0 && catno <= CQ_NRECS(cq)) {
	    call strcpy (CQ_NAME(cq, catno), name, maxch)
	    return (catno)
	}

	return (0)
end
