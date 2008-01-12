include	"cqdef.h"
include	"cq.h"


# CQ_STATI -- Get an integer catalog database parameter.

int procedure cq_stati (cq, param)

pointer	cq			#I pointer to the catalog query structure.
int	param			#I the integer parameter to be retrieved

begin
	switch (param) {
	case CQNRECS:
	    return (CQ_NRECS(cq))
	case CQSZRECLIST:
	    return (CQ_NAMEI(cq, CQ_NRECS(cq) + 1))
	case CQCATNO:
	    return (CQ_CATNO(cq))
	default:
	    call error (0, "Error fetching integer catalog database parameter")
	}
end



# CQ_STATS -- Get a string catalog database parameter.

procedure cq_stats (cq, param, str, maxch)

pointer	cq			#I pointer to the catalog query structure.
int	param			#I the string parameter to be retrieved
char	str[ARB]		#O the output string parameter
int	maxch			#I the maximum size of the string parameter

begin
	switch (param) {
	case CQCATDB:
	    call strcpy (CQ_CATDB(cq), str, maxch)
	case CQCATNAME:
	    call strcpy (CQ_CATNAME(cq), str, maxch)
	default:
	    call error (0, "Error fetching string catalog database parameter")
	}
end


# CQ_STATT -- Get a text list catalog database parameter. A text list is a
# string with items separated from each other by newlines.

int procedure cq_statt (cq, param, str, maxch)

pointer	cq			#I pointer to the catalog query structure.
int	param			#I the list parameter to be retrieved
char	str[ARB]		#O the output string parameter
int	maxch			#I the maximum size of the string parameter

int	i, fd
int	stropen()

begin
	switch (param) {
	case CQRECLIST:
	    fd = stropen (str, maxch, NEW_FILE)
	    do i = 1, CQ_NRECS(cq) {
		call fprintf (fd, "%s\n")
		    call pargstr (CQ_NAME(cq, i))
	    }
	    call strclose (fd)
	    return (CQ_NRECS(cq))
	default:
	    call error (0, "Error fetching list catalog database parameter")
	}
end
