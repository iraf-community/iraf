include "cqdef.h"
include "cq.h"

# These routines fetch fields from the catalog configuation by field name.
# They can be used by the calling program to read quantities of interest
# directly from the configuration file. In most applications it should
# not be necessary to use these routines as the main interface routines
# provide most of the desired functionality, but they are included for
# completeness.

# CQ_FGETI -- Fetch an integer field from the current catalog.

int procedure cq_fgeti (cq, field)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name

int	ival
int	cq_dgeti()
errchk	cq_dgeti()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	ival = cq_dgeti (cq, CQ_CATNO(cq), field)

	return (ival)
end


# CQ_FGETR -- Fetch a real field from the current catalog.

real procedure cq_fgetr (cq, field)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name

real	rval
real	cq_dgetr()
errchk	cq_dgetr()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	rval = cq_dgetr (cq, CQ_CATNO(cq), field)

	return (rval)
end


# CQ_FGETD -- Fetch a double precision field from the current catalog.

double procedure cq_fgetd (cq, field)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name

double	dval
double	cq_dgetd()
errchk	cq_dgetd()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	dval = cq_dgetd (cq, CQ_CATNO(cq), field)

	return (dval)
end


# CQ_FGAI -- Get an array valued integer parameter.

int procedure cq_fgai (cq, field, array, max_len)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
int	array[ARB]			#O the output array
int	max_len				#I the maximum length of the array

int	npts

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgai (cq, CQ_CATNO(cq), field, array, max_len, npts)

	return (npts)
end


# CQ_FGAR -- Get an array valued real parameter.

int procedure cq_fgar (cq, field, array, max_len)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
real	array[ARB]			#O the output array
int	max_len				#I the maximum length of the array

int	npts

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgar (cq, CQ_CATNO(cq), field, array, max_len, npts)

	return (npts)
end


# CQ_FGAD -- Get an array valued double parameter.

int procedure cq_fgad (cq, field, array, max_len)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
double	array[ARB]			#O the output array
int	max_len				#I the maximum length of the array

int	npts

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgad (cq, CQ_CATNO(cq), field, array, max_len, npts)

	return (npts)
end


# CQ_FGWRD -- Fetch a single word field from the current catalog.

procedure cq_fgwrd (cq, field, str, maxch)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
char	str[ARB]			#O the output string
int	maxch				#I the maximum number of characters

errchk	cq_dgwrd()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgwrd (cq, CQ_CATNO(cq), field, str, maxch)
end


# CQ_FGSTR -- Fetch a string field from the current catalog.

procedure cq_fgstr (cq, field, str, maxch)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
char	str[ARB]			#O the output string
int	maxch				#I the maximum number of characters

errchk	cq_dgwrd()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgstr (cq, CQ_CATNO(cq), field, str, maxch)
end


# CQ_FGTEXT -- Fetch a multi-line text field from the current catalog.

int procedure cq_fgtext (cq, field, str, maxch)

pointer	cq				#I the catalog descriptor
char	field[ARB]			#I the field name
char	str[ARB]			#O the output string
int	maxch				#I the maximum number of characters

int	nlines
errchk	cq_dgatxt()

begin
	if (CQ_CAT(cq) == NULL)
	    call error (0, "The current catalog is undefined")
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    call error (0, "The current catalog is undefined")

	call cq_dgatxt (cq, CQ_CATNO(cq), field, str, maxch, nlines)

	return (nlines)
end


# CQ_SCAN -- Scan the database at the current position.

int procedure cq_scan (cq)

pointer cq                              # The catalog database descriptor.

int     fscan()

begin
        return (fscan (CQ_FD(cq)))
end
