include "cqdef.h"
include "cq.h"


# CQ_SETCAT -- Set the current catalog by name.

int procedure cq_setcat (cq, name)

pointer	cq				#I the catalog descriptor
char	name[ARB]			#I the catalog name

int	i, catno
int	cq_ccrquery()
bool	streq()

begin
	catno = 0
	do i = 1, CQ_NRECS(cq) {
	    if (streq (name, CQ_NAME(cq, i)))
		catno = i
	}
	if (catno == 0)
	    return (0)

	# Free the previous current catalog descriptor if any.
	call cq_ccfree (cq)

	# Allocate the new descriptor.
	call cq_ccinit (cq, catno)

	# Get the new catalog parameters.
	if (cq_ccrquery (cq) == ERR) {
	    call cq_ccfree (cq)
	    return (0)
	}

	return (catno)
end


# CQ_SETCATN -- Set the current catalog by number.

int procedure cq_setcatn (cq, catno)

pointer	cq				#I the catalog descriptor
int	catno				#I the catalog number

int	cq_ccrquery()

begin
	if (catno < 1 || catno > CQ_NRECS(cq))
	    return (0)

	# Free the previous current catalog descriptor if any.
	call cq_ccfree (cq)

	# Allocate the new descriptor.
	call cq_ccinit (cq, catno)

	# Get the new catalog parameters.
	if (cq_ccrquery (cq) == ERR) {
	    call cq_ccfree (cq)
	    return (0)
	}

	return (catno)
end


# CQ_CCINIT -- Initialize the current catalog descriptor.

procedure cq_ccinit (cq, catno)

pointer	cq			#I the catalog database descriptor
int	catno			#I the current catalog number

pointer	cc

begin
	if (catno < 1 || catno > CQ_NRECS(cq))
	    return
	CQ_CATNO(cq) = catno
	call strcpy (CQ_NAME(cq, catno), CQ_CATNAME(cq), SZ_FNAME)

	call calloc (CQ_CAT(cq), CQ_LEN_CC, TY_STRUCT)
	cc = CQ_CAT(cq)

	CQ_NQPARS(cc) = 0
	CQ_HFMT(cc) = CQ_HNONE

	call calloc (CQ_PQPNAMES(cc), SZ_LINE, TY_CHAR)
	call calloc (CQ_PQPDVALUES(cc), SZ_LINE, TY_CHAR)
	call calloc (CQ_PQPVALUES(cc), SZ_LINE, TY_CHAR)
	call calloc (CQ_PQPUNITS(cc), SZ_LINE, TY_CHAR)
	call calloc (CQ_PQPFMTS(cc), SZ_LINE, TY_CHAR)

	Memc[CQ_PQPNAMES(cc)] = EOS
	Memc[CQ_PQPDVALUES(cc)] = EOS
	Memc[CQ_PQPVALUES(cc)] = EOS
	Memc[CQ_PQPUNITS(cc)] = EOS
	Memc[CQ_PQPFMTS(cc)] = EOS

	CQ_ADDRESS(cc) = EOS
	CQ_QUERY(cc) = EOS

end


# CQ_CCRQUERY -- Read in the query related parameters from the catalog
# database. May need to encode the field names at some point.

int procedure cq_ccrquery (cq)

pointer	cq			#I the catalog database descriptor

pointer	cc, sp, str
int	i, catno, nqpars, npars, sz1, sz2, sz3, sz4, sz5
int	op1, op2, op3, op4, op5
int	cq_dgeti(), cq_dscan(), nscan(), gstrcpy(), strdic()
errchk	cq_dgwrd(), cq_dgeti(), cq_dscan()

begin
	# If the current catalog is not defined then return.
	if (CQ_CAT(cq) == NULL)
	    return (ERR)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (ERR)
	cc = CQ_CAT(cq)
	catno = CQ_CATNO(cq)

	call smark (sp)
	call salloc (str, 4 * (SZ_LINE + 1), TY_CHAR)

	iferr {

	    # Get the catalog address and query string.
	    call cq_dgwrd (cq, catno, "address", CQ_ADDRESS(cc), SZ_LINE)
	    call cq_dgstr (cq, catno, "query", CQ_QUERY(cc), SZ_LINE)

	    # Get the protocol.
	    call cq_dgwrd (cq, catno, "protocol", Memc[str], SZ_LINE)
	    CQ_HFMT(cc) = strdic (Memc[str], Memc[str], SZ_LINE,
	        CQ_HFMTSTR)
	    if (CQ_HFMT(cc) <= 0)
		CQ_HFMT(cc) = CQ_HNONE

	    # Determine the number of query parameters and position the
	    # file to the correct place.
	    nqpars = cq_dgeti (cq, catno, "nquery")

	} then {

	    # Reinitialize and return.
	    CQ_ADDRESS(cc) = EOS
	    CQ_QUERY(cc) = EOS
	    call sfree (sp)
	    return (ERR)
	}


	# Open the query parameter string dictionaries.
	sz1 = SZ_LINE; op1 = 2
	sz2 = SZ_LINE; op2 = 2
	sz3 = SZ_LINE; op3 = 2
	sz4 = SZ_LINE; op4 = 2
	sz5 = SZ_LINE; op5 = 2
	call strcpy ("|", Memc[CQ_PQPNAMES(cc)], sz1)
	call strcpy ("|", Memc[CQ_PQPDVALUES(cc)], sz2)
	call strcpy ("|", Memc[CQ_PQPVALUES(cc)], sz3)
	call strcpy ("|", Memc[CQ_PQPUNITS(cc)], sz4)
	call strcpy ("|", Memc[CQ_PQPFMTS(cc)], sz5)

	# Scan the query parameter list.
	npars = 0
	for (i = 1; i <= nqpars; i = i + 1) {
	    if (cq_dscan (cq) == EOF)
		break

	    # Get the query parameter fields.
	    call gargwrd (Memc[str], SZ_LINE)
	    call gargwrd (Memc[str+SZ_LINE+1], SZ_LINE)
	    call gargwrd (Memc[str+2*(SZ_LINE+1)], SZ_LINE)
	    call gargwrd (Memc[str+3*(SZ_LINE+1)], SZ_LINE)
	    if (nscan() != 4)
		break

	    # Get the query parameter name.
	    if ((sz1 - op1 + 1)  <  (CQ_SZ_QPNAME + 1)) {
		sz1 = sz1 + SZ_LINE
		call realloc (CQ_PQPNAMES(cc), sz1, TY_CHAR)
	    }
	    op1 = op1 + gstrcpy (Memc[str], Memc[CQ_PQPNAMES(cc)+op1-1],
	        sz1 - op1 + 1)
	    op1 = op1 + gstrcpy ("|", Memc[CQ_PQPNAMES(cc)+op1-1],
	        sz1 - op1 + 1)

	    # Get the default query parameter value and initialize the
	    # user query parameter string.
	    if ((sz2 - op2 + 1)  <  (CQ_SZ_QPVALUE + 1)) {
		sz2 = sz2 + SZ_LINE
		sz3 = sz3 + SZ_LINE
		call realloc (CQ_PQPDVALUES(cc), sz2, TY_CHAR)
		call realloc (CQ_PQPVALUES(cc), sz3, TY_CHAR)
	    }
	    op2 = op2 + gstrcpy (Memc[str+SZ_LINE+1],
	        Memc[CQ_PQPDVALUES(cc)+op2-1], sz2 - op2 + 1)
	    op2 = op2 + gstrcpy ("|", Memc[CQ_PQPDVALUES(cc)+op2-1],
	        sz2 - op2 + 1)
	    op3 = op3 + gstrcpy (Memc[str+SZ_LINE+1],
	        Memc[CQ_PQPVALUES(cc)+op3-1], sz3 - op3 + 1)
	    op3 = op3 + gstrcpy ("|", Memc[CQ_PQPVALUES(cc)+op3-1],
	        sz3 - op3 + 1)

	    # Get the query parameter units.
	    if ((sz4 - op4 + 1)  <  (CQ_SZ_QPUNITS + 1)) {
		sz4 = sz4 + SZ_LINE
		call realloc (CQ_PQPUNITS(cc), sz4, TY_CHAR)
	    }
	    op4 = op4 + gstrcpy (Memc[str+2*(SZ_LINE+1)],
	        Memc[CQ_PQPUNITS(cc)+op4-1], sz4 - op4 + 1)
	    op4 = op4 + gstrcpy ("|", Memc[CQ_PQPUNITS(cc)+op4-1],
	        sz4 - op4 + 1)

	    # Get the query parameter formats.
	    if ((sz5 - op5 + 1)  <  (CQ_SZ_QPFMTS + 1)) {
		sz5 = sz5 + SZ_LINE
		call realloc (CQ_PQPFMTS(cc), sz5, TY_CHAR)
	    }
	    op5 = op5 + gstrcpy (Memc[str+3*(SZ_LINE+1)],
	        Memc[CQ_PQPFMTS(cc)+op5-1], sz5 - op5 + 1)
	    op5 = op5 + gstrcpy ("|", Memc[CQ_PQPFMTS(cc)+op5-1],
	        sz5 - op5 + 1)

	    npars = npars + 1
	}

	# Return the appropriate status.
	call sfree (sp)
	if (npars != nqpars) {
	    CQ_NQPARS(cc) = 0
	    call realloc (CQ_PQPNAMES(cc), SZ_LINE, TY_CHAR)
	    call realloc (CQ_PQPDVALUES(cc), SZ_LINE, TY_CHAR)
	    call realloc (CQ_PQPVALUES(cc), SZ_LINE, TY_CHAR)
	    call realloc (CQ_PQPUNITS(cc), SZ_LINE, TY_CHAR)
	    call realloc (CQ_PQPFMTS(cc), SZ_LINE, TY_CHAR)
	    Memc[CQ_PQPNAMES(cc)] = EOS
	    Memc[CQ_PQPDVALUES(cc)] = EOS
	    Memc[CQ_PQPVALUES(cc)] = EOS
	    Memc[CQ_PQPUNITS(cc)] = EOS
	    Memc[CQ_PQPFMTS(cc)] = EOS
	    CQ_ADDRESS(cc) = EOS
	    CQ_QUERY(cc) = EOS
	    return (ERR)
	} else {
	    CQ_NQPARS(cc) = npars
	    call realloc (CQ_PQPNAMES(cc), op1, TY_CHAR)
	    call realloc (CQ_PQPDVALUES(cc), op2, TY_CHAR)
	    call realloc (CQ_PQPVALUES(cc), op3, TY_CHAR)
	    call realloc (CQ_PQPUNITS(cc), op4, TY_CHAR)
	    call realloc (CQ_PQPFMTS(cc), op5, TY_CHAR)
	    Memc[CQ_PQPNAMES(cc)+op1] = EOS
	    Memc[CQ_PQPDVALUES(cc)+op2] = EOS
	    Memc[CQ_PQPVALUES(cc)+op3] = EOS
	    Memc[CQ_PQPUNITS(cc)+op4] = EOS
	    Memc[CQ_PQPFMTS(cc)+op5] = EOS
	    return (OK)
	}
end


# CQ_CCFREE - Free the current catalog descriptor

procedure cq_ccfree (cq)

pointer	cq			#I the catalog database descriptor

pointer	cc

begin
	CQ_CATNAME(cq) = EOS
	CQ_CATNO(cq) = 0

	if (CQ_CAT(cq) != NULL) {
	    cc  = CQ_CAT(cq)
	    call mfree (CQ_PQPNAMES(cc), TY_CHAR)
	    call mfree (CQ_PQPDVALUES(cc), TY_CHAR)
	    call mfree (CQ_PQPVALUES(cc), TY_CHAR)
	    call mfree (CQ_PQPUNITS(cc), TY_CHAR)
	    call mfree (CQ_PQPFMTS(cc), TY_CHAR)
	    call mfree (CQ_CAT(cq), TY_STRUCT)
	}
	CQ_CAT(cq) = NULL
end
