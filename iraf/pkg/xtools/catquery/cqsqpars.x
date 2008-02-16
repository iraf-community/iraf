include "cqdef.h"
include "cq.h"


# CQ_SQPAR -- Set the value of a query parameter by name.

int procedure cq_sqpar (cq, name, valuestr)

pointer	cq				#I the catalog descriptor
char	name[ARB]			#I the input query parameter name
char	valuestr[ARB]			#I the parameter value string

pointer	cc, sp, pname, tmpdic, pvalue
int	i, parno, sz1, op1
int	strdic(), strlen(), cq_wrdstr(), gstrcpy()

begin
	# Check that the current catalog is defined.
	if (CQ_CAT(cq) == NULL)
	    return (0)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (0)
	cc = CQ_CAT(cq)


	# Allocate temporary space.
	call smark (sp)
	call salloc (pname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (pvalue, CQ_SZ_QPVALUE, TY_CHAR)

	# Locate the parameter.
	parno = strdic (name, Memc[pname], CQ_SZ_QPNAME, Memc[CQ_PQPNAMES(cc)])
	if (parno <= 0) {
	    call sfree (sp)
	    return (0)
	}

	# Initalize the temporary string.
	sz1 = strlen (Memc[CQ_PQPVALUES(cc)]) + CQ_SZ_QPVALUE
	call malloc (tmpdic, sz1, TY_CHAR)
	call strcpy ("|", Memc[tmpdic], sz1)
	op1 = 2


	# Reformat the values string.
	do i = 1, CQ_NQPARS(cc) {
	    if ((sz1 - op1 + 1) < (CQ_SZ_QPVALUE + 1)) {
		sz1 = sz1 + SZ_LINE
		call realloc (tmpdic, sz1, TY_CHAR)
	    }
	    if (i == parno) {
		op1 = op1 + gstrcpy (valuestr, Memc[tmpdic+op1-1],
		    sz1 - op1 + 1) 
		op1 = op1 + gstrcpy ("|", Memc[tmpdic+op1-1], sz1 - op1 + 1) 
	    } else if (cq_wrdstr (i, Memc[pvalue], CQ_SZ_QPNAME,
	        Memc[CQ_PQPVALUES(cc)]) > 0) {
		op1 = op1 + gstrcpy (Memc[pvalue], Memc[tmpdic+op1-1],
		    sz1 - op1 + 1) 
		op1 = op1 + gstrcpy ("|", Memc[tmpdic+op1-1], sz1 - op1 + 1) 
	    }
	}

	# Update the values string. Leave as temp length for now.
	call realloc (CQ_PQPVALUES(cc), op1 - 1, TY_CHAR)
	call strcpy (Memc[tmpdic], Memc[CQ_PQPVALUES(cc)], op1 - 1)

	call mfree (tmpdic, TY_CHAR)
	call sfree (sp)

	return (parno)
end


# CQ_SQPARN -- Set the value of a query parameter by number.

int procedure cq_sqparn (cq, parno, valuestr)

pointer	cq				#I the catalog descriptor
int	parno				#I the query parameter number
char	valuestr[ARB]			#I the parameter value string

pointer	cc, sp, pname, tmpdic, pvalue
int	i, sz1, op1
int	strlen(), cq_wrdstr(), gstrcpy()

begin
	# Check that the current catalog is defined.
	if (CQ_CAT(cq) == NULL)
	    return (0)
	if (CQ_CATNO(cq) < 1 || CQ_CATNO(cq) > CQ_NRECS(cq))
	    return (0)
	cc = CQ_CAT(cq)

	if (parno < 1 || parno > CQ_NQPARS(cc))
	    return (0)

	# Get some working space.
	call smark (sp)
	call salloc (pname, CQ_SZ_QPNAME, TY_CHAR)
	call salloc (pvalue, CQ_SZ_QPVALUE, TY_CHAR)

	# Initialize the new dictionary.
	sz1 = strlen (Memc[CQ_PQPVALUES(cc)]) + CQ_SZ_QPVALUE
	call calloc (tmpdic, sz1, TY_CHAR)
	call strcpy ("|", Memc[tmpdic], sz1)
	op1 = 2

	# Reformat the values string.
	do i = 1, CQ_NQPARS(cc) {
	    if ((sz1 - op1 + 1) < (CQ_SZ_QPVALUE + 1)) {
		sz1 = sz1 + SZ_LINE
		call realloc (tmpdic, sz1, TY_CHAR)
	    }
	    if (i == parno) {
		op1 = op1 + gstrcpy (valuestr, Memc[tmpdic+op1-1],
		    sz1 - op1 + 1) 
		op1 = op1 + gstrcpy ("|", Memc[tmpdic+op1-1], sz1 - op1 + 1) 
	    } else if (cq_wrdstr (i, Memc[pvalue], CQ_SZ_QPNAME,
	        Memc[CQ_PQPVALUES(cc)]) > 0) {
		op1 = op1 + gstrcpy (Memc[pvalue], Memc[tmpdic+op1-1],
		    sz1 - op1 + 1) 
		op1 = op1 + gstrcpy ("|", Memc[tmpdic+op1-1], sz1 - op1 + 1) 
	    }
	}

	# Update the values string.
	call realloc (CQ_PQPVALUES(cc), op1, TY_CHAR)
	call strcpy (Memc[tmpdic], Memc[CQ_PQPVALUES(cc)], op1 - 1)

	# Free memory.
	call mfree (tmpdic, TY_CHAR)
	call sfree (sp)

	return (parno)
end
