# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<qpset.h>
include	"qpoe.h"

# QP_ADDF -- Add a new field (header parameter) to the datafile.  It is an
# error if the parameter redefines an existing symbol.  For variable array
# parameters the initial size is zero, and a new lfile is allocated for the
# parameter value.  For static parameters storage is initialized to all zeros.

procedure qp_addf (qp, param, datatype, maxelem, comment, flags)

pointer	qp			#I QPOE descriptor
char	param[ARB]		#I parameter name
char	datatype[ARB]		#I parameter data type
int	maxelem			#I allocated length of parameter
char	comment[ARB]		#I comment describing parameter
int	flags			#I parameter flags

bool	newtype
pointer	sp, text, st, fm, sym, pval, dsym, dd
int	fd, sz_elem, type, nchars, dtype, nfields, i

long	note()
pointer	qp_gpsym(), stenter(), strefstab()
int	stpstr(), qp_dtype(), qp_parsefl(), gstrcpy
int	fm_nextlfile(), fm_getfd(), qp_elementsize(), fm_fopen()
errchk	qp_bind, qp_gpsym, stenter, stpstr, fm_nextlfile, fm_fopen
errchk	fm_getfd, note, write, syserrs
define	fixed_ 91

begin
	call smark (sp)
	call salloc (text, SZ_TEXTBUF, TY_CHAR)

	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	st = QP_ST(qp)
	fm = QP_FM(qp)

	# Resolve any macro references in the 'datatype' text.
	# (Disabled - not sure this is a good idea here).

	# nchars = qp_expandtext (qp, datatype, Memc[text], SZ_TEXTBUF)
	nchars = gstrcpy (datatype, Memc[text], SZ_TEXTBUF)

	if (QP_DEBUG(qp) > 1) {
	    call eprintf ("qp_addf: `%s' typ=`%s' nel=%d com=`%s' flg=%oB\n")
		call pargstr (param)
		call pargstr (Memc[text])
		call pargi (maxelem)
		call pargstr (comment)
		call pargi (flags)
	}

	# Check for a redefinition.
	sym = qp_gpsym (qp, param)
	if (sym != NULL)
	    call syserrs (SYS_QPREDEF, param)

	# Add the symbol.
	sym = stenter (st, param, LEN_SYMBOL)

	# Determine symbol type.
	dtype = qp_dtype (qp, Memc[text], dsym)
	newtype = (dtype == TY_USER && dsym == NULL)
	sz_elem = qp_elementsize (qp, Memc[text], INSTANCEOF)

	S_DTYPE(sym) = dtype
	S_SZELEM(sym) = 0
	if (dsym != NULL)
	    S_DSYM(sym) = dsym - strefstab(st,0)
	else
	    S_DSYM(sym) = 0

	# If defining a new user datatype (domain), SZELEM is the size of
	# a structure element in chars, and MAXELEM is the length of the
	# field list string, which becomes the value of the domain definition
	# parameter.

	if (newtype) {
	    S_MAXELEM(sym) = nchars
	    call salloc (dd, LEN_DDDES, TY_STRUCT)
	    iferr (nfields = qp_parsefl (qp, Memc[text], dd))
		call erract (EA_WARN)
	    else
		S_SZELEM(sym) = DD_STRUCTLEN(dd) * SZ_STRUCT
	} else
	    S_MAXELEM(sym) = maxelem

	# If no flags are specified, set SF_INHERIT for fixed length params.
	if (flags == 0 && S_MAXELEM(sym) > 0)
	    S_FLAGS(sym) = SF_INHERIT
	else if (flags == QPF_NONE)
	    S_FLAGS(sym) = 0
	else
	    S_FLAGS(sym) = flags

	# Comments are stored in the symbol table and cannot be modified.
	if (comment[1] != EOS)
	    S_COMMENT(sym) = stpstr (st, comment, 0)
	else
	    S_COMMENT(sym) = NULL

	# Initialize data storage for the parameter.
	if (S_MAXELEM(sym) == 0) {
	    # A variable length parameter; store in it's own lfile.  The
	    # initial length is zero, hence initialization is not needed.

	    S_NELEM(sym) = 0
	    S_OFFSET(sym) = 1

	    # If we run out of lfiles, try to make do by allocating a fixed
	    # amount of static storage.

	    iferr (S_LFILE(sym) = fm_nextlfile(fm)) {
		S_MAXELEM(sym) = (QP_FMPAGESIZE(qp) + sz_elem-1) / sz_elem
		call erract (EA_WARN)
		goto fixed_
	    }

	    if (dtype == TY_CHAR)
		type = TEXT_FILE
	    else
		type = BINARY_FILE

	    fd = fm_fopen (fm, S_LFILE(sym), NEW_FILE, type)
	    call close (fd)

	} else {
	    # A fixed length parameter; allocate and initialize storage in
	    # LF_STATICPARS.
fixed_
	    fd = fm_getfd (fm, LF_STATICPARS, APPEND, 0)

	    S_NELEM(sym) = 0
	    S_OFFSET(sym) = note (fd)
	    S_LFILE(sym) = LF_STATICPARS
	    nchars = S_MAXELEM(sym) * sz_elem

	    # The param value is the field list (datatype parameter) for a
	    # domain definition; otherwise we do not have a value yet, so we
	    # merely allocate the storage and initialize to zero.

	    if (newtype) {
		call write (fd, Memc[text], nchars)
		S_NELEM(sym) = S_MAXELEM(sym)
	    } else {
		call salloc (pval, nchars, TY_CHAR)
		call aclrc (Memc[pval], nchars)
		call write (fd, Memc[pval], nchars)
	    }

	    call fm_retfd (fm, S_LFILE(sym))
	}

	if (QP_DEBUG(qp) > 2) {
	    # Dump symbol.
	    call eprintf ("%s: FLG=%oB TYP=%d DSY=%xX NEL=%d ")
		call pargstr (param)
		do i = 1, 4
		    call pargi (Memi[sym+i-1])
	    call eprintf ("MEL=%d SZE=%d COM=%xX LFN=%d OFF=%d\n")
		do i = 5, 9
		    call pargi (Memi[sym+i-1])
	}

	QP_MODIFIED(qp) = YES
	call sfree (sp)
end
