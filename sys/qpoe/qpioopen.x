# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<syserr.h>
include	<error.h>
include	<mach.h>
include	<fset.h>
include	<plset.h>
include	"qpoe.h"
include	"qpex.h"
include	"qpio.h"

# QPIO_OPEN -- Open the named event list parameter for event i/o.  Since event
# lists can only be read and written sequentially, there are only two useful
# i/o modes, namely, READ_ONLY and NEW_FILE.  Filtering is permitted only
# when reading an event list; when writing to a new event list, the events
# are merely copied out as they are received.

pointer procedure qpio_open (qp, paramex, mode)

pointer	qp			#I QPOE descriptor
char	paramex[ARB]		#I event-list parameter plus expression list
int	mode			#I access mode

bool	newlist
pointer	sp, io, dd, eh, op, oo, flist, deffilt, defmask, maskname
pointer	param, expr, filter, filter_text, mask, umask, psym, dsym, name
int	sz_filter, szb_page, nwords, nchars, junk, fd, ip, i, j

pointer	qp_gpsym(), qpex_open(), stname(), strefstab()
int	qp_popen(), qp_lenf(), read(), pl_l2pi(), fstati()
int	qp_geti(), qp_gstr(), qp_parsefl(), qpio_parse(), qpex_modfilter()

errchk	qp_bind, qp_geti, qpio_parse, qp_gpsym, qp_addf, qp_gstr
errchk	qp_parsefl, qp_popen, qpex_open, qpio_loadmask, qpex_modfilter
errchk	stname, calloc, malloc, realloc, read, syserrs
string	s_deffilt DEF_FILTER
string	s_defmask DEF_MASK
define	done_ 91

begin
	call smark (sp)
	call salloc (deffilt, SZ_FNAME, TY_CHAR)
	call salloc (defmask, SZ_FNAME, TY_CHAR)
	call salloc (maskname, SZ_FNAME, TY_CHAR)
	call salloc (umask, SZ_FNAME, TY_CHAR)

	if (QP_ACTIVE(qp) == NO)
	    call qp_bind (qp)

	newlist = (mode == NEW_FILE || mode == APPEND)

	if (QP_DEBUG(qp) > 0) {
	    call eprintf ("qpio_open (%xX, `%s', %d)\n")
		call pargi (qp)
		call pargstr (paramex)
		call pargi (mode)
	}

	# Allocate and initialize the QPIO descriptor.
	call calloc (io, LEN_IODES, TY_STRUCT)

	call calloc (IO_DD(io), LEN_DDDES, TY_STRUCT)
	call calloc (IO_PARAM(io), SZ_FNAME, TY_CHAR)
	call calloc (IO_MASK(io), SZ_FNAME, TY_CHAR)

	IO_QP(io) = qp
	IO_MODE(io) = mode
	IO_DEBUG(io) = QP_DEBUG(qp)
	IO_XBLOCK(io) = QP_XBLOCK(qp)
	IO_YBLOCK(io) = QP_YBLOCK(qp)
	IO_NODEFFILT(io) = QP_NODEFFILT(qp)
	IO_NODEFMASK(io) = QP_NODEFMASK(qp)
	IO_OPTBUFSIZE(io) = QP_OPTBUFSIZE(qp)
	IO_ACTIVE(io) = NO

	dd 	= IO_DD(io)
	param 	= IO_PARAM(io)
	mask 	= IO_MASK(io)
	filter 	= NULL

iferr {
	# Get the image dimensions.
	IO_NCOLS(io)	= qp_geti (qp, "axlen[1]")
	IO_NLINES(io)	= qp_geti (qp, "axlen[2]")

	# Parse the parameter expression into the parameter name and
	# expression qualifier fields.  Possible variations on the input
	# syntax are "" (null string, default everything), "param" (parameter
	# name only), "param[expr]" (parameter name plus expression), and
	# "[expr]" or "expr" (expression only), where the parameter name may
	# be specified as in "[param=value,...]", i.e., as a term in the
	# expression (allowing it to be input by the user to override the
	# default).

	op = param
	for (ip=1;  paramex[ip] != EOS && paramex[ip] != '[';  ip=ip+1) {
	    Memc[op] = paramex[ip]
	    op = op + 1
	}
	expr = ip

	# Parse the expression qualifier field to set the i/o parameters,
	# e.g., region mask, event attribute filter, blocking factor,
	# coordinate system, etc.  All QPIO parameters are removed, returning
	# the filter expression (if any) to be passed on to QPEX for event
	# attribute filtering.  The `filter' buffer is passed by pointer so
	# that it may be reallocated if more space is needed.

	sz_filter = DEF_SZEXPRBUF
	call malloc (filter, sz_filter, TY_CHAR)
	if (qpio_parse (io, paramex[expr],
	    filter, sz_filter, Memc[mask], SZ_FNAME) == ERR)
	    call eprintf ("QPIO warning: error parsing options expression\n")

	# If no event list parameter was named, use the default.
	if (Memc[param] == EOS)
	    call strcpy (DEF_EVENTPARAM, Memc[param], SZ_FNAME)

	# Verify the parameter's type if it already exists, or create a new
	# parameter of the default type if the mode is newfile or append.

	psym = qp_gpsym (qp, Memc[param])
	if (psym != NULL) {
	    if (S_DTYPE(psym) != TY_USER || S_DSYM(psym) == NULL)
		call syserrs (SYS_QPNEVPAR, Memc[param])
	    else if (newlist && S_NELEM(psym) > 0)
		call syserrs (SYS_QPCLOBBER, Memc[param])
	} else if (mode == READ_ONLY) {
	    call syserrs (SYS_QPUKNPAR, Memc[param])
	} else {
	    call qp_addf (qp, Memc[param], DEF_EVENTTYPE, 0, "", 0)
	    psym = qp_gpsym (qp, Memc[param])
	}

	# Get the field list for the user defined event structure.  This
	# defines the size of an event struct, lists the offset and type
	# of each field, and indicates which fields are to be used for X
	# and Y in positional accesses (unless already set in the paramex).

	dsym = strefstab (QP_ST(qp), S_DSYM(psym))
	nchars = S_NELEM(dsym)
	name = stname (QP_ST(qp), dsym)

	call salloc (flist, nchars, TY_CHAR)
	if (qp_gstr (qp, Memc[name], Memc[flist], nchars) < nchars)
	    call syserrs (SYS_QPBADVAL, Memc[name])

	if (qp_parsefl (qp, Memc[flist], IO_DD(io)) <= 0)
	    call syserrs (SYS_QPINVDD, Memc[name])
	else if (IO_EVXOFF(io) == NULL && IO_EVYOFF(io) == NULL) {
	    i = DD_XFIELD(dd)
	    j = DD_YFIELD(dd)
	    if (i == 0 || j == 0)
		call syserrs (SYS_QPNOXYF, Memc[name])

	    switch (DD_FTYPE(dd,i)) {
	    case TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE:
		IO_EVXTYPE(io) = DD_FTYPE(dd,i)
	    default:
		call syserrs (SYS_QPXYFNS, Memc[name])
	    }

	    switch (DD_FTYPE(dd,j)) {
	    case TY_SHORT, TY_INT, TY_LONG, TY_REAL, TY_DOUBLE:
		IO_EVYTYPE(io) = DD_FTYPE(dd,j)
	    default:
		call syserrs (SYS_QPXYFNS, Memc[name])
	    }

	    IO_EVXOFF(io) = DD_FOFFSET(dd,i)
	    IO_EVYOFF(io) = DD_FOFFSET(dd,j)
	}

	IO_EVENTLEN(io) = DD_STRUCTLEN(dd) * SZ_STRUCT / SZ_SHORT

	# Open the lfile used to store the event list.
	IO_FD(io) = qp_popen (qp, Memc[param], mode, BINARY_FILE)
	IO_LF(io) = S_LFILE(psym)
	IO_CHAN(io) = fstati (IO_FD(io), F_CHANNEL)
	IO_PSYM(io) = psym

	# The rest of the initialization is performed in the first call to
	# qpio_putev if we are writing a new event list.

	if (newlist) 			# EXIT if new event list
	    goto done_			# -----------------------

	fd = IO_FD(io)
	szb_page = QP_FMPAGESIZE(qp)
	nchars = szb_page / SZB_CHAR
	call salloc (eh, szb_page / (SZ_STRUCT*SZB_CHAR), TY_STRUCT)
	call aclri (Memi[eh], szb_page / (SZ_STRUCT*SZB_CHAR))

	# Read event list header.
	if (read (fd, Memi[eh], nchars) < nchars)
	    call syserrs (SYS_QPNOEH, Memc[param])

	IO_NEVENTS(io)	= EH_NEVENTS(eh)
	IO_EVENTLEN(io)	= EH_EVENTLEN(eh)
	IO_SZBBUCKET(io)= EH_SZBBUCKET(eh)
	IO_BUCKETLEN(io)= EH_BUCKETLEN(eh)
	IO_FBOFF(io)	= EH_FBOFF(eh)
	IO_EVMINOFF(io)	= EH_EVMINOFF(eh)
	IO_EVMAXOFF(io)	= EH_EVMAXOFF(eh)
	IO_INDEXLEN(io)	= EH_INDEXLEN(eh)
	IO_YOFFVOFF(io) = EH_YOFFVOFF(eh)
	IO_YOFFVLEN(io) = EH_YOFFVLEN(eh)
	IO_YLENVOFF(io) = EH_YLENVOFF(eh)
	IO_YLENVLEN(io) = EH_YLENVLEN(eh)
	IO_IXXOFF(io)	= EH_IXXOFF(eh)
	IO_IXYOFF(io)	= EH_IXYOFF(eh)
	IO_IXXTYPE(io)	= EH_IXXTYPE(eh)
	IO_IXYTYPE(io)	= EH_IXYTYPE(eh)

	# Copy the MINEVL event struct into the QPIO descriptor.
	nwords = IO_EVENTLEN(io)
	call malloc (IO_MINEVL(io), nwords, TY_SHORT)
	call amovs (Memi[eh+EH_MINEVLOFF(eh)], Mems[IO_MINEVL(io)],
	    IO_EVENTLEN(io))

	# Copy the MAXEVL event struct into the QPIO descriptor.
	call malloc (IO_MAXEVL(io), nwords, TY_SHORT)
	call amovs (Memi[eh+EH_MAXEVLOFF(eh)], Mems[IO_MAXEVL(io)],
	    IO_EVENTLEN(io))

	if (IO_DEBUG(io) > 0) {
	    call eprintf ("%s: nev=%d, szbk=%d, bklen=%d+2, ixlen=%d\n")
		call pargstr (Memc[param])
		call pargi (IO_NEVENTS(io))
		call pargi (IO_SZBBUCKET(io))
		call pargi (IO_BUCKETLEN(io))
		call pargi (IO_INDEXLEN(io))
	}

	# Get compressed event list index, if any.
	if (IO_INDEXLEN(io) > 0) {
	    call salloc (oo, IO_INDEXLEN(io) * 2, TY_SHORT)
	    call malloc (IO_YOFFVP(io), IO_INDEXLEN(io), TY_INT)
	    call malloc (IO_YLENVP(io), IO_INDEXLEN(io), TY_INT)

	    nchars = IO_YOFFVLEN(io) * SZ_SHORT
	    call seek (fd, IO_YOFFVOFF(io))
	    if (read (fd, Mems[oo], nchars) < nchars)
		call syserrs (SYS_QPBADIX, Memc[param])
	    junk = pl_l2pi (Mems[oo], 1, Memi[IO_YOFFVP(io)], IO_INDEXLEN(io))

	    nchars = IO_YLENVLEN(io) * SZ_SHORT
	    call seek (fd, IO_YLENVOFF(io))
	    if (read (fd, Mems[oo], nchars) < nchars)
		call syserrs (SYS_QPBADIX, Memc[param])
	    junk = pl_l2pi (Mems[oo], 1, Memi[IO_YLENVP(io)], IO_INDEXLEN(io))
	}

	# We won't need the file buffer any more, so free it.
	call fseti (fd, F_BUFSIZE, 0)

	# Compile the event attribute filter (EAF).  Always open the default
	# filter if one is provided with the datafile.  If the user has also
	# specified a filter, this will modify the default filter.

	if (IO_NODEFFILT(io) != YES) {
	    # Check for "deffilt.<evl>" first, then "deffilt".
	    call sprintf (Memc[deffilt], SZ_FNAME, "%s.%s")
		call pargstr (s_deffilt)
		call pargstr (Memc[param])
	    nchars = qp_lenf (qp, Memc[deffilt])
	    if (nchars <= 0) {
		call strcpy (s_deffilt, Memc[deffilt], SZ_FNAME)
		nchars = qp_lenf (qp, Memc[deffilt])
	    }

	    # Open the default filter if one was found.
	    if (nchars > 0) {
		call salloc (filter_text, nchars, TY_CHAR)
		if (qp_gstr(qp,Memc[deffilt],Memc[filter_text],nchars) < nchars)
		    call syserrs (SYS_QPBADVAL, Memc[deffilt])
		IO_EX(io) = qpex_open (qp, Memc[filter_text])
		IO_EXCLOSE(io) = YES
	    }
	}

	# Fold in the user specified filter if one was given.
	if (Memc[filter] != EOS) {
	    if (IO_EX(io) != NULL) {
		if (qpex_modfilter (IO_EX(io), Memc[filter]) == ERR)
		    call fprintf (STDERR,
			"Warning: error compiling QPIO filter\n")
	    } else {
		IO_EX(io) = qpex_open (qp, Memc[filter])
		IO_EXCLOSE(io) = YES
	    }

	    if (IO_DEBUG(io) > 0) {
		call eprintf ("event attribute filter: %s\n")
		    call pargstr (Memc[filter])
	    }
	}

	# Open the region mask.  This may be specified (named) in the parameter
	# expression, else we try to open a default mask.  If a mask is named,
	# the name may be the name of a header parameter containing the mask
	# as the stored array value (TY_OPAQUE parameter), the name of a header
	# parameter containing the name of the mask (TY_CHAR), or the name of
	# a mask storage file (.pl extension).

	# Make a copy of the user mask name, as qpio_loadmask will clobber it.
	call strcpy (Memc[mask], Memc[umask], SZ_FNAME)

	if (IO_NODEFMASK(io) != YES) {
	    # Check for "defmask.<evl>" first, then "defmask".
	    call sprintf (Memc[defmask], SZ_FNAME, "%s.%s")
		call pargstr (s_defmask)
		call pargstr (Memc[param])
	    nchars = qp_lenf (qp, Memc[defmask])
	    if (nchars <= 0) {
		call strcpy (s_defmask, Memc[defmask], SZ_FNAME)
		nchars = qp_lenf (qp, Memc[defmask])
	    }

	    if (nchars > 0)
		if (qp_gstr (qp, Memc[defmask], Memc[maskname], SZ_FNAME) > 0)
		    call qpio_loadmask (io, Memc[maskname], NO)
	}

	# Load user specified mask.
	if (Memc[umask] != EOS)
	    call qpio_loadmask (io, Memc[umask], YES)
	else if (IO_INDEXLEN(io) > 0)
	    call malloc (IO_RL(io), RL_LENELEM*2, TY_INT)

	# Allocate the bucket buffer.
	call malloc (IO_BP(io), IO_SZBBUCKET(io)/SZB_CHAR/SZ_SHORT, TY_SHORT)
done_
	# If no default rect was specified, set default bounding box for
	# reading to be the entire image.

	if (IO_BBUSED(io) == NO) {
	    IO_VSDEF(io,1) = 1;			IO_VSDEF(io,2) = 1
	    IO_VEDEF(io,1) = IO_NCOLS(io);	IO_VEDEF(io,2) = IO_NLINES(io)
	}

	# Initialize the active BB to the default.
	call amovi (IO_VSDEF(io,1), IO_VS(io,1), NDIM)
	call amovi (IO_VEDEF(io,1), IO_VE(io,1), NDIM)

} then {
	# We branch here if any nasty errors occur above.  Cleanup and free
	# the partially opened descriptor and pass the error on to whoever
	# called us.

	if (IO_BP(io) != NULL)
	    call mfree (IO_BP(io), TY_SHORT)
	if (IO_RL(io) != NULL)
	    call mfree (IO_RL(io), TY_INT)
	if (IO_PL(io) != NULL && IO_PLCLOSE(io) == YES)
	    call pl_close (IO_PL(io))
	if (IO_EX(io) != NULL)
	    call qpex_close (IO_EX(io))

	if (IO_YLENVP(io) != NULL)
	    call mfree (IO_YLENVP(io), TY_INT)
	if (IO_YOFFVP(io) != NULL)
	    call mfree (IO_YOFFVP(io), TY_INT)
	if (IO_MINEVL(io) != NULL)
	    call mfree (IO_MINEVL(io), TY_SHORT)
	if (IO_MAXEVL(io) != NULL)
	    call mfree (IO_MAXEVL(io), TY_SHORT)
	if (IO_FD(io) != NULL)
	    call close (IO_FD(io))

	if (IO_MASK(io) != NULL)
	    call mfree (IO_MASK(io), TY_CHAR)
	if (IO_PARAM(io) != NULL)
	    call mfree (IO_PARAM(io), TY_CHAR)
	if (IO_DD(io) != NULL)
	    call mfree (IO_DD(io), TY_STRUCT)

	if (filter != NULL)
	    call mfree (filter, TY_CHAR)
	if (io != NULL)
	    call mfree (io, TY_STRUCT)

	call erract (EA_ERROR)
}

	# The filter can be regenerated, so don't keep the input expr around.
	call mfree (filter, TY_CHAR)

	# Normal exit for read-only access.
	call sfree (sp)
	return (io)
end
