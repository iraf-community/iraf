# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <syserr.h>
include <fmset.h>
include "qpoe.h"
include "qpio.h"

# QP_OPEN -- Open or create a QPOE datafile.  This routine must be called
# before the poefile can be accessed.  In the case of a create, the file
# parameters are not fixed until the first i/o or header access occurs,
# allowing one to use QP_SET calls to modify the file parameters after the
# open.

pointer procedure qp_open (poefile, mode, o_qp)

char	poefile[ARB]		#I QPOE file to be opened
int	mode			#I file access mode
pointer o_qp			#I reference file, if NEW_COPY

int	fmmode, fd, n
pointer sp, qph, qp, fname, fm

real	qp_getr()
pointer fm_open(), strestore(), qm_access()
int	fm_fopen(), read(), fm_stati(), qp_accessf()
errchk	fm_open, strestore, fm_fopen, seek, read
errchk	calloc, syserrs, qm_access

string	s_defblock DEF_BLOCK
string	s_defxblock DEF_XBLOCK
string	s_defyblock DEF_YBLOCK

begin
	call smark (sp)
	call salloc (qph, LEN_QPH, TY_STRUCT)
	call salloc (fname, SZ_PATHNAME, TY_CHAR)

	# Construct the filename (with extension .qp) of the poefile.
	call qp_mkfname (poefile, QPOE_EXTN, Memc[fname], SZ_PATHNAME)

	# Open or create the poefile via the file manager.
	fmmode = mode
	if (mode == NEW_COPY)
	    fmmode = NEW_FILE
	fm = fm_open (Memc[fname], fmmode)

	# Allocate the QPOE descriptor.
	call calloc (qp, LEN_QPDES, TY_STRUCT)
	call strcpy (Memc[fname], QP_DFNAME(qp), SZ_QPDFNAME)

	# Access the global macro database, and set the default values of
	# all interface and datafile parameters.

	QP_QM(qp) = qm_access()
	call qm_setdefaults (QP_QM(qp), qp)

	QP_MODE(qp) = mode
	QP_OQP(qp) = o_qp
	QP_FM(qp) = fm

	if (mode == NEW_FILE || mode == NEW_COPY) {
	    # Initialize the descriptor for a new poefile.  The file
	    # attributes are not fixed until the file is accessed, to
	    # allow time to change the defaults with qp_seti.

	    QP_MAGIC(qp) = QPOE_MAGIC
	    QP_VERSION(qp) = QPOE_VERSION

	    if (mode == NEW_COPY) {
		# Inherit datafile defaults from parent file.
		QP_BUCKETLEN(qp)	= QP_BUCKETLEN(o_qp)
		QP_FMMAXLFILES(qp)	= QP_FMMAXLFILES(o_qp)
		QP_FMMAXPTPAGES(qp)	= QP_FMMAXPTPAGES(o_qp)
		QP_FMPAGESIZE(qp)	= QP_FMPAGESIZE(o_qp)
		QP_FMCACHESIZE(qp)	= QP_FMCACHESIZE(o_qp)
		QP_STINDEXLEN(qp)	= QP_STINDEXLEN(o_qp)
		QP_STSTABLEN(qp)	= QP_STSTABLEN(o_qp)
		QP_STSBUFSIZE(qp)	= QP_STSBUFSIZE(o_qp)
	    }

	    QP_ACTIVE(qp) = NO

	} else {
	    # Open an existing poefile.  The encoded QPOE header and
	    # symbol table are stored in a binary lfile in the datafile.

	    fd = fm_fopen (fm, LF_QPOE, READ_ONLY, BINARY_FILE)

	    # Read the QPOE file header.
	    n = LEN_QPH * SZ_STRUCT
	    call aclri (Memi[qph], LEN_QPH)
	    if (read (fd, Memi[qph], n) < n)
		call syserrs (SYS_QPBADFILE, QP_DFNAME(qp))
	    call miiupk32 (Memi[qph], Memi[qph], LEN_QPH, TY_STRUCT)

	    QP_MAGIC(qp)	= QPH_MAGIC(qph)
	    QP_VERSION(qp)	= QPH_VERSION(qph)
	    QP_STOFFSET(qp)	= QPH_STOFFSET(qph)

	    if (QP_MAGIC(qp) != QPOE_MAGIC)
		call syserrs (SYS_QPBADFILE, QP_DFNAME(qp))

	    # Read the stored symbol table.
	    call seek (fd, QP_STOFFSET(qp))
	    QP_ST(qp) = strestore (fd)

	    # Initialize any remaining QP descriptor parameters.
	    QP_FMPAGESIZE(qp) = fm_stati (fm, FM_PAGESIZE)
	    call fm_seti (fm, FM_FCACHESIZE, DEF_FMCACHESIZE)
	    QP_ACTIVE(qp) = YES

	    # See if the default block factor is set in the datafile header.
	    if (qp_accessf (qp, s_defblock) == YES) {
		QP_XBLOCK(qp) = qp_getr (qp, s_defblock)
		QP_YBLOCK(qp) = QP_XBLOCK(qp)
	    }
	    if (qp_accessf (qp, s_defxblock) == YES)
		QP_XBLOCK(qp) = qp_getr (qp, s_defxblock)
	    if (qp_accessf (qp, s_defyblock) == YES)
		QP_YBLOCK(qp) = qp_getr (qp, s_defyblock)

	    call close (fd)
	}

	# Allow any interface parameters set explicitly in global macro SET
	# statements to override the inherited or datafile values set above.

	call qm_upddefaults (QP_QM(qp), qp)

	call sfree (sp)
	return (qp)
end
