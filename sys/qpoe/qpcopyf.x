# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpset.h>
include	"qpoe.h"

define	MAX_NELEM	8192	# copy unit (chunk) size

# QP_COPYF -- Copy a field (parameter), either within a datafile, or from one
# datafile to another.

procedure qp_copyf (o_qp, o_param, n_qp, n_param)

pointer	o_qp			#I QPOE descriptor of old (input) datafile
char	o_param[ARB]		#I input parameter name
pointer	n_qp			#I QPOE descriptor of new (output) datafile
char	n_param[ARB]		#I output parameter name

pointer	sp, dp, cp, buf
int	nelem, elsize, chunk, nleft, first, maxelem, flags
int	qp_queryf(), qp_accessf(), qp_elementsize(), qp_read()
errchk	qp_queryf, qp_addf, qp_read, qp_write

begin
	call smark (sp)
	call salloc (dp, SZ_DATATYPE, TY_CHAR)
	call salloc (cp, SZ_COMMENT, TY_CHAR)

	# Get parameter attributes and create new parameter if necessary.
	nelem = qp_queryf (o_qp, o_param, Memc[dp], maxelem, Memc[cp], flags)
	if (qp_accessf (n_qp, n_param) == NO)
	    call qp_addf (n_qp, n_param, Memc[dp], maxelem, Memc[cp], flags)

	# Copy parameter value.
	if (nelem > 0) {
	    elsize = qp_elementsize (o_qp, Memc[dp], INSTANCEOF)
	    chunk = min (MAX_NELEM, nelem)
	    call salloc (buf, chunk * elsize, TY_CHAR)

	    first = 1
	    for (nleft=nelem;  nleft > 0;  nleft=nleft-nelem) {
		nelem = qp_read (o_qp,o_param, Memc[buf], chunk,first, Memc[dp])
		call   qp_write (n_qp,n_param, Memc[buf], nelem,first, Memc[dp])
		first = first + nelem
	    }
	}

	call sfree (sp)
end
