# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<error.h>
include	<mach.h>
include	<imhdr.h>
include	<imio.h>
include	<qpset.h>
include	<qpioset.h>
include	"qpf.h"

# QPF_OPEN -- Open a QPOE image.  New QPOE images can only be written by
# calling QPOE directly; under IMIO, only READ_ONLY access is supported.

procedure qpf_open (kernel, im, o_im,
	root, extn, ksection, cl_index, cl_size, acmode, status)

int	kernel			#I IKI kernel
pointer	im			#I image descriptor
pointer	o_im			#I [not used]
char	root[ARB]		#I root image name
char	extn[ARB]		#I filename extension
char	ksection[ARB]		#I QPIO filter expression
int	cl_index		#I [not used]
int	cl_size			#I [not used]
int	acmode			#I [not used]
int	status			#O ok|err

size_t	sz_val
int	n
real	xblock, yblock, tol
pointer	sp, qp, io, v, fname, qpf
long	c_1
int	initialized

pointer	qp_open, qpio_open()
real	qpio_statr(), qp_statr()
int	qpio_getrange(), qp_geti(), qp_gstr(), qp_lenf()
long	lnint()
define	err_ 91

include "qpf.com"
include	<nullptr.inc>
data initialized /0/

begin
	c_1 = 1

	# Setup qpf address table
	if ( initialized == 0 ) {
	    initialized = 1
	    qpf_ptrs0 = NULL
	    num_qpf = 0
	}

	call smark (sp)
	sz_val = SZ_PATHNAME
	call salloc (fname, sz_val, TY_CHAR)
	sz_val = SZ_FNAME
	call salloc (v, sz_val, TY_CHAR)

	io = NULL
	qp = NULL
	qpf = NULL
	tol = EPSILONR * 100

	# The only valid cl_index for a QPOE image is -1 (none specified) or 1.
	if (!(cl_index < 0 || cl_index == 1))
	    goto err_

	sz_val = LEN_QPFDES
	call malloc (qpf, sz_val, TY_STRUCT)

	# Open the QPOE file.
	call iki_mkfname (root, extn, Memc[fname], SZ_PATHNAME)
	iferr (qp = qp_open (Memc[fname], READ_ONLY, NULLPTR)) {
	    qp = NULL
	    goto err_
	}

	# Open the event list under QPIO for sampled (pixel) i/o.
	iferr (io = qpio_open (qp, ksection, READ_ONLY))
	    io = NULL

	# Determine the data range and pixel type.
	iferr (IM_CTIME(im) = qp_geti (qp, "cretime"))
	    IM_CTIME(im) = 0
	iferr (IM_MTIME(im) = qp_geti (qp, "modtime"))
	    IM_MTIME(im) = 0
	iferr (IM_LIMTIME(im) = qp_geti (qp, "limtime"))
	    IM_LIMTIME(im) = 0

	# The min and max pixel values for a sampled event file depend
	# strongly on the blocking factor, which is a runtime variable.
	# Ideally when the poefile is written the vectors 'datamin' and
	# 'datamax' should be computed for the main event list.  These
	# give the min and max pixel values (counts/pixel) for each blocking
	# factor from 1 to len(data[min|max]), i.e., the blocking factor
	# serves as the index into these vectors.

	if (io != NULL) {
	    xblock = max (1.0, qpio_statr (io, QPIO_XBLOCKFACTOR))
	    yblock = max (1.0, qpio_statr (io, QPIO_YBLOCKFACTOR))
	} else {
	    xblock = max (1.0, qp_statr (qp, QPOE_XBLOCKFACTOR))
	    yblock = max (1.0, qp_statr (qp, QPOE_YBLOCKFACTOR))
	}
	call strcpy ("datamax", Memc[v], SZ_FNAME)
	n = qp_lenf (qp, Memc[v])

	if (n >= max(xblock,yblock)) {
	    call sprintf (Memc[v+7], SZ_FNAME-7, "[%d]")
		call pargl (lnint((xblock+yblock)/2.0))
	    IM_MAX(im) = qp_geti (qp, Memc[v])
	    Memc[v+5] = 'i';  Memc[v+6] = 'n'
	    IM_MIN(im) = qp_geti (qp, Memc[v])
	} else
	    IM_LIMTIME(im) = 0

	# Set the image pixel type.  This is arbitrary, provided we have
	# enough dynamic range to represent the maximum pixel value.

	IM_PIXTYPE(im) = TY_INT
	if (IM_LIMTIME(im) != 0 && IM_LIMTIME(im) >= IM_MTIME(im))
	    if (int(IM_MAX(im)) <= MAX_SHORT)
		IM_PIXTYPE(im) = TY_SHORT

	# Set the image size parameters.  If the user has specified a rect
	# within which i/o is to occur, set the logical image size to the
	# size of the rect rather than the full matrix.

	if (io != NULL) {
	    IM_NDIM(im)  = qpio_getrange (io, QPF_VS(qpf,1), QPF_VE(qpf,1), 2)
	    IM_LEN(im,1) = (QPF_VE(qpf,1) - QPF_VS(qpf,1) + 1) / xblock + tol
	    IM_LEN(im,2) = (QPF_VE(qpf,2) - QPF_VS(qpf,2) + 1) / yblock + tol
	} else {
	    IM_NDIM(im)  = 2
	    IM_LEN(im,1) = qp_geti (qp, "axlen[1]") / xblock + tol
	    IM_LEN(im,2) = qp_geti (qp, "axlen[2]") / yblock + tol
	    QPF_VS(qpf,1) = 1;	QPF_VE(qpf,1) = IM_LEN(im,1)
	    QPF_VS(qpf,2) = 1;	QPF_VE(qpf,2) = IM_LEN(im,2)
	}
	call imioff (im, c_1, YES, c_1)

	iferr (n = qp_gstr (qp, "title", IM_TITLE(im), SZ_IMTITLE))
	    IM_TITLE(im) = EOS
	iferr (n = qp_gstr (qp, "history", IM_HISTORY(im), SZ_IMHIST))
	    IM_HISTORY(im) = EOS

	call strcpy (root, IM_HDRFILE(im), SZ_IMHDRFILE)
	IM_PIXFILE(im) = EOS
	IM_HFD(im) = NULL
	IM_PFD(im) = NULL

	# Set up the QPF descriptor.
	QPF_IM(qpf)	= im
	QPF_QP(qpf)	= qp
	QPF_IO(qpf)	= io
	QPF_XBLOCK(qpf)	= xblock
	QPF_YBLOCK(qpf)	= yblock
	QPF_IOSTAT(qpf)	= 0

	IM_KDES(im) = qpf

	# Copy any scalar QPOE file header parameters into the IMIO header.
	iferr (call qpf_copyparams (im, qp))
	    call erract (EA_WARN)

	status = OK
	call sfree (sp)
	return

err_
	# Error abort.
	if (io != NULL)
	    call qpio_close (io)
	if (qp != NULL)
	    call qp_close (qp)

	call mfree (qpf, TY_STRUCT)
	IM_KDES(im) = NULL

	status = ERR
	call erract (EA_WARN)
	call sfree (sp)
end
