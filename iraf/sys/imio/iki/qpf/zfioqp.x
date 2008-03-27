# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<imhdr.h>
include	<imio.h>
include	<fio.h>
include	<qpioset.h>
include	"qpf.h"

# ZFIOQP -- QPF virtual file driver.  This driver presents to the caller a
# virtual file space containing a two dimensional array of type short or int
# pixels, wherein each "pixel" is a count of the number of events from a
# QPOE event list which map into that pixel.  An i/o request results in
# runtime filtering and sampling of the event list, mapping each event which
# passes the filter into the corresponding output pixel, and incrementing the
# value of that pixel to count the event.

# QPFZOP -- Open the file driver for i/o on the QPIO descriptor opened at
# qpf_open time.

procedure qpfzop (pkfn, mode, status)

char	pkfn[ARB]		#I packed virtual filename from FIO
int	mode			#I file access mode (ignored)
int	status			#O output status - i/o channel if successful

size_t	sz_val
int	ip, i
pointer	sp, fn, qpf
long	lval
int	ctol()

include "qpf.com"

begin
	call smark (sp)
	sz_val = SZ_FNAME
	call salloc (fn, sz_val, TY_CHAR)

	# The QPF descriptor is passed encoded in the pseudo filename as
	# "QPFxxxx" (decimal).  Extract this and return it as the i/o
	# channel for the driver.

	ip = 4
	sz_val = SZ_FNAME
	call strupk (pkfn, Memc[fn], sz_val)
	if (ctol (Memc[fn], ip, lval) <= 0)
	    status = ERR
	else
	    qpf = lval

	# Setup qpf address table
	do i = 0, num_qpf {
	    if ( i == num_qpf ) break
	    if ( Memp[qpf_ptrs0+i] == NULL ) break
	}
	if ( i == num_qpf ) {
	    sz_val = num_qpf + 1
	    call realloc (qpf_ptrs0, sz_val, TY_POINTER)
	    num_qpf = sz_val
	}
	Memp[qpf_ptrs0+i] = qpf
	qpf_ptrs = qpf_ptrs0 - 1
	status = i + 1
	#

	QPF_IOSTAT(qpf) = 0
	call sfree (sp)
end


# QPFZCL -- Close the QPF binary file driver.

procedure qpfzcl (chan, status)

int	chan			#I QPF i/o channel
int	status			#O output status

size_t	sz_val
int	i, i_max

include "qpf.com"

begin
	# Setup qpf address table
	Memp[qpf_ptrs+chan] = NULL
	i_max = -1
	do i = 0, num_qpf-1 {
	    if ( Memp[qpf_ptrs0+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (qpf_ptrs0, TY_POINTER)
	    qpf_ptrs0 = NULL
	} else {
	    call realloc (qpf_ptrs0, sz_val, TY_POINTER)
	}
	qpf_ptrs = qpf_ptrs0 - 1
	num_qpf = sz_val
	#

	status = OK
end


# QPFZRD -- Read a segment of the virtual pixel array into the output buffer,
# i.e., zero the output buffer and sample the event list, accumulating counts
# in the output array.

procedure qpfzrd (chan, obuf, nbytes, boffset)

int	chan			#I QPF i/o channel
char	obuf[ARB]		#O output buffer
size_t	nbytes			#I nbytes to be read
long	boffset			#I file offset at which read commences

pointer	qpf, im, io
long	vs[2], ve[2]
real	xblock, yblock
int	szb_pixel, nev
long	xoff, yoff
long	ncols, pixel
int	sizeof(), qpio_readpixs(), qpio_readpixi()
long	modl()

include "qpf.com"

begin
	qpf = Memp[qpf_ptrs+chan]
	im  = QPF_IM(qpf)
	io  = QPF_IO(qpf)

	xblock = QPF_XBLOCK(qpf)
	yblock = QPF_YBLOCK(qpf)
	ncols = IM_PHYSLEN(im,1)
	xoff  = QPF_VS(qpf,1)
	yoff  = QPF_VS(qpf,2)
	szb_pixel = sizeof(IM_PIXTYPE(im)) * SZB_CHAR

	# Convert boffset, nbytes to vs, ve.
	pixel = (boffset - 1) / szb_pixel
	vs[1] = (modl (pixel, ncols)) * xblock + xoff
	vs[2] = (pixel / ncols) * yblock + yoff

	pixel = (boffset-1 + nbytes - szb_pixel) / szb_pixel
	ve[1] = (modl (pixel, ncols)) * xblock + (xblock-1) + xoff
	ve[2] = (pixel / ncols) * yblock + (yblock-1) + yoff

	# Call readpix to sample image into the output buffer.  Zero the buffer
	# first since the read is additive.

	call aclrc (obuf, nbytes / SZB_CHAR)
	iferr {
	    switch (IM_PIXTYPE(im)) {
	    case TY_SHORT:
		nev = qpio_readpixs (io, obuf, vs, ve, 2, xblock, yblock)
	    case TY_INT:
		# arg2: incompatible pointer
		nev = qpio_readpixi (io, obuf, vs, ve, 2, xblock, yblock)
	    }
	} then {
	    QPF_IOSTAT(qpf) = ERR
	} else
	    QPF_IOSTAT(qpf) = nbytes
end


# QPFZWR -- Write to the virtual pixel array.  QPF permits only read-only
# access, but we ignore write requests, so return OK and do nothing if this
# routine is called.

procedure qpfzwr (chan, ibuf, nbytes, boffset)

int	chan			#I QPF i/o channel
char	ibuf[ARB]		#O datg buffer
size_t	nbytes			#I nbytes to be written
long	boffset			#I file offset to write at

pointer	qpf

include "qpf.com"

begin
	qpf = Memp[qpf_ptrs+chan]
	QPF_IOSTAT(qpf) = nbytes
end


# QPFZWT -- Return the number of virtual bytes transferred in the last i/o
# request.

procedure qpfzwt (chan, status)

int	chan			#I QPF i/o channel
long	status			#O i/o channel status

pointer	qpf

include "qpf.com"

begin
	qpf = Memp[qpf_ptrs+chan]
	status = QPF_IOSTAT(qpf)
end


# QPFZST -- Query device/file parameters.

procedure qpfzst (chan, param, value)

int	chan			#I QPF i/o channel
int	param			#I parameter to be returned
long	value			#O parameter value

pointer	qpf, im, io
int	szb_pixel
long	npix
long	qpio_statl()
int	sizeof()

include "qpf.com"

begin
	qpf = Memp[qpf_ptrs+chan]
	im = QPF_IM(qpf)
	io = QPF_IO(qpf)
	npix = IM_PHYSLEN(im,1) * IM_PHYSLEN(im,2)
	szb_pixel = sizeof(IM_PIXTYPE(im)) * SZB_CHAR

	switch (param) {
	case FSTT_BLKSIZE:
	    value = 1
	case FSTT_FILSIZE:
	    value = npix * szb_pixel
	case FSTT_OPTBUFSIZE:
	    value = min (npix*szb_pixel, qpio_statl(io,QPIO_OPTBUFSIZE))
	case FSTT_MAXBUFSIZE:
	    value = npix * szb_pixel
	default:
	    value = ERR
	}
end
