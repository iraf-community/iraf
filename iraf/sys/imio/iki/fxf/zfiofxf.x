# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include <mach.h>
include <knet.h>
include <fio.h>
include <fset.h>
include <imio.h>
include <imhdr.h>
include	"fxf.h"

# ZFIOFXF -- FITS kernel virtual file driver.   This maps the actual
# FITS file into the virtual pixel file expected by IMIO.


# FXFZOP -- Open the file driver for i/o.  The filename has appended the
# string "_nnnnn", where 'nnnnn' is the FIT descriptor to the structure
# defined in "fit.h".

procedure fxfzop (pkfn, mode, status)

char	pkfn[ARB]		#I packed virtual filename from FIO
int	mode			#I file access mode (ignored)
int	status			#O output status - i/o channel if successful

size_t	sz_val
long	lval
pointer im, fit
int	ip, indx, channel, i
bool	lscale, lzero
#bool	bfloat
int	strldx(), ctol()
bool	fxf_fpl_equald()
char    fname[SZ_PATHNAME]

include "fxf.com"

begin
	# Separate the FIT descriptor from the file name.
	sz_val = SZ_PATHNAME
	call strupk (pkfn, fname, sz_val)

        ip = strldx ("_", fname)
	indx = ip + 1
	if (ctol (fname, indx, lval) <= 0) {
	   status = ERR
	   return
	}
	fit = lval

	# Determine if we have a Fits Kernel non supported
	# data format; i.e. Bitpix -32 or -64 and BSCALE and/or
	# BZERO with non default values.

	### Only "low level" routines can be falled from a file driver:
	### high level routines like syserr cannot be used due to
	### recursion/reentrancy problems.
	# We are calling syserrs at this level because we want to
	# give the application the freedom to manipulate the FITS header
	# at will and not imposing restriction at that level.

	im = FIT_IM(fit)
	lscale = fxf_fpl_equald (1.0d0, FIT_BSCALE(fit), 1)
	lzero =  fxf_fpl_equald (0.0d0, FIT_BZERO(fit), 1)

	# Determine if scaling is necessary.
	#bfloat = (!lscale || !lzero)
	#if (bfloat && (FIT_BITPIX(fit) == -32 || FIT_BITPIX(fit) == -64)) {
	#    FIT_IOSTAT(fit) = ERR
	#    #call syserrs (SYS_FXFRDHSC,IM_HDRFILE(im))
	#    status = ERR
	#    return
	#}

	fname[ip] = EOS
	sz_val = SZ_PATHNAME
	call strpak (fname, fname, sz_val)

	# Open the file.
	call zopnbf (fname, mode, channel)
	if (channel == ERR) {
	   status = ERR
	   return
	}

	# Setup fit address table
	do i = 0, num_fit {
	    if ( i == num_fit ) break
	    if ( Memp[fit_ptrs0+i] == NULL ) break
	}
	if ( i == num_fit ) {
	    sz_val = num_fit + 1
	    call realloc (fit_ptrs0, sz_val, TY_POINTER)
	    num_fit = sz_val
	}
	Memp[fit_ptrs0+i] = fit
	fit_ptrs = fit_ptrs0 - 1
	status = i + 1
	#

	FIT_IO(fit) = channel
end


# FITZCL -- Close the FIT binary file driver.

procedure fxfzcl (chan, status)

int	chan			#I FIT i/o channel
int	status			#O output status

size_t	sz_val
pointer fit
int	i, i_max

include "fxf.com"

begin
	fit = Memp[fit_ptrs+chan]
	call zclsbf (FIT_IO(fit), status)

	# Setup fit address table
	Memp[fit_ptrs+chan] = NULL
	i_max = -1
	do i = 0, num_fit-1 {
	    if ( Memp[fit_ptrs0+i] != NULL ) i_max = i
	}
	sz_val = i_max + 1
	if ( sz_val == 0 ) {
	    call mfree (fit_ptrs0, TY_POINTER)
	    fit_ptrs0 = NULL
	} else {
	    call realloc (fit_ptrs0, sz_val, TY_POINTER)
	}
	fit_ptrs = fit_ptrs0 - 1
	num_fit = sz_val
	#
end


# FXFZRD --  Read the FIT file (header and pixel data).  An offset pointer
# needs to be set to point to the data portion of the file. If we are reading
# pixel data, the scale routine fxf_unpack_data is called.  We need to keep
# a counter (npix_read) with the current number of pixels unpacked since we
# don't want to convert beyond the total number of pixels; where the last
# block of data read can contain zeros or garbage up to a count of 2880 bytes.

procedure fxfzrd (chan, obuf, nbytes, boffset)

int	chan			#I FIT i/o channel
char	obuf[ARB]		#O output buffer
size_t	nbytes			#I nbytes to be read
long	boffset			#I file offset at which read commences

pointer fit, im
int	pixtype, i
long	ip, nb
long	status, totpix, datasizeb, pixoffb, nb_skipped
size_t	npix
#double  dtemp
#real    rtemp, rscale, roffset
int	sizeof()

include "fxf.com"

begin
	fit = Memp[fit_ptrs+chan]
	im = FIT_IM(fit)
	FIT_IOSTAT(fit) = OK

	totpix = IM_PHYSLEN(im,1)
	do i = 2, IM_NPHYSDIM(im)
	    totpix = totpix * IM_PHYSLEN(im,i)

	if (FIT_ZCNV(fit) == YES) {
	    if (FIT_PIXTYPE(fit) != TY_REAL && FIT_PIXTYPE(fit) != TY_DOUBLE) {
	       call fxf_cnvpx (im, totpix, obuf, nbytes, boffset)
	       return
	    } 
	}

	pixtype = IM_PIXTYPE(im)
	datasizeb = totpix * (sizeof(pixtype) * SZB_CHAR)
	pixoffb = (FIT_PIXOFF(fit) - 1) * SZB_CHAR + 1

	# We can read the data directly into the caller's output buffer as 
	# any FITS kernel input conversions are guaranteed to not make the
	# data smaller.  

	call zardbf (FIT_IO(fit), obuf, nbytes, boffset)
	call zawtbf (FIT_IO(fit), status)
	if (status == ERR) {
	    FIT_IOSTAT(fit) = ERR
	    return
	}

	### boffset is 1-indexed, so one would expect (boffset/SZB_CHAR) to
	### be ((boffset - 1) * SZB_CHAR + 1).  This is off by one from what
	### is being calculated, so if PIXOFF and boffset point to the same
	### place IP will be one, which happens to be the correct array index. 
	### Nonehtless expressions like this should be written out so that
	### they can be verified easily by reading them.  Any modern compiler
	### will optimize the expression, we don't have to do this in the
	### source code.

	ip = FIT_PIXOFF(fit) - boffset/SZB_CHAR
	if (ip <= 0)
	    ip = 1

	nb_skipped = boffset - pixoffb
	if (nb_skipped <= 0)
	    nb = min (status + nb_skipped, datasizeb)
	else
	    nb = min (status, datasizeb - nb_skipped)
	npix = max (0, nb / (sizeof(pixtype) * SZB_CHAR))

	if (FIT_ZCNV(fit) == YES) {
	    if (FIT_PIXTYPE(fit) == TY_REAL) {
		# This is for scaling -32 (should not be allowed)
		# arg1: incompatible pointer
		call fxf_zaltrr(obuf[ip], npix, FIT_BSCALE(fit), FIT_BZERO(fit))
            } else if (FIT_PIXTYPE(fit) == TY_DOUBLE) {
	        # This is for scaling -64 data (should not be allowed)
		# arg1: incompatible pointer
		call fxf_zaltrd(obuf[ip], npix, FIT_BSCALE(fit), FIT_BZERO(fit))
            }	    
        } else {
	   call fxf_unpack_data (obuf[ip],
	       npix, pixtype, FIT_BSCALE(fit), FIT_BZERO(fit)) 
        }
end

procedure fxf_zaltrr (data, npix, bscale, bzero)

real data[ARB]
size_t	npix
double bscale, bzero

long	i

begin
	call ieevupkr (data, data, npix)
        do i = 1, npix {
	   data[i] = data[i] * bscale + bzero
        }
end


procedure fxf_zaltrd (data, npix, bscale, bzero)

double data[ARB]
size_t	npix
double bscale, bzero

long	i

begin
	call ieevupkd (data, data, npix)
        do i = 1, npix
	   data[i] = data[i] * bscale + bzero
end


 
# FXFZWR -- Write to the output file. 

procedure fxfzwr (chan, ibuf, nbytes, boffset)

int	chan			#I QPF i/o channel
char	ibuf[ARB]		#O data buffer
size_t	nbytes			#I nbytes to be written
long	boffset			#I file offset

size_t	sz_val
pointer fit, im, sp, obuf
bool	noconvert, lscale, lzero, bfloat
int	pixtype, i
long	ip, op, totpix, nb, nchars
long	datasizeb, pixoffb, nb_skipped
size_t	obufsize, npix

bool	fxf_fpl_equald()
int	sizeof()

include "fxf.com"

begin
	fit = Memp[fit_ptrs+chan]
	im = FIT_IM(fit)
	FIT_IOSTAT(fit) = OK

	# We don't have to pack the data if it is integer and we don't need
	# to byte swap; the data buffer can be written directly out.


	# Determine if we are writing into an scaled floating point data
	# unit; i.e. bitpix > 0 and BSCALE or/and BZERO with non default
	# values.  This is an error since we are not supporting this
	# combination for writing at this time.

	lscale = fxf_fpl_equald (1.0d0, FIT_BSCALE(fit), 1)
	lzero =  fxf_fpl_equald (0.0d0, FIT_BZERO(fit), 1)

	# Determine if scaling is necessary.
	bfloat = (!lscale || !lzero)
	if (bfloat &&
		(IM_PIXTYPE(im) == TY_REAL || IM_PIXTYPE(im) == TY_DOUBLE)) {
	    FIT_IOSTAT(fit) = ERR
	    return
	}

	pixtype = IM_PIXTYPE(im)
	noconvert = ((pixtype == TY_SHORT && BYTE_SWAP2 == NO) ||
	    ((pixtype == TY_INT || pixtype == TY_LONG) && BYTE_SWAP4 == NO))

	if (noconvert) {
	    call zawrbf (FIT_IO(fit), ibuf, nbytes, boffset)
	    return
	}

	# Writing pixel data to an image is currently illegal if on-the-fly
	# conversion is in effect, as on-the-fly conversion is currently only
	# available for reading.

	if (FIT_ZCNV(fit) == YES) {
	    FIT_IOSTAT(fit) = ERR
	    return
        }		

	totpix = IM_PHYSLEN(im,1)
	do i = 2, IM_NPHYSDIM(im)
	    totpix = totpix * IM_PHYSLEN(im,i)

	datasizeb = totpix * (sizeof(pixtype) * SZB_CHAR)
	pixoffb = (FIT_PIXOFF(fit) - 1) * SZB_CHAR + 1

	### Same comments as for fxfzrd apply here.
	### There doesn't appear to be any support here for byte data like
	### in fxfzwr.  This must mean that byte images are read-only.
	### This shouldn't be necessary, but we shouldn't try to do anything
	### about it until the fxf_byte_short issue is addressed.

	ip = FIT_PIXOFF(fit) - boffset / SZB_CHAR
	if (ip <= 0)
	    ip = 1

	nb_skipped = boffset - pixoffb
	if (nb_skipped <= 0)
	    nb = min (nbytes + nb_skipped, datasizeb)
	else
	    nb = min (nbytes, datasizeb - nb_skipped)
	npix = max (0, nb / (sizeof(pixtype) * SZB_CHAR))

	if (npix == 0) 
	    return

	# We don't do scaling (e.g. BSCALE/BZERO) when writing.  All the
	# generated FITS files in this interface are ieee fits standard.
	### I didn't look into it but I don't understand this; when accessing
	### a BSCALE image read-write, it should be necessary to scale both
	### when reading and writing if the application sees TY_REAL pixels.
	### When writing a new image I suppose the application would take
	### care of any scaling.

	# Convert any pixel data in the input buffer to the binary format
	# required for FITS and write it out.  Any non-pixel data in the
	# buffer should be left as-is.

	obufsize = (nbytes + SZB_CHAR-1) / SZB_CHAR

	call smark (sp)
	call salloc (obuf, obufsize, TY_CHAR)

	# Preserve any leading non-pixel data.
	op = 1
	if (ip > 1) {
	    nchars = min (obufsize, ip - 1)
	    sz_val = nchars
	    call amovc (ibuf[1], Memc[obuf], sz_val)
	    op = op + nchars
	}

	# Convert and output the pixels.
	call fxf_pak_data (ibuf[ip], Memc[obuf+op-1], npix, pixtype)
	op = op + npix * sizeof(pixtype)

	# Preserve any remaining non-pixel data.
	nchars = obufsize - op + 1
	if (nchars > 0) {
	    sz_val = nchars
	    call amovc (ibuf[op], Memc[obuf+op-1], sz_val)
	}

	# Write out the data.
	call zawrbf (FIT_IO(fit), Memc[obuf], nbytes, boffset)

	call sfree (sp)
end


# FXFZWT -- Return the number of bytes transferred in the last i/o request.

procedure fxfzwt (chan, status)

int	chan			#I QPF i/o channel
long	status			#O i/o channel status

pointer fit, im

include "fxf.com"

begin
	fit = Memp[fit_ptrs+chan]
	im = FIT_IM(fit)

	# A file driver returns status for i/o only in the AWAIT routine;
	# hence any i/o errors occurring in the FK itself are indicated by
	# setting FIT_IOSTAT.  Otherwise the actual i/o operation must have
	# been submitted, and we call zawtbf to wait for i/o, and get status.

	if (FIT_IOSTAT(fit) != OK)
	    status = FIT_IOSTAT(fit)
	else
	    call zawtbf (FIT_IO(fit), status)

	# FIT_ZBYTES has the correct number of logical bytes that need
	# to be passed to fio since we are expanding the buffer size
	# from byte to short or real and short to real.

	if (status > 0) {
	    if (FIT_PIXTYPE(fit) == TY_UBYTE)
		status = FIT_ZBYTES(fit)
	    else if (FIT_PIXTYPE(fit) == TY_SHORT && IM_PIXTYPE(im) == TY_REAL)
		status = FIT_ZBYTES(fit)
	}
end


# FXFZST -- Query device/file parameters.

procedure fxfzst (chan, param, value)

int	chan			#I FIT i/o channel
int	param			#I parameter to be returned
long	value			#O parameter value

pointer fit, im
long	totpix, lval
int	i, szb_pixel, szb_real
int	sizeof()

include "fxf.com"

begin
	fit = Memp[fit_ptrs+chan]
	im = FIT_IM(fit)

    	totpix = IM_PHYSLEN(im,1)
	do i = 2, IM_NPHYSDIM(im)
	    totpix = totpix * IM_PHYSLEN(im,i)

        szb_pixel = sizeof(IM_PIXTYPE(im)) * SZB_CHAR
	szb_real = SZ_REAL * SZB_CHAR

	call zsttbf (FIT_IO(fit), param, value)

	if (param == FSTT_FILSIZE) {
	    switch (FIT_PIXTYPE(fit)) {
	    case TY_SHORT:
		if (IM_PIXTYPE(im) == TY_REAL) {
		    lval = (totpix * SZ_SHORT * SZB_CHAR) / 2880. + .5
		    value = value + lval * 2880
		}
	    case TY_UBYTE:
		if (IM_PIXTYPE(im) == TY_SHORT) {
		    lval = totpix/2880. + 0.5
		    value = value + lval * 2880
	        } else if (IM_PIXTYPE(im) == TY_REAL) {
		    lval = totpix*(szb_real-1)/2880. + 0.5
		    value = value + lval * 2880
		}
            }
	}
end


# FXF_CNVPX -- Convert FITS type BITPIX = 8 to SHORT or REAL depending
# on the value of BSCALE, BZERO (1, 32768 is already iraf supported as ushort
# and is not treated in here). If BITPIX=16 and BSCALE and BZERO are 
# non-default then the pixels are converted to REAL.

procedure fxf_cnvpx (im, totpix, obuf, nbytes, boffset)

pointer im			#I Image descriptor
long	totpix			#I Total number of pixels		
char    obuf[ARB]		#O Output data buffer
size_t	nbytes			#I Size in bytes of the output buffer
long	boffset			#I Byte offset into the virtual image

pointer sp, buf, fit, op
double	bscale, bzero
int	pfactor, pixtype
long	ip, nelem, nb, bzoff, nboff
long	status, offset, datasizeb, pixoffb, nb_skipped
size_t	buf_size, npix, c_1
int	sizeof()

begin
	c_1 = 1

	fit = IM_KDES(im)
	bscale = FIT_BSCALE(fit)
	bzero  = FIT_BZERO(fit)

	ip = FIT_PIXOFF(fit) - boffset/SZB_CHAR
	if (ip <= 0)
	    ip = 1

	# The beginning of the data portion in bytes.
	pixoffb = (FIT_PIXOFF(fit)-1) * SZB_CHAR + 1
	
	# Determine the factor to applied: size(im_pixtype)/size(fit_pixtype)
	if (FIT_PIXTYPE(fit) == TY_UBYTE) {
	    if (IM_PIXTYPE(im) == TY_REAL)
		pfactor = SZ_REAL * SZB_CHAR
	    else  					# TY_SHORT
		pfactor = SZB_CHAR
	    datasizeb = totpix
	} else if (FIT_PIXTYPE(fit) == TY_SHORT) {
	    pfactor = SZ_REAL / SZ_SHORT
	    pixtype = TY_SHORT
	    datasizeb = totpix * (sizeof(pixtype) * SZB_CHAR)
	} else {
	    FIT_IOSTAT(fit) = ERR
	    return
	}

	# We need to map the virtual image of type im_pixtype to the actual
	# file of type fit_pixtype. 'nbytes' is the number of bytes to read
	# from the virtual image. To find out how many fit_pixtype bytes 
	# we need to read from disk we need to subtract the FITS
	# header size (if boffset is 1) from nbytes and then divide
	# the resultant value by the convertion factor.
	# We then add the size of the header if necessary.

	# Determine the offset into the pixel area.
	nboff = boffset - pixoffb
	if (nboff > 0) {
	    nelem = nboff / pfactor
	    offset = nelem + pixoffb
	} else  {
	    # Keep the 1st boffset.
	    bzoff = boffset
	    offset = boffset
	}

	# Calculates the number of elements to convert. We keep the offset from
	# the beginning of the unit (bzoff) and not from file's 1st byte.

	nelem = nbytes - (pixoffb - bzoff + 1)
	nelem = nelem / pfactor
	buf_size = nelem + (pixoffb - bzoff + 1)
	if (buf_size*pfactor > nbytes && ip == 1)
	    buf_size = (nbytes - 1) / pfactor + 1 

	# Allocate space for TY_SHORT
	call smark(sp)
	call salloc (buf, buf_size/SZB_CHAR, TY_SHORT)
	    
	call zardbf (FIT_IO(fit), Mems[buf], buf_size, offset)
	call zawtbf (FIT_IO(fit), status)
	if (status == ERR) {
	    FIT_IOSTAT(fit) = ERR
	    call sfree (sp)
	    return
	}
	    
	# Map the number of bytes of datatype FIT_PIXTYPE to 
	# IM_PIXTYPE for use in zfxfwt().
	     
	if (status*pfactor >= nbytes)
	    FIT_ZBYTES(fit) = nbytes
	else
	    FIT_ZBYTES(fit) = status * pfactor

	nb_skipped = offset - pixoffb
	if (nb_skipped <= 0)
	    nb = min (status + nb_skipped, datasizeb)
	else
	    nb = min (status, datasizeb - nb_skipped)

	switch (FIT_PIXTYPE(fit)) {
	case TY_UBYTE:
	    npix = max (0, nb)
	    if (IM_PIXTYPE(im) == TY_SHORT) {
		# arg1: incompatible pointer
		call achtbs (Mems[buf+ip-1], obuf[ip], npix)
	    } else {
		# Scaled from byte to REAL.
		# arg1,2: incompatible pointer
		call achtbi (Mems[buf+ip-1], obuf[ip], npix)
		# arg1,2: incompatible pointer
		call fxf_altmr (obuf[ip], obuf[ip], npix, bscale, bzero)
	    }
	case TY_SHORT:
	    op = buf + ip - 1
	    npix = max (0, nb / (sizeof(pixtype) * SZB_CHAR))
	    if (BYTE_SWAP2 == YES) {
		call bswap2 (Mems[op], c_1, Mems[op], c_1, npix*SZB_CHAR)
	    }
	    # arg2: incompatible pointer
	    call fxf_astmr (Mems[op], obuf[ip], npix, bscale, bzero)
	}

	call sfree (sp)
end
