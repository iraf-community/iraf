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

int	ip, indx, channel, strldx(), ctoi(), fstati()
char    fname[SZ_PATHNAME]
pointer fit, fp

begin
	# Separate the FIT descriptor from the file name.
	call strupk (pkfn, fname, SZ_PATHNAME)

        ip = strldx ("_", fname)
	indx = ip + 1
	if (ctoi (fname, indx, fit) <= 0) {
	   status = ERR
	   return
	}

	fname[ip] = EOS
	call strpak (fname, fname, SZ_PATHNAME)

	# Open the file.
	call zopnbf (fname, mode, channel)
	if (channel == ERR) {
	   status = ERR
	   return
	}

	# Reset the FIO filename in case we are called again by FIO.
	fp = fstati (channel, F_FIODES)
	call strupk (pkfn, fname, SZ_PATHNAME)
	call strpak (fname, FPKOSFN(fp), SZ_PATHNAME)

	status = fit
	FIT_IO(fit) = channel
end


# FITZCL -- Close the FIT binary file driver.

procedure fxfzcl (chan, status)

int	chan			#I FIT i/o channel
int	status			#O output status

pointer fit

begin
	fit = chan
	call zclsbf (FIT_IO(fit), status)
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
int	nbytes			#I nbytes to be read
int	boffset			#I file offset at which read commences

pointer fit, im
int	ip, pixtype, nb
int	status, totpix, npix
int	datasizeb, pixoffb, nb_skipped, i
int	sizeof()

begin
	fit = chan
	im = FIT_IM(fit)
	FIT_IOSTAT(fit) = OK

	totpix = IM_PHYSLEN(im,1)
	do i = 2, IM_NPHYSDIM(im)
	    totpix = totpix * IM_PHYSLEN(im,i)

	if (FIT_ZCNV(fit) == YES) {
	    call fxf_cnvpx (im, totpix, obuf, nbytes, boffset)
	    return
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

	call fxf_unpack_data (obuf[ip],
	    npix, pixtype, FIT_BSCALE(fit), FIT_BZERO(fit)) 
end

 
# FXFZWR -- Write to the output file. 

procedure fxfzwr (chan, ibuf, nbytes, boffset)

int	chan			#I QPF i/o channel
char	ibuf[ARB]		#O data buffer
int	nbytes			#I nbytes to be written
int	boffset			#I file offset

bool	noconvert
pointer fit, im, sp, obuf
int	ip, op, pixtype, npix, totpix, nb, nchars, i
int	datasizeb, pixoffb, nb_skipped, obufsize
int	sizeof()

begin
	fit = chan
	im = FIT_IM(fit)
	FIT_IOSTAT(fit) = OK

	# We don't have to pack the data if it is integer and we don't need
	# to byte swap; the data buffer can be written directly out.

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
	    call amovc (ibuf[1], Memc[obuf], nchars)
	    op = op + nchars
	}

	# Convert and output the pixels.
	call fxf_pak_data (ibuf[ip], Memc[obuf+op-1], npix, pixtype)
	op = op + npix * sizeof(pixtype)

	# Preserve any remaining non-pixel data.
	nchars = obufsize - op + 1
	if (nchars > 0)
	    call amovc (ibuf[op], Memc[obuf+op-1], nchars)

	# Write out the data.
	call zawrbf (FIT_IO(fit), Memc[obuf], nbytes, boffset)

	call sfree (sp)
end


# FXFZWT -- Return the number of bytes transferred in the last i/o request.

procedure fxfzwt (chan, status)

int	chan			#I QPF i/o channel
int	status			#O i/o channel status

pointer fit, im

begin
	fit = chan
	im = FIT_IM(fit)

	# A file driver returns status for i/o only in the AWAIT routine;
	# hence any i/o errors occurring in the FK itself are indicated by
	# setting FIT_IOSTAT.  Otherwise the actual i/o operation must have
	# been submitted, and we call zawtbf to wait for i/o, and get status.

	if (FIT_IOSTAT(fit) != OK)
	    status = FIT_IOSTAT(fit)
	else
	    call zawtbf (FIT_IO(fit), status)

	if (status > 0) {
	    if (FIT_PIXTYPE(fit) == TY_SHORT && IM_PIXTYPE(im) == TY_REAL)
		status = FIT_ZBYTES(fit)
	    else if (FIT_PIXTYPE(fit) == TY_UBYTE && IM_PIXTYPE(im) == TY_SHORT)
		status = status * SZB_CHAR		# to short
	    else if (FIT_PIXTYPE(fit) == TY_UBYTE && IM_PIXTYPE(im) == TY_REAL)
		status = status * SZB_CHAR * SZ_REAL	# to real
	}
end


# FXFZST -- Query device/file parameters.

procedure fxfzst (chan, param, value)

int	chan			#I FIT i/o channel
int	param			#I parameter to be returned
int	value			#O parameter value

pointer fit, im
int	i, totpix, szb_pixel, szb_real
int	sizeof()

begin
	fit = chan
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
		    value = value + int ((totpix * SZ_SHORT * SZB_CHAR) /
			2880. + .5) * 2880
		}
	    case TY_UBYTE:
		if (IM_PIXTYPE(im) == TY_SHORT)
		    value = value + int (totpix/2880. + 0.5)*2880
	        else if (IM_PIXTYPE(im) == TY_REAL)
		    value = value + int(totpix*(szb_real-1)/2880. + 0.5) * 2880
            }
	}
end

# FXF_CNVPX -- Convert FITS type BITPIX = 8 to SHORT or REAL depending
# on the value of BSCALE, BZERO (1, 32768 is already iraf supported as ushort
# and is not treated in here). If BITPIX=16 and BSCALE and BZERO are 
# non-default then the pixels are converted to REAL.

procedure fxf_cnvpx (im, totpix, obuf, nbytes, boffset)

pointer im			#I Image descriptor
int	totpix			#I Total number of pixels		
char    obuf[ARB]		#O Output data buffer
int	nbytes			#I Size in bytes of the output buffer
int	boffset			#I Byte offset into the virtual image

pointer sp, buf, fit, op
double	bscale, bzero
int	ip, nelem, pfactor
int	pixtype, nb, buf_size, bzoff, nboff
int	status, offset, npix
int	datasizeb, pixoffb, nb_skipped
int	sizeof()

begin
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
	    if (IM_PIXTYPE(im) == TY_SHORT)
		call achtbs (Mems[buf+ip-1], obuf[ip], npix)
	    else {
		# Scaled from byte to REAL.
		call achtbl (Mems[buf+ip-1], obuf[ip], npix)
		call fxf_altmr (obuf[ip], obuf[ip], npix, bscale, bzero)
	    }
	case TY_SHORT:
	    op = buf + ip - 1
	    npix = max (0, nb / (sizeof(pixtype) * SZB_CHAR))
	    if (BYTE_SWAP2 == YES)
		call bswap2 (Mems[op], 1, Mems[op], 1, npix*SZB_CHAR)
	    call fxf_astmr (Mems[op], obuf[ip], npix, bscale, bzero)
	}

	call sfree (sp)
end
