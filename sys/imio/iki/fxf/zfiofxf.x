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
int	status, offset, totpix, npix
int	datasizeb, pixoffb, nb_skipped, i
int	sizeof()

begin
	fit = chan
	im = FIT_IM(fit)

	totpix = IM_PHYSLEN(im,1)
	do i = 2, IM_NPHYSDIM(im)
	    totpix = totpix * IM_PHYSLEN(im,i)

	pixtype = IM_PIXTYPE(im)
	datasizeb = totpix * (sizeof(pixtype) * SZB_CHAR)
	pixoffb = (FIT_PIXOFF(fit) - 1) * SZB_CHAR + 1

	### Comments set off by "###" are not code comments but meta-comments
	### about what we may want to do with the code later (once it is fixed
	### these comments should be removed).

	### I don't think the TY_UBYTE branch of this code ever gets used due
	### to the fxf_byte_short code.  If the offset conversion were being
	### used it needs to scaled by SZB_CHAR as shown since it is 1-indexed.
	### Also, use a local variable such as offset instead of modifying 
	### boffset directly, as boffset is a readonly input argument.  We
	### have no idea here what the calling code does with boffset and 
	### whether it relies on the value being unchanged after the call.
	### The kernel binary file drives spec says that it is an Input arg.
	### Also, if we need to scale the file offset by 2 we probably need to
	### scale nbytes by 2 as well.

	if (FIT_PIXTYPE(fit) == TY_UBYTE)
	    offset = (boffset - 1) / SZB_CHAR + 1
	else
	    offset = boffset

	# We can read the data directly into the caller's output buffer as 
	# any FITS kernel input conversions are guaranteed to not make the
	# data smaller.  

	call zardbf (FIT_IO(fit), obuf, nbytes, offset)
	call zawtbf (FIT_IO(fit), status)

	### status is not error checked here

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

	# Calculate the number of pixels to convert.
	if (FIT_PIXTYPE(fit) == TY_UBYTE)
	    call achtbs (obuf[ip], obuf[ip], npix)
	else {
	    call fxf_unpack_data (obuf[ip],
		npix, pixtype, FIT_BSCALE(fit), FIT_BZERO(fit)) 
	}
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
int	datasizeb, pixoffb, nb_skipped, status
int	sizeof()

begin
	fit = chan
	im = FIT_IM(fit)

	# We don't have to pack the data if it is integer and we don't need
	# to byte swap; the data buffer can be written directly out.

	pixtype = IM_PIXTYPE(im)
	noconvert = ((pixtype == TY_SHORT && BYTE_SWAP2 == NO) ||
	    ((pixtype == TY_INT || pixtype == TY_LONG) && BYTE_SWAP4 == NO))

	if (noconvert) {
	    call zawrbf (FIT_IO(fit), ibuf, nbytes, boffset)
	    call zawtbf (FIT_IO(fit), status)
	    return
	}

	# Convert any pixel data in the input buffer to the binary format
	# required for FITS and write it out.  Any non-pixel data in the
	# buffer should be left as-is.

	call smark (sp)
	call salloc (obuf, (nbytes + SZB_CHAR-1) / SZB_CHAR, TY_CHAR)

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

	# We don't do scaling (e.g. BSCALE/BZERO) when writing.  All the
	# generated FITS files in this interface are ieee fits standard.
	### I didn't look into it but I don't understand this; when accessing
	### a BSCALE image read-write, it should be necessary to scale both
	### when reading and writing if the application sees TY_REAL pixels.
	### When writing a new image I suppose the application would take
	### care of any scaling.

	# Preserve any leading non-pixel data.
	op = 1
	if (ip > 1) {
	    nchars = ip - 1
	    call amovc (ibuf[1], Memc[obuf], nchars)
	    op = op + nchars
	}

	# Convert and output the pixels.
	call fxf_pak_data (ibuf[ip], Memc[obuf+op-1], npix, pixtype)
	op = op + npix * sizeof(pixtype)

	# Preserve any remaining non-pixel data.
	nchars = (nbytes / SZB_CHAR) - op + 1
	if (nchars > 0)
	    call amovc (ibuf[op], Memc[obuf+op-1], nchars)

	# Write out the data.
	call zawrbf (FIT_IO(fit), Memc[obuf], nbytes, boffset)
	call zawtbf (FIT_IO(fit), status)

	call sfree (sp)
end


# FXFZWT -- Return the number of bytes transferred in the last i/o request.

procedure fxfzwt (chan, status)

int	chan			#I QPF i/o channel
int	status			#O i/o channel status

pointer fit

begin
	fit = chan
	call zawtbf (FIT_IO(fit), status)
end


# FXFZST -- Query device/file parameters.

procedure fxfzst (chan, param, value)

int	chan			#I FIT i/o channel
int	param			#I parameter to be returned
int	value			#O parameter value

pointer fit

begin
	fit = chan
	call zsttbf (FIT_IO(fit), param, value)
end
