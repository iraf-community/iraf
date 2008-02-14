# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 
include	<syserr.h>
include	<imhdr.h>
include	<imio.h>
include	<mach.h>
include	"fxf.h"


# FXF_RHEADER -- Read a FITS header into the image descriptor and the 
# internal FITS descriptor.

procedure fxf_rheader (im, group, acmode)

pointer	im			#I image descriptor
int	group			#I group number to read
int	acmode			#I access mode

long	pixoff,	mtime
pointer	sp, fit, lbuf, poff
int	compress, devblksz, i, impixtype
bool    bfloat, lscale, lzero
bool    fxf_fpl_equald()
int	strncmp()

errchk	fxf_rfitshdr, realloc, syserr, syserrs

begin
	call smark (sp)
	call salloc (lbuf, SZ_LINE, TY_CHAR)

	fit = IM_KDES(im)

	FIT_MAX(fit) = 0.0
	FIT_MIN(fit) = 0.0
	FIT_MTIME(fit) = 0.0
	FIT_IM(fit) = im
	FIT_OBJECT(fit) = EOS
	IM_CLSIZE(im) = 0

	# Read the header unit number 'group', setting the values of the 
	# reserved fields in the FIT descriptor saving it in the FITS cache.

	call fxf_rfitshdr (im, group, poff)

	IM_MIN(im) = FIT_MIN(fit)
	IM_MAX(im) = FIT_MAX(fit)
	IM_MTIME(im) = FIT_MTIME(fit)
	call strcpy (FIT_OBJECT(fit), IM_TITLE(im), LEN_CARD)

	# If there is no group specification in the filename, group is -1;
	# new group number is in FIT_GROUP.

	group = FIT_GROUP(fit)
	IM_CLINDEX(im) = group

	# Process the reserved keywords (set in the FIT descriptor) into the
	# corresponding fields of the IMIO descriptor.

	if (acmode != NEW_COPY) {
	    IM_NDIM(im) = FIT_NAXIS(fit)		# IM_NDIM
	    do i = 1, IM_NDIM(im) {			# IM_LEN
		IM_LEN(im,i) = FIT_LENAXIS(fit,i)
		if (IM_LEN(im,i) == 0) {
		    IM_NDIM(im) = 0
		    break
	        }
	    }
	}

	lscale = fxf_fpl_equald (1.0d0, FIT_BSCALE(fit), 1)
	lzero =  fxf_fpl_equald (0.0d0, FIT_BZERO(fit), 1)

	# Determine if scaling is necessary.
	bfloat = (!lscale || !lzero)

	FIT_PIXTYPE(fit) = NULL
	FIT_ZCNV(fit) = NO

	switch (FIT_BITPIX(fit)) {
	case  8:
	    FIT_PIXTYPE(fit) = TY_UBYTE
	    if (bfloat)
		impixtype = TY_REAL
	    else
		impixtype = TY_SHORT              # convert from byte to short
	    FIT_ZCNV(fit) = YES
	case 16:
	    FIT_PIXTYPE(fit) = TY_SHORT
	    if (bfloat) {
		impixtype = TY_REAL
	        FIT_ZCNV(fit) = YES
	    } else
		impixtype = TY_SHORT

	    if ((strncmp ("USHORT", FIT_DATATYPE(fit), 6) == 0) ||
		    (lscale && fxf_fpl_equald (32768.0d0, FIT_BZERO(fit),4))) {
		impixtype = TY_USHORT
	        FIT_ZCNV(fit) = NO
	    }
	case 32:
	    FIT_PIXTYPE(fit) = TY_INT
	    if (bfloat)
		impixtype = TY_REAL
	    else
		impixtype = TY_INT
	case -32:
	    FIT_PIXTYPE(fit) = TY_REAL
	    impixtype = TY_REAL
	case -64:
	    FIT_PIXTYPE(fit) = TY_DOUBLE
	    impixtype = TY_DOUBLE
	default:
	    impixtype = ERR
	}

	IM_PIXTYPE(im) = impixtype

	IM_NBPIX(im)   = 0			# no. bad pixels
	mtime = IM_MTIME(im)
		
	if (IM_MAX(im) > IM_MIN(im))
	   IM_LIMTIME(im) = mtime + 1		# time max/min last updated
	else
	   IM_LIMTIME(im) = mtime - 1		# Invalidate DATA(MIN,MAX)
	IM_HISTORY(im) = EOS

	# Call up IMIO to set up the remaining image header fields used to
	# define the physical offsets of the pixels in the pixfile.

	compress = YES		# do not align image lines on blocks
	devblksz = 1		# disable all alignment

	pixoff = Memi[poff+group]
	FIT_PIXOFF(fit) = pixoff
	call imioff (im, pixoff, compress, devblksz)
	
	call sfree (sp)
end


# FXF_FPL_EQUALD -- Compare 2 double precision quantities up to a precision
# given by a tolerance.

bool procedure fxf_fpl_equald (x, y, it)

double	x, y		#I Input quantities to be compare for equality
int	it		#I Tolerance factor of 10 to compare the values

int	ex, ey
double	x1, x2, normx, normy, tol

begin
	# Check for the obvious first.
	if (x == y)
	    return (true)

	# We can't normalize zero, so handle the zero operand cases first.
	# Note that the case 0 equals 0 is handled above.

	if (x == 0.0D0 || y == 0.0D0)
	    return (false)

	# Normalize operands and do an epsilon compare.
	call fp_normd (x, normx, ex)
	call fp_normd (y, normy, ey)

	if (ex != ey)
	    return (false)
	else {
	    tol = EPSILOND * 10.0D0 * it
	    x1 = 1.0D0 + abs (normx - normy)
	    x2 = 1.0D0 + tol
	    return (x1 <= x2)
	}
end
