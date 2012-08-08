include <error.h>

# MAKEWCS -- Task to write the WCS information into a header of an image
# subset from the scanned plate catalog.

procedure t_makewcs()

int	imtopenp(), imtgetim()
int	list
char	image[SZ_FNAME]
bool	verbose, clgetb()

begin
	list = imtopenp ("infiles")
	verbose = clgetb("verbose")
	while (imtgetim(list, image, SZ_FNAME) != EOF) {
	    call makecd(image)
	    if (verbose) {
	       call printf (" WCS for %s\n")
		   call pargstr(image)
	       call flush(STDOUT)
	    }
	}

end

include <imhdr.h>
include <math.h>

define  SZ_KEYWORD  8

# MAKECD -- Procedure to calculate the WCS values for a subset of a 
# scanned plate that contains a general plate solution.
#
# Calculates CRPIX, CRVAL and CD matrix from the information in the GSSS
# header wich consist on the positions of the lower left corner of the
# subimage (CNPIX1, CNPIX2) w/r to the lower left corner of the scanned plate.
# From here create a grid of 100x100 points and do a local solution to 
# the CD values and translate the reference position to the center of the
# subimage. 
#

procedure makecd(image)

char	image[SZ_FNAME]

double	amdx[20], amdy[20]		# Plate Soln.coeff.
double  plate_cen_x, plate_cen_y	# center position (microns)
double  plate_cen_ra, plate_cen_dec	# in radians
double	x_pixel_size, y_pixel_size	# step size (microns)
double	plate_scale			# arcsec/mm
int	ra_h, ra_m			# RA hours, minutes
double	ra_s				# RA seconds
char	dec_sign[1]			# 
int	dec_d, dec_m			# DEC degrees, minutes
double	dec_s				# DEC seconds
double  object_mag, object_col		# Magnitud, color for object
double  im_x_center_pix, im_y_center_pix # Pixel position of image center
				        # w/r to the original scanned plate
int	xcorner, ycorner		# Lower left position of image w/r to
					# original scanned plate
int	xsize, ysize			# Naxis1, naxis2
real    crpix1, crpix2
double	crval1, crval2
double	cdmatx[4]
double	imgetd()
real	imgetr()
int	imgeti()
char 	parname[8]
int	i
pointer im
int	immap(), imaccf()
errchk  imgetd, imgeti

begin
	im = immap (image, READ_WRITE, 0)

	# See if image header has the General Plate solution.
	if (imaccf(im,"PPO3    ") == NO)
	   call error(0,"No Plate solution available in header")

	# If we have an old DSS image (i.e. the one with the CRPIX), rename
	# this keyword to CNPIX and proceed.

	if (imaccf(im,"CRPIX1") == YES ) {
	   if (imaccf(im,"CRVAL1") == YES) {
	      if (imaccf(im,"CD1_1") == NO ){
		 # This is the case when we have CRPIX1, CRVAL1 and no CD1_1
		 # so, proceed to calculate the WCS again.
	         crpix1 = imgetr (im, "CRPIX1")
	         call imdelf (im, "CRPIX1")
	         call imaddr (im, "CNPIX1", crpix1)
	         crpix2 = imgetr (im, "CRPIX2")
	         call imdelf (im, "CRPIX2")
	         call imaddr (im, "CNPIX2", crpix2)
	      }
	   } else {
	      crpix1 = imgetr (im, "CRPIX1")
	      call imdelf (im, "CRPIX1")
	      call imaddr (im, "CNPIX1", crpix1)
	      crpix2 = imgetr (im, "CRPIX2")
	      call imdelf (im, "CRPIX2")
	      call imaddr (im, "CNPIX2", crpix2)
	   }
	}
	if (imaccf(im,"CNPIX1") == NO )
	   call error(0,"Lower left position CNPIX(1,2) not found")
	plate_cen_x = imgetd(im, "PPO3    ")
	plate_cen_y = imgetd(im, "PPO6    ")
	x_pixel_size = imgetd(im, "XPIXELSZ")
	y_pixel_size = imgetd(im, "YPIXELSZ")
	plate_scale = imgetd(im, "PLTSCALE")
	ra_h = imgeti (im, "PLTRAH  ")
	ra_m = imgeti (im, "PLTRAM  ")
	ra_s = imgetd (im, "PLTRAS  ")
	call imgstr(im, "PLTDECSN", dec_sign, 1)
	dec_d = imgeti (im, "PLTDECD ")
	dec_m = imgeti (im, "PLTDECM ")
	dec_s = imgetd (im, "PLTDECS ")

	plate_cen_ra = DEGTORAD ((ra_h + ra_m/60.0d0 + ra_s/3600.0)*15.0d0)
	plate_cen_dec = DEGTORAD (dec_d + dec_m/60.0d0 + dec_s/3600.0d0)
	if (dec_sign[1] == '-')
	   plate_cen_dec = -plate_cen_dec
 
	# Get general plate solution coefficients
	do i = 1, 20 {
	  call sprintf(parname, SZ_LINE, "AMDX%d")
	       call pargi(i)
	  amdx[i] = imgetd (im, parname)
	}
	do i = 1, 20 {
	  call sprintf(parname, SZ_LINE, "AMDY%d")
	       call pargi(i)
	  amdy[i] = imgetd (im, parname)
	}

	if (imaccf(im, "CNPIX1") == YES) {
	   xcorner = imgetr (im, "CNPIX1")
	   ycorner = imgetr (im, "CNPIX2")
	} else
	   call error(0,"CNPIXs keyword found")

	xsize   = IM_LEN(im,1)
	ysize   = IM_LEN(im,2) 
	crpix1  = xsize/2.
	crpix2  = ysize/2.

	# Center of image w/r to original lower left corner of
	# scanned plate
	im_x_center_pix = xcorner + (xsize/2.) - 0.5
	im_y_center_pix = ycorner + (ysize/2.) - 0.5

	# Calculate equatorial coordinates for the center of subset giving
	# the complete plate solution w/r to the original lower left corner
	call ccgseq (plate_cen_ra,
     		     plate_cen_dec,
		     plate_cen_x,
		     plate_cen_y,
		     x_pixel_size,
		     y_pixel_size,
		     plate_scale,
		     amdx,
		     amdy,
		     im_x_center_pix,
		     im_y_center_pix,
		     object_mag,
		     object_col,
		     crval1,
		     crval2)

	
	# calculate CD matrix values for the input subset from the original
	# plate solution.
	call calcds (plate_cen_ra,
		  plate_cen_dec,
		  plate_cen_x,
		  plate_cen_y,
		  xcorner,
		  ycorner,
		  x_pixel_size,
		  y_pixel_size,
		  plate_scale,
		  xsize,
		  ysize,	
		  crval1,
		  crval2,
		  amdx,
		  amdy,
		  cdmatx)

	# Now update the image header

	iferr (call addkf (im, "CTYPE1", 8, TY_CHAR,
			"r.a. in tangent plane proyection"))
			;
	call impstr (im, "CTYPE1", "RA---TAN")

	iferr (call addkf (im, "CTYPE2", 8, TY_CHAR,
			"dec. in tangent plane proyection"))
			;
	call impstr (im, "CTYPE2", "DEC--TAN")

	iferr (call addkf (im, "CRPIX1", 1, TY_REAL,
			"reference pixel of first axis"))
			;
	call imputr (im, "CRPIX1", crpix1)

	iferr (call addkf (im, "CRPIX2", 1, TY_REAL,
			"reference pixel of second axis"))
			;
	call imputr (im, "CRPIX2", crpix2)

	iferr (call addkf (im, "CRVAL1", 1, TY_REAL,
			"r.a. at pixel crpix1 in decimal degrees"))
			;
	call imputd (im, "CRVAL1", RADTODEG(crval1))

	iferr (call addkf (im, "CRVAL2", 1, TY_REAL,
			"dec. at pixel crpix2 in decimal degrees"))
			;
	call imputd (im, "CRVAL2", RADTODEG(crval2))

	# now add the new CD values
	iferr (call addkf (im, "CD1_1", 1, TY_REAL,
			"Change in r.a. per pixel along the first axis"))
			;
	else 
	   call addkf (im, "", 1, TY_REAL,
			"evaluated at the reference pixel")
	call imputd (im, "CD1_1", cdmatx[1])

	iferr (call addkf (im, "CD1_2", 1, TY_REAL,
			"ditto along the second axis"))
			;
	call imputd (im, "CD1_2", cdmatx[2])

	iferr (call addkf (im, "CD2_1", 1, TY_REAL,
			"Change in dec. per pixel along the first axis"))
			;
	else 
	   call addkf (im, "", 1, TY_REAL,
			"evaluated at the reference pixel")
	call imputd (im, "CD2_1", cdmatx[3])

	iferr (call addkf (im, "CD2_2", 1, TY_REAL,
			"ditto along the second axis"))
			;
	call imputd (im, "CD2_2", cdmatx[4])

	call imunmap (im)
end

include	<syserr.h>
include	<imhdr.h>
include <imio.h>
include <mach.h>

define  SZ_VALSTR   80
# ADDKF -- Add a user field to the image header.  It is an error if the named
# field already exists. 

procedure addkf (im, key, lenv, datatype, comment)

pointer	im			# image descriptor
char	key[SZ_KEYWORD]		# name of the new parameter
int	lenv			# keyword value length
int	datatype		# datatype of parameter
char	comment[SZ_VALSTR]		# comment describing new parameter

int	max_lenuserarea, diff, slen
pointer	sp, keyname, rp, ua, op
int	idb_kwlookup(), idb_findrecord(), strlen()
errchk	syserrs

begin
	call smark (sp)
	call salloc (keyname, SZ_FNAME, TY_CHAR)
	call strcpy (key, Memc[keyname], SZ_FNAME)

	# Check for a redefinition.
	if ((idb_kwlookup (key) > 0) || (idb_findrecord (im, key, rp) > 0))
	    call syserrs (SYS_IDBREDEF, key)
	
	# Open the user area string for appending.  If the user area is not
	# empty the last character must be the newline record delimiter,
	# else the new record we add will be invalid.

	max_lenuserarea = (LEN_IMDES + IM_LENHDRMEM(im) - IMU + 1) * SZ_STRUCT
	ua = IM_USERAREA(im)

	for (rp=ua;  Memc[rp] != EOS;  rp=rp+1)
	    ;
	if (rp - ua + SZ_VALSTR + 1 >= max_lenuserarea)
	    call syserrs (SYS_IDBOVFL, key)

	if (rp > ua && Memc[rp-1] != '\n') {
	    Memc[rp] = '\n'
	    rp = rp + 1
	}

	# Append the new record with an uninitialized value field.  Keyword
	# value pairs are encoded in FITS format.

	do op = rp, rp + SZ_VALSTR		# blank fill card
	    Memc[op] = ' '

	# Add the "= 'value' / comment".
	slen = strlen(Memc[keyname])
	call amovc (Memc[keyname], Memc[rp], slen)
	if (slen != 0)
	   Memc[rp+9-1] = '='
	if (datatype == TY_CHAR) {
	    Memc[rp+11-1] = '\''
	    Memc[rp+20-1] = '\''
	}

	# Add the comment field.
	diff = lenv - 20
	if (diff > 0) {
	   Memc[rp+34-1+diff] = '/'
	   call amovc (comment, Memc[rp+36-1+diff],
	       min (SZ_VALSTR-36+1-diff, strlen(comment)))
	} else {
	   Memc[rp+32-1] = '/'
	   call amovc (comment, Memc[rp+34-1],
	       min (SZ_VALSTR-34+1, strlen(comment)))
	}
	# Terminate the card.
	Memc[rp+SZ_VALSTR] = '\n'
	Memc[rp+SZ_VALSTR+1] = EOS

	call sfree (sp)
end
