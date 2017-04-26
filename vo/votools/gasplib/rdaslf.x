# code:	 Nelson Zarate		January 89

define	SZ_KEYWORD	8
define	NTERMS_MODEL	20

# RD_ASLF -- Read astrometry solution parameters obtained with program
#	     'pltsol'

procedure rd_aslf (im, fd, namd, icd, crpix1, crpix2, crval1, 
		   crval2, x_pixel_size, y_pixel_size, plate_scale, 
		   namdx, namdy)

pointer	im			# image file descriptor
pointer	fd			# Ascii file descriptor
bool    namd			# Are new coefficient from pltsol in header?
bool	icd			# Use CD values?
double	crpix1, crpix2		# Reference pixel
double	crval1, crval2		# Reference equatorial position (degrees)
double  x_pixel_size		# Y pixel size (microns)
double  y_pixel_size		# X pixel size (microns)
double	plate_scale		# [arc_sec/mm]
double	namdx[NTERMS_MODEL]		# New coeff in x direction
double	namdy[NTERMS_MODEL]		# New coeff in y direction

char	buf[SZ_KEYWORD]
char	outstr[SZ_LINE]
int	nch, fscan(), getline(), i, j, k, ic, ctoi(), ip, ii
int	strncmp(), ctowrd(), ctod(), idb_find()
double	imgetd()
pointer	pp, rp

begin

    # Clear the coefficient arrays.
    call aclrd (namdx, NTERMS_MODEL) 
    call aclrd (namdy, NTERMS_MODEL) 
   
    if (im == -1) {
	nch = getline (fd, outstr)  # Title line
	nch = getline (fd, outstr)  # Blank line
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (crpix1)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (crpix2)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (crval1)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (crval2)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (x_pixel_size)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (y_pixel_size)
	      call gargstr (outstr, SZ_LINE)	
	nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      call gargd (plate_scale)
	      call gargstr (outstr, SZ_LINE)	

	do i = 1, NTERMS_MODEL { # read XCOEFF
	   k = 7	# starting character number for digits
	   nch = fscan (fd)	
	      call gargwrd (buf, SZ_KEYWORD)
	      nch = ctoi (buf, k, ic)
	      if (strncmp (buf, "YCOEFF", 6) == 0) {
	         call gargd (namdy[ic])
	         break
	      }
	      call gargd (namdx[ic])
	      call gargstr (outstr, SZ_LINE)	
	}
	do j = 2, NTERMS_MODEL {
	   k = 7	# starting character number for digits
	   nch = fscan (fd)	
	   if (nch == EOF)
	      break
	   call gargwrd (buf, SZ_KEYWORD)
	   nch = ctoi (buf, k, ic)
	   # in case there is more than one set of coeeficients, take only
	   # the first one.
	   if (strncmp (buf, "XCOEFF", 6) == 0)  
	      break
	   call gargd (namdy[ic])
	   call gargstr (outstr, SZ_LINE)	
	}
    } else {
	   crpix1 = imgetd (im, "CRPIX1")
	   crpix2 = imgetd (im, "CRPIX2")
	   crval1 = imgetd (im, "CRVAL1")
	   crval2 = imgetd (im, "CRVAL2")
	if (namd) {
	   x_pixel_size = imgetd (im, "XPIXELSZ")
	   y_pixel_size = imgetd (im, "YPIXELSZ")
	   plate_scale = imgetd (im, "PLTSCALE")
	}

	# Are new coefficients from 'pltsol' task in header
	if (namd) {
	   do k = 1, NTERMS_MODEL {
	      namdx[k] = 0.0
	      namdy[k] = 0.0
	   }
	   call strcpy ("NAMDX?", buf, SZ_KEYWORD)
	   if (idb_find (im, buf, rp) == 0)
	      call error (13,"New solution coefficients (NAMDX*) not found")
	   for (pp=rp;  Memc[pp] != EOS;  pp=pp+81) {
	      k = 6
	      ii = 11	# starting char number of keyword value
	      ip = 1
       	      # get keyword
	      nch = ctowrd (Memc[pp], ip, buf, SZ_KEYWORD)
	      # get keyword sequence 
	      nch = ctoi (buf, k, ic)		     
	      if (strncmp (buf, "NAMDX", 5) == 0)
	         nch = ctod (Memc[pp], ii, namdx[ic])
	      else if (strncmp (buf, "NAMDY", 5) == 0)
	         nch = ctod (Memc[pp], ii, namdy[ic])
	   }
	}
    }
end
