include <mach.h>
include <imio.h>
include	<imhdr.h>
include "gi.h"

# GI_MGTOXDIM -- Convert a multigroup image to one group image.  Use sequential
# routines to permit copying images of any dimension.  Perform pixel i/o in 
# the datatype of the image, to avoid unnecessary type conversion.

procedure gi_mgtoxdim (image1, image2, verbose)

char	image1[SZ_FNAME]		# Input image
char	image2[SZ_FNAME]		# Output image
bool	verbose

int	npix, junk,i
pointer	buf1, buf2, im1, im2
pointer	sp
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnls(), imgnli(), imgnll(), imgnlr(), imgnld()
int	impnls(), impnli(), impnll(), impnlr(), impnld()
pointer	immap(), imaccf()

pointer	tp, cp, pp
char	line[SZ_LINE], tname[SZ_FNAME], extn[MAX_LENEXTN]
char	pname[SZ_PTYPE]
int	stf, gn
int	ngroups, pcount, blklen, ndim2, compress
real	datamin, datamax

begin
	call iki_init
	call smark (sp)

	compress = YES
	blklen = 1
	datamin = 1.0
	datamax = -datamin

	# Map the input image.
	iferr (im1 = immap (image1, READ_ONLY, 0))
	    call error (1,"error opening input image")

	stf = IM_KDES(im1)
	ngroups = STF_GCOUNT(stf)
	pcount  = STF_PCOUNT(stf)

	# Open output file with inherited information from input file.
	iferr (im2 = immap (image2, NEW_COPY, im1))
	    call error (1,"error opening input image")

	# Add special keyword name saying that this is an image plus a table
	call sprintf (pname, SZ_PTYPE, "SDASMGNU")
	call imaddi (im2, pname, ngroups)

	# Change dimensionality only of ngroups > 1
	if (ngroups > 1) {
	   ndim2 = IM_NDIM(im1) + 1

	   # Change dimensionality of output image.
	   # It will have an extra dimension to account for the groups 
	   # and one table to put the gpb values.
	   IM_NDIM(im2) = ndim2
	   IM_LEN(im2,ndim2) = ngroups

	   # Add image keyword to the new image to account for the new
	   # dimension
# (Apr 12 1990) Append the new keyword corresponding to the WCS
# for the xdim axis. The keyword name "CGTYPE" should be "CTYPE", but
# there are some headers that use this name for other purposes and
# we don't want to overwrite the value at this time. The solution is to
# force append the CTYPE(xdim) keyword regardless if exists or no, but
# it it left for future enhancements.
	   call sprintf (pname, SZ_PTYPE, "CTYPE%d"); call pargi (ndim2)
	   if (imaccf (im2, pname) == YES) {
	      call sprintf (pname, SZ_PTYPE, "CGTYPE%d")
	           call pargi (ndim2)
           }
	   call imastr (im2, pname, "GROUP_NUMBER")
	   do i = 1, ndim2-1 {
	      call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (ndim2)
		call pargi (i)
	      call imaddr (im2, pname, 0.0)
	      call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (i)
		call pargi (ndim2)
	      call imaddr (im2, pname, 0.0)
	    }
	    call sprintf (pname, SZ_PTYPE, "CD%d_%d")
		call pargi (ndim2)
		call pargi (ndim2)
	    call imaddr (im2, pname, 1.0)
	} 


	npix = IM_LEN(im1, 1)
	call amovkl (long(1), v2, IM_MAXDIM)

	# Open table to contain the gpb values
	call iki_parse (image2, tname, extn)
	call strcat (".tab", tname, SZ_FNAME)

	if (verbose) {
	   call printf ("converting %s with %d groups to %s and %s\n")
		call pargstr (image1)
		call pargi (ngroups)
		call pargstr (image2)
		call pargstr (tname)
	    call flush(STDOUT)
	}
	call salloc (cp, pcount, TY_INT)
	call giopn_table (tname, im1, tp, Memi[cp])

	# Loop through the groups
	do gn = 1, ngroups {

	   call gi_opengr (im1, gn, datamin, datamax, 0)
	   # Setup start vector for sequential reads and writes.
	   call amovkl (long(1), v1, IM_MAXDIM)
	   switch (IM_PIXTYPE(im1)) {

	   case TY_USHORT:
   	       while (imgnll (im1, buf1, v1) != EOF) {
		   junk = impnll (im2, buf2, v2)
		   call amovl (Meml[buf1], Meml[buf2], npix)
	       }
	   case TY_SHORT:
   	       while (imgnls (im1, buf1, v1) != EOF) {
		   junk = impnls (im2, buf2, v2)
		   call amovs (Mems[buf1], Mems[buf2], npix)
	       }
	   case TY_INT:
	       while (imgnli (im1, buf1, v1) != EOF) {
		   junk = impnli (im2, buf2, v2)
		   call amovi (Memi[buf1], Memi[buf2], npix)
	       }
	   case TY_LONG:
	       while (imgnll (im1, buf1, v1) != EOF) {
		   junk = impnll (im2, buf2, v2)
		   call amovl (Meml[buf1], Meml[buf2], npix)
	       }
	   case TY_REAL:
	       while (imgnlr (im1, buf1, v1) != EOF) {
		   junk = impnlr (im2, buf2, v2)
		   call amovr (Memr[buf1], Memr[buf2], npix)
	       }
	   case TY_DOUBLE:
	       while (imgnld (im1, buf1, v1) != EOF) {
		   junk = impnld (im2, buf2, v2)
		   call amovd (Memd[buf1], Memd[buf2], npix)
	       }
	   default:
	       call error (1, "unknown pixel datatype")
	   }

	   # Read gpb	
	   do i = 1, pcount {
	      pp = STF_PDES(stf,i)
	      if (P_PDTYPE(pp) == 'C') {
	         call imgstr (im1, P_PTYPE(pp), line, P_LEN(pp))
	      } else {
	         call imgstr (im1, P_PTYPE(pp), line, SZ_LINE)
	      }
	      # write value to table
	      call tbeptt (tp, Memi[cp+i-1], gn, line)
	   }
	}
	call sfree (sp)

	# copy the wcs information to the new image header, since the outpu
	# file is a non_group file.

	stf = IM_KDES(im2)
	STF_GROUPS(stf) = NO
	STF_GCOUNT(stf) = 1
	STF_PCOUNT(stf) = 0
	STF_PSIZE(stf) = 0
	
	# Redefine the im_userarea pointer to keep the gpb values
	# in the image header of the new image.
#	STF_SZGPBHDR(stf) = 4*(FITS_RECLEN + 1)
	# Unmap the images.

	call imunmap (im2)
	call imunmap (im1)
	call tbtclo (tp)

end
