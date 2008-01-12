include <mach.h>
include <imio.h>
include	<imhdr.h>
include <error.h>
include "gi.h"

# GI_XDIMTOMG -- Convert an image with an extra dimension for the groups and 
# and attach table with gpb values, into a true sdas/geis multigroup file.

procedure gi_xdimtomg (image1, image2, verbose)

char	image1[SZ_FNAME]		# Input image
char	image2[SZ_FNAME]		# Output image
bool	verbose

int	npix, junk,i, k
pointer	buf1, buf2, im1, im2
pointer	sp
long	v1[IM_MAXDIM], v2[IM_MAXDIM]
int	imgnls(), imgnll(), imgnlr(), imgnld()
int	impnls(), impnll(), impnlr(), impnld()
pointer	immap()

pointer	tp
char	tname[SZ_FNAME]
char	temp[SZ_FNAME]
int	dim1, dim2, nlines, lenu, strlen()
int	stf, gn, tbtopn(), tbtacc(), pp
int	ngroups, blklen, compress, pixoff
real	datamin, datamax, imgetr()
int	nch, sizeof(), imgeti(), strcmp(), pcount, strldx()
int     nlen

begin
	call smark (sp)

	compress = YES
	blklen = 1
	datamin = 1.0
	datamax = -datamin

	# Map the input image.
	iferr (im1 = immap (image1, READ_ONLY, 0))
	    call error(1,"error opening input image")
	dim1 = IM_NDIM(im1)
	dim2 = dim1

	# make sure is the correct kind of file
	stf = IM_KDES(im1)
	if (STF_GCOUNT(stf) != 1 || STF_PCOUNT(stf) != 0 ||
	    STF_PSIZE(stf) != 0 ) {
	    call imunmap(im1)
	    call error (1, "Input file is not valid for convertion")
	}

	# See if table with gpb values exists.
#	nch = fnextn (image1, extn, MAX_LENEXTN)
#	nch = fnroot (image1, tname, SZ_FNAME)

	# Look for special string that 'nwfits' puts into the filename
	# when the 'nwfits.sdasmgcv=yes'.
#	if (strmatch (image1, "_cvt") != 0 && strmatch (extn ,"hhh") == 0)
#	   call strcpy (image1, tname, SZ_FNAME)
#	call strcat (".tab", tname, SZ_FNAME)

	call strcpy(image1,tname,SZ_FNAME)
	nch = strldx(".",image1)
	call strcpy ("tab",tname[nch+1], SZ_FNAME)
	if (tbtacc (tname) == NO) {
	   call imunmap(im1)
	   call error(1,"Associated table does not exists")
	}

	# See if the last dimension correspond to the "group" dimension.
	# The special keyword "SDASMGNU=   <number_of groups>" would tell this.
	
	iferr (ngroups = imgeti (im1, "SDASMGNU") ) {
	      call imunmap(im1)
	      call error(13,"Keyword SDASMGNU not in header")
	}

	# Open output file with inherited information from input file.
	call sprintf (temp, SZ_FNAME, "[1/%d]")	
	     call pargi (ngroups)
	call strcat (temp, image2, SZ_FNAME)
	iferr (im2 = immap (image2, NEW_COPY, im1)) {
	     call imunmap(im1)
	     call error (1, "error opening output image")
	}
	
	# Now delete the keyword from the output IM_USERAREA
	call imdelf (im2, "SDASMGNU")

	# Setup output stf parameters
	stf = IM_KDES(im2)
	STF_GROUPS(stf) = YES

	# Setup start vector for sequential reads and writes.
	call amovkl (long(1), v1, IM_MAXDIM)

	if (ngroups > 1) {
	    # The groups are stack in the last dimension.
	    dim2 = dim1-1
	    ngroups =  IM_LEN(im1, dim1)

	    # Diminish output dimensionality by one.
	    IM_NDIM(im2) = dim2
	    IM_LEN(im2,dim1) = 1
	    STF_GCOUNT(stf) = ngroups 
	    STF_NAXIS(stf) = dim2
	    do i = 1, dim2
	       STF_LENAXIS(stf,i) = IM_LEN(im2,i)
	    # Delete the information about the extra dimension in the
	    # userarea. This lines are at the end of the user area
	    # Only: CTYPEn, CDn_n and CDn_m
	    nlines = (dim1*2)*81
	} else {
	    nlines = 0
	    ngroups = 1
	}
	npix = IM_LEN(im1, 1)

	if (verbose) {
	   call printf ("converting %s and %s to %s with %d groups\n") 
		call pargstr (image1)
		call pargstr (tname)
		call pargstr (image2)
		call pargi (ngroups)
	   call flush(STDOUT)
	}
	# Set up STFDES for the gpb values.
	tp = tbtopn (tname, READ_ONLY, 0)
	call gistfdes_setup (tp, stf, im2)
	pcount = STF_PCOUNT (stf)

	STF_NEWIMAGE(stf) = NO
	# Loop through the groups
	do gn = 1, ngroups {

	   STF_GROUP(stf) = gn
	   # Read gpb from the table.
	   call gird_gpb (tp, gn, im2)

	   #See if there is a DATAMIN or DATAMAX keyword in the table
	   do k = 1, pcount {
	      pp = STF_PDES(stf,k)
	      if (strcmp (P_PTYPE(pp), "DATAMAX") == 0)
	         datamax = imgetr (im2, "DATAMAX")
	      else if (strcmp (P_PTYPE(pp), "DATAMIN") == 0)
	         datamin = imgetr (im2, "DATAMIN")
	   }
	     
	   nlen = IM_LEN(im2,dim2)
	   call amovkl (long(1), v2, IM_MAXDIM)
	   switch (IM_PIXTYPE(im1)) {

	   case TY_SHORT:
	       repeat {
   	           junk = imgnls (im1, buf1, v1) 
		   junk = impnls (im2, buf2, v2)
		   call amovs (Mems[buf1], Mems[buf2], npix)
	       } until (v2[dim2] == nlen+1 || dim1 == 1)
	   case TY_USHORT,TY_INT,TY_LONG:
	       repeat {
	           junk = imgnll (im1, buf1, v1) 
		   junk = impnll (im2, buf2, v2)
		   call amovl (Meml[buf1], Meml[buf2], npix)
	       } until (v2[dim2] == nlen+1 || dim1 == 1)
	   case TY_REAL:
	       repeat {
	           junk = imgnlr (im1, buf1, v1) 
		   junk = impnlr (im2, buf2, v2)
		   call amovr (Memr[buf1], Memr[buf2], npix)
	       } until (v2[dim2] == nlen+1|| dim1 == 1)
	   case TY_DOUBLE:
	       repeat {
	           junk = imgnld (im1, buf1, v1) 
		   junk = impnld (im2, buf2, v2)
		   call amovd (Memd[buf1], Memd[buf2], npix)
	       } until (v2[dim2] == nlen+1 || dim1 == 1)
	   default:
	       call error (1, "unknown pixel datatype")
	   }

	   if (gn != ngroups) {
	      call imflush (im2)
	      call stf_wgpb (im2, STF_GROUP(stf), datamin, datamax)
	      pixoff = gn*STF_SZGROUP(stf) + 1
	      call imioff (im2, pixoff, compress, blklen)
	      if (mod(pixoff, sizeof(IM_PIXTYPE(im2))) != 1)
	          IM_FAST(im2) = NO
	      STF_NEWIMAGE(stf) = NO
	   }


	}
	call sfree (sp)

	lenu = strlen (Memc(IM_USERAREA(im2)))
	Memc(IM_USERAREA(im2)+lenu-nlines) = EOS
	# Unmap the images.
	
	IM_MIN(im2) = datamin
	IM_MAX(im2) = datamax

	IM_LIMTIME(im2) = IM_MTIME(im2) + 1	
	STF_NEWIMAGE(stf) = YES		# to call stf_wfitshdr
	call imunmap (im2)
	call imunmap (im1)
	call tbtclo (tp)

end
