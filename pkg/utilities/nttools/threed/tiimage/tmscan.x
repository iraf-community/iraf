include <error.h>
include <imhdr.h>
include <tbset.h>

#  TM_SCAN  --  Scan input image list and create column pointer array 
#               and table from information stored in image headers.
#
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)
#  21-May-97  -  Changes from code review (IB)


procedure tm_scan (otp, ocp, ocpsize, nocp, imlist)

pointer	otp		# i:  output table pointer
pointer	ocp		# io: output table column pointer array
int	ocpsize		# i:  size of above array
int	nocp		# o:  actual number of columns in array
char	imlist[ARB]	# i:  input image list
#--
pointer	sp, im, list
pointer	imname, cn, cn1, cu, cf, duma
int	image, column, lendata, dumi, i
bool	match

errchk	tm_header

pointer	imtopen(), immap()
int	imtlen(), imtgetim()
bool	streq()

begin
	call smark (sp)
	call salloc (imname, SZ_PATHNAME, TY_CHAR)
	call salloc (cn, SZ_COLNAME, TY_CHAR)
	call salloc (cn1, SZ_COLNAME, TY_CHAR)
	call salloc (cu, SZ_COLUNITS, TY_CHAR)
	call salloc (cf, SZ_COLFMT, TY_CHAR)
	call salloc (duma, max(SZ_COLUNITS,SZ_COLFMT),TY_CHAR)

	# Open input list and initialize number of columns.
	list = imtopen (imlist)
	nocp = 0

	# Scan input list.
	do image = 1, imtlen(list) {

	    # Open image.
	    i = imtgetim (list, Memc[imname], SZ_PATHNAME)
	    iferr (im = immap (Memc[imname], READ_ONLY, 0)) {
	        call erract (EA_WARN)
	        next
	    }

	    # Get column data from image header.
	    iferr (call tm_header (im, Memc[cn], Memc[cu], Memc[cf])) {
	        call erract (EA_WARN)
	        next
	    }

	    # Array size is full image size.
	    lendata = 1
	    do i = 1, IM_NDIM(im)
	        lendata = lendata * IM_LEN(im,i)


	    # See if column name from header matches any name
	    # already stored in column pointer array.
	    match = false
	    do column = 1, nocp {
	        call tbcinf (Memi[ocp+column-1], dumi, Memc[cn1], 
                             Memc[duma], Memc[duma], dumi, dumi, dumi)
	        if (streq (Memc[cn1], Memc[cn])) {
	            match = true
	            break
	        }
	    }
	    if (!match) {

	    # No names matched, define new column.
	        call tbcdef (otp, Memi[ocp+nocp], Memc[cn], Memc[cu], 
                             Memc[cf], IM_PIXTYPE(im), lendata, 1)
	        nocp = nocp + 1
	    }
	}

	call imtclose (list)
	call sfree (sp)
	if (nocp == 0)
	    call error (1, "No images with column data in header.")
	call tbtcre (otp)
end
