include <imhdr.h>
include <tbset.h>

#  TM_COPY  --  Copy image into designated row/column.
#
#
#
#
#  Revision history:
#  ----------------
#  30-Jan-97  -  Task created (I.Busko)
#  21-May-97  -  Changes from code review (IB)


procedure tm_copy (tp, cp, row, rflag, im)

pointer	tp		# table pointer
pointer	cp		# column pointer
int	row		# row where to begin insertion
bool	rflag		# use row number in image header ?
pointer	im		# imio pointer
#--
pointer	sp, duma
int	i, lena, leni, dumi

int	tbcigi(), imgeti(), imaccf()

begin
	# See if table and image pixel types match.
	if (tbcigi (tp, TBL_COL_DATATYPE) == IM_PIXTYPE(im))
	    call error (1, "Pixel type mismatch.")

	# Look for row information in image header.
	if (imaccf (im, "ORIG_ROW") == YES) {
	    if (rflag)
	         row = imgeti (im, "ORIG_ROW")   
	}

	# Get column array size and image size. 
	call smark (sp)
	call salloc (duma, max(max(SZ_COLNAME,SZ_COLUNITS),SZ_COLFMT),TY_CHAR)
	call tbcinf (cp, dumi, Memc[duma], Memc[duma], Memc[duma], dumi, 
                     lena, dumi)
	call sfree (sp)
	leni = 1
	do i = 1, IM_NDIM(im)
	    leni = leni * IM_LEN(im,i)

	# Copy.
	switch (IM_PIXTYPE(im)) {
	case TY_SHORT:
	    call tm_cp1s (im, tp, cp, row, lena, leni)
	case TY_INT:
	    call tm_cp1i (im, tp, cp, row, lena, leni)
	case TY_REAL:
	    call tm_cp1r (im, tp, cp, row, lena, leni)
	case TY_DOUBLE:
	    call tm_cp1d (im, tp, cp, row, lena, leni)
	default:
	    call error (1, "Non-supported data type.")
	}

end




