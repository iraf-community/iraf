include <mach.h>
include	<imhdr.h>
include <fset.h>
include	"vt.h"

define	WDSBRSTR	50

# READSS4 -- Read data file from tape or disk and format the data into
# an IRAF image.  This is for type 4 sector scans.

procedure readss4 (inputfd, filenumber, brief, select, bright, velocity, hs)

int	inputfd			# file descriptor for input (usually tape)
int	filenumber		# file number on tape
bool	brief			# short output file names
bool	select			# flag to make select image
bool	bright			# flag to make bright image
bool	velocity		# flag to make velocity image
int	hs			# header data structure pointer

pointer	im, srp
char	imagefile[SZ_FNAME]
int	date, hour, minute, seconds, i, j, num, lrs
short	u[SWTH_HIGH]

int	read()
pointer	immap(), impl2s()
errchk	immap, impl2s

begin
	# Calculate the time.  Assemble the output image name.
	hour = int(VT_HTIME(hs)/3600)
	minute = int((VT_HTIME(hs) - hour * 3600)/60)
	seconds = int(VT_HTIME(hs) - hour * 3600 - minute * 60)
	if (brief) {
	    call sprintf (imagefile[1], SZ_FNAME, "s%03d")
	        call pargi (filenumber)
	} else {
	    call sprintf (imagefile[1], SZ_FNAME, "s%02d_%02d%02d_%03d")
	        call pargi (VT_HDAY(hs)) # day of month
	        call pargi (hour)
	        call pargi (minute)
	        call pargi (filenumber)
	}

	if (select) {
	    im = immap (imagefile, NEW_IMAGE, 0)
	    IM_NDIM(im) = 2
	    IM_LEN(im,1) = SWTH_HIGH
	    IM_LEN(im,2) = VT_HNUMCOLS(hs)
	    IM_PIXTYPE(im) = TY_SHORT
	    call imaddi (im, "obs_time", VT_HTIME(hs))
	    date = VT_HMONTH(hs) * 10000 + VT_HDAY(hs) * 100 + VT_HYEAR(hs)
	    call imaddi (im, "obs_date", date )
	    call imaddi (im, "wv_lngth", VT_HWVLNGTH(hs))
	    call imaddi (im, "obs_type", VT_HOBSTYPE(hs))
	    call imaddi (im, "av_intns", VT_HAVINTENS(hs))
	    call imaddi (im, "num_cols", VT_HNUMCOLS(hs))
	    call imaddi (im, "intg/pix", VT_HINTGPIX(hs))
	    call imaddi (im, "rep_time", VT_HREPTIME(hs))
	}

	do j = 1, VT_HNUMCOLS(hs) {
	    if (select)
	        srp = impl2s (im, j)

	    iferr (num = read (inputfd, u, SWTH_HIGH*SZB_SHORT/SZB_CHAR)) {
		call fseti (inputfd, F_VALIDATE, lrs*SZB_SHORT/SZB_CHAR)
		call eprintf ("Error on tape read.\n")
		num = read (inputfd, u, SWTH_HIGH*SZB_SHORT/SZB_CHAR)
	    }
	    lrs = num
	    if (num < SWTH_HIGH*SZB_SHORT/SZB_CHAR)
	        call error (0, "eof encountered when reading file")
	    if (BYTE_SWAP2 == YES)
	        call bswap2 (u, 1, u, 1, SWTH_HIGH * SZB_SHORT)

	    if (select)
	        do i = 1, 512
		    Mems[srp+i-1] = u[i]
	}

	if (select)
	    call imunmap (im)
end
