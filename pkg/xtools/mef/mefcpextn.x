include <mach.h>
include <pkg/mef.h>

# MEF_COPY_EXTN -- Append a FITS unit to the output file.

procedure mef_copy_extn (mefi, mefo, gn)

pointer	mefi	#I input mef descriptor
pointer	mefo	#I output mef descriptor
int	gn      #I input group number

char    ibuf[FITS_BLKSZ_CHAR]
int	ndim, totpix, i, k, in, out, status
int	read(), mef_rdhdr_gn(), mef_totpix()
bool    iphdu

errchk  mef_rdhdr_gn

begin
	iphdu = (gn == 0)

	status = mef_rdhdr_gn (mefi, gn)
	if (status == EOF)
	   call error (13, " EOF encountered on input file")

	call mef_wrhdr (mefi, mefo, iphdu)
	MEF_ACMODE(mefo) = APPEND

	# Count the pixels and write data. 
	ndim = MEF_NDIM(mefi)
        if (ndim > 0 || MEF_PCOUNT(mefi) > 0) {
	    # Set in multiple of FITS_BLKSZ_CHAR
	    totpix = mef_totpix(mefi)
	    totpix = (totpix + 1439)/1440

	    in = MEF_FD(mefi)
	    out = MEF_FD(mefo)

	    # Position the input file to the beginning of the pixel area.
	    call seek (in, MEF_POFF(mefi))
	    do i = 1, totpix {
	        k = read (in, ibuf, 1440)
	        call write (out, ibuf, 1440)
	    }
	}
end
