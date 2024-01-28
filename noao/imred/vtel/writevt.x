include <error.h>
include <mach.h>
include <fset.h>
include "vt.h"

define	SZ_TABLE	8192		# size of lookup table (data)

# WRITEVT -- Write an IRAF image (vacuum telescope full disk image) out to
# tape in a format identical to the format produced bye the vacuum telescope.

procedure t_writevt()

char	imagefile[SZ_FNAME]			# name of image to be written
char	outputfile[SZ_FNAME]			# output file name (tape)
bool	verbose					# verbose flag

int	obsdate
int	x1, y1, subraster, outfd
int	one
pointer	table
pointer	srp, im, hs, sp

int	imgeti(), mtopen()
int	mtfile(), mtneedfileno()
bool	clgetb()
pointer	imgs2s(), immap()
errchk	immap, imgs2s, mtopen

begin
	call smark (sp)
	call salloc (table, SZ_TABLE, TY_SHORT)
	call salloc (hs, VT_LENHSTRUCT, TY_STRUCT)

	# Get the image name and the verbose flag from the cl.
	call clgstr ("imagefile", imagefile, SZ_FNAME)
	verbose = clgetb ("verbose")

	# Get the output file from the cl.
	call clgstr ("outputfile", outputfile, SZ_FNAME)

	# See if the outputfile is mag tape, if not, error.
        if (mtfile (outputfile) == NO)
	    call error (1, "Outputfile should be magnetic tape.")

	# If no tape file number is given, then ask whether the tape
	# is blank or contains data.  If blank then start at [1], else
	# start at [EOT].

	if (mtneedfileno(outputfile) == YES)
	    if (!clgetb ("new_tape"))
		call mtfname (outputfile, EOT, outputfile, SZ_FNAME)
	    else
		call mtfname (outputfile, 1, outputfile, SZ_FNAME)

	if (verbose) {
	    call printf ("outputfile name = %s\n")
		call pargstr (outputfile)
	}

	# Open the image file and the output file.
	im = immap (imagefile, READ_ONLY, 0)
	outfd = mtopen (outputfile, WRITE_ONLY, SZ_VTREC)

	# Get date and time from the header.
	obsdate = imgeti (im, "OBS_DATE")
	VT_HMONTH(hs) = obsdate/10000
	VT_HDAY(hs) = obsdate/100 - 100 * (obsdate/10000)
	VT_HYEAR(hs) = obsdate - 100 * (obsdate/100)
	VT_HTIME(hs) = imgeti (im, "OBS_TIME")
	VT_HWVLNGTH(hs) = imgeti(im, "wv_lngth")
	VT_HOBSTYPE(hs) = imgeti (im, "obs_type")
	VT_HAVINTENS(hs) = imgeti (im, "av_intns")
	VT_HNUMCOLS(hs) = imgeti (im, "num_cols")
	VT_HINTGPIX(hs) = imgeti (im, "intg/pix")
	VT_HREPTIME(hs) = imgeti (im, "rep_time")

	# Write header data to tape.
	call writeheader (outfd, hs, verbose)

	# Set up lookuptable for data subswaths.
	one = 1
	call amovks (one, Mems[table], SZ_TABLE)
	call aclrs (Mems[table], HALF_DIF)
	call aclrs (Mems[table + SWTHWID_14 + HALF_DIF], HALF_DIF)
	call aclrs (Mems[table + SWTHWID_23 * 3], HALF_DIF)
	call aclrs (Mems[table + SZ_TABLE - HALF_DIF], HALF_DIF)

	# Write the image data to tape.
	do subraster = 1, NUM_SRSTR {

	    # Calculate position of bottom left corner of this subraster.
	    x1 = ((NUM_SRSTR_X - 1) - mod((subraster - 1), NUM_SRSTR_X)) *
		SRSTR_WID + 1
	    y1 = ((NUM_SRSTR_Y - 1) - ((subraster - mod((subraster - 1),
		NUM_SRSTR_Y)) / NUM_SRSTR_Y)) * SWTH_HIGH + 1

	    # Get subraster.
	    srp = imgs2s (im, x1, x1+(SRSTR_WID - 1), y1, y1+(SWTH_HIGH - 1))
	    iferr (call putsubraster (outfd, Mems[srp], SRSTR_WID,
		SWTH_HIGH, Mems[table], subraster))
		call eprintf ("Error in putsubraster, subraster = %d\n")
		    call pargi (subraster)
	    if (verbose) {
		call printf("%d%% done\n")
		    call pargi ((subraster*100)/NUM_SRSTR)
		call flush (STDOUT)
	    }
	}

	# Close output file and unmap image.
	call close (outfd)
	call imunmap (im)
	call sfree (sp)
end


# WRITEHEADER -- Write header info to the output, pack date
# and time, and, if 'verbose' flag is set, display some information
# to the user.

procedure writeheader(outfd, hs, verbose)

int	outfd			# output file descriptor
pointer	hs			# header data structure pointer
bool	verbose			# verbose flag

int	i
short	hbuf[SZ_VTHDR]
int	fstati()
errchk	write

begin
	# Pack date, time.  The constants below are explained in the
	# description of the image header and how it is packed.  If any
	# changes are made the following code will have to be rewritten.

	call bitpak (VT_HMONTH(hs)/10, hbuf[1], 13, 4)
	call bitpak ((VT_HMONTH(hs)-(VT_HMONTH(hs)/10)*10), hbuf[1], 9, 4)
	call bitpak (VT_HDAY(hs)/10, hbuf[1], 5, 4)
	call bitpak ((VT_HDAY(hs)-(VT_HDAY(hs)/10)*10), hbuf[1], 1, 4)
	call bitpak (VT_HYEAR(hs)/10, hbuf[2], 13, 4)
	call bitpak ((VT_HYEAR(hs)-(VT_HYEAR(hs)/10)*10), hbuf[2], 9, 4)
	call bitpak (VT_HTIME(hs)/2**15, hbuf[3], 1, 2)
	call bitpak ((VT_HTIME(hs)-(VT_HTIME(hs)/2**15)*2**15), hbuf[4], 1, 15)

	# Put other parameters in appropriate places.
	hbuf[5] = VT_HWVLNGTH(hs)
	hbuf[6] = VT_HOBSTYPE(hs)
	hbuf[7] = VT_HAVINTENS(hs)
	hbuf[8] = VT_HNUMCOLS(hs)
	hbuf[9] = VT_HINTGPIX(hs)
	hbuf[10] = VT_HREPTIME(hs)

	# Store other header parameters.
	for (i = 11 ; i <= SZ_VTHDR ; i = i + 1)
	    hbuf[i] = 0

	if (verbose) {
	    call printf ("\nmonth/day/year = %d/%d/%d\n")
		call pargi (VT_HMONTH(hs))
		call pargi (VT_HDAY(hs))
		call pargi (VT_HYEAR(hs))
	    call printf ("time = %d seconds since midnight\n")
		call pargi (VT_HTIME(hs))
	    call printf ("wavelength = %d\nobservation type = %d\n")
		call pargi (VT_HWVLNGTH(hs))
		call pargi (VT_HOBSTYPE(hs))
	    call flush (STDOUT)
	}

	if (BYTE_SWAP2 == YES)
		call bswap2 (hbuf, 1, hbuf, 1, SZ_VTHDR*SZB_SHORT)
	call write (outfd, hbuf, SZ_VTHDR*SZB_SHORT/SZB_CHAR)
	if (fstati (outfd, F_NCHARS) !=  SZ_VTHDR*SZB_SHORT/SZB_CHAR)
	    call error (0, "error when writing header")
	call flush (outfd)
end


# PUTSUBRASTER -- Write data to the output from this subraster, look
# in the table to see if each subswath should be filled with data or zeros.

procedure putsubraster (outfd, array, nx, ny, table, subraster)

int	outfd			# output file descriptor
int	subraster		# subraster number
int	nx			# size of the data array (x)
int	ny			# size of the data array (y)
short	array[nx, ny]		# data array
short	table[SZ_TABLE]		# subswath lookup table

int	i, subswath, tableindex
pointer	sp, bufpointer
errchk	writesubswath

begin
	call smark (sp)
	call salloc (bufpointer, ny, TY_SHORT)

	do subswath = nx, 1, -1 {
	    tableindex = (subraster - 1) * nx + ((nx + 1) - subswath)
	    if (table[tableindex] == IS_DATA) {
		do i = ny, 1, -1
		     Mems[bufpointer + ny - i] = array[subswath,i]
		call writesubswath (outfd, Mems[bufpointer], ny)
	    } else
		next
	}

	call sfree(sp)
end


# WRITESUBSWATH -- Write data to file whose logical unit is outfd.
# Swap the bytes in each data word.

procedure writesubswath (outfd, buf, buflength)

int	outfd			# output file descriptor
int	buflength		# length of data buffer
short	buf[buflength]		# data buffer

int	fstati()
errchk	write

begin
	if (BYTE_SWAP2 == YES)
	    call bswap2 (buf, 1, buf, 1, buflength * SZB_SHORT)
	call write (outfd, buf, buflength*SZB_SHORT/SZB_CHAR)
	if (fstati (outfd, F_NCHARS) != buflength*SZB_SHORT/SZB_CHAR)
	    call error (0, "eof encountered when reading subswath")
end
