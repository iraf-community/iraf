include	<error.h>
include <mach.h>
include <ctype.h>
include	<fset.h>
include	<imhdr.h>
include <fset.h>
include "idsmtn.h"

# T_WIDSTAPE -- Convert each line of an IRAF image to IDSOUT text format.
# Each image line is treated as a one dimensional spectrum.
# A maximum IDSOUT length of 1024 points is enforced silently.
#
# There are two types of output:
#    single   -- All image lines are appended to a single IDSOUT file.
#    multiple -- Each image line is appended to a different IDSOUT file.

define	SZ_IDSTITLE	64		# Length of IDSOUT title
define	SZ_CARD		80		# Columns on a card

procedure t_widstape ()

char	image[SZ_FNAME]			# Image to be converted
char	idsout[SZ_FNAME]		# IDSOUT file or root name to be written

char	rec_numbers[SZ_LINE]
int	records[3, MAX_RANGES]
int	mfd, root, nfiles, block_size, nrecs
int	i, strsize
bool	ebcdic
pointer	im, sp, ids, pix

int	open(), mtopen(), clgeti(), clpopni(), clplen()
int	get_next_image(), decode_ranges(), mtfile()
int	strlen(), strldxs()
bool	clgetb()
pointer	immap(), imgl1r(), imgl2r()

begin
	# Get output file
	call clgstr ("idsout", idsout, SZ_FNAME)

	# Get root name and record numbers
	root   = clpopni ("input")
	nfiles = clplen (root)
	call clgstr ("records", rec_numbers, SZ_LINE)
	if (decode_ranges (rec_numbers, records, MAX_RANGES, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Get block size
	block_size = clgeti ("block_size")

	# Check that a realistic block size was requested
	if (mod (block_size, SZ_CARD) == 0)
	    block_size = block_size / SZB_CHAR
	else
	    call error (0, "Blocks not integral number of cards")

	# Open output tape file
	# First determine if a file number was specified
	if (mtfile (idsout) == YES) {
	    strsize = strlen (idsout)

	    # If no file, check if new_tape was specified and if so,
	    # force file=1; otherwise force file=EOT
	    if (strldxs ("]", idsout) != strsize) {
		if (!clgetb("new_tape")) {
		    call sprintf (idsout[strsize+1], SZ_FNAME, "%s")
			call pargstr ("[EOT]")

		} else {
		    call sprintf (idsout[strsize+1], SZ_FNAME, "%s")
		        call pargstr ("[1]")
		}

	    }
	    mfd = mtopen (idsout, WRITE_ONLY, block_size)

	} else
	    mfd =   open (idsout, NEW_FILE  , BINARY_FILE)

	# ASCII or EBCDIC
	ebcdic = clgetb ("ebcdic")

	call fseti (STDOUT, F_FLUSHNL, YES)
	call reset_next_image ()

	call smark (sp)
	call salloc (ids, LEN_IDS, TY_STRUCT)
	call salloc (POINT(ids), MAX_NCOEFF, TY_REAL)

	# Loop over all files
	while (get_next_image (root, records, nrecs, image, SZ_FNAME) != EOF) {

	    iferr (im = immap (image, READ_ONLY, 0)) {
		call erract (EA_WARN)
		next
	    } else {

		# Load header elements, pixels
		call load_ids_hdr (ids, im, 1)

		# Write out a spectrum for each line in the image
		if (IM_NDIM(im) == 1) {
		    pix = imgl1r (im)
		    call wrt_ids_rec (mfd, ids, im, image, Memr[pix], ebcdic)
		} else {
		    do i = 1, IM_LEN (im,2) {
			pix = imgl2r (im, i)

			# Write out record
			call wrt_ids_rec (mfd, ids, im, image, Memr[pix], 
			    ebcdic)
		    }
		}


		call printf ("copied - [%s]: %s\n")
		call pargstr (image)
		call pargstr (IM_TITLE(im))

		call imunmap (im)
	    }
	}

	call sfree (sp)
	call close (mfd)
end

# WRT_IDS_REC -- Write one IIDS/IRS format record in IDSOUT form

procedure wrt_ids_rec (mfd, ids, im, image, pix, ebcdic)

int	mfd
pointer	ids, im
char	image[SZ_FNAME]
real	pix[ARB]
bool	ebcdic

# IDSOUT header parameters
char	label[SZ_IDSTITLE]		# Record label
int	record				# Record number
int	uttime				# UT time in seconds
int	st				# Siderial time in seconds
real	ra				# Right Ascension in hours
real	dec				# Declination in degrees
real	ha				# Hour angle in hours
real	airmass				# Air mass
int	itime				# Integration time
real	wavelen1			# Wavelength of first pixel
real	dispersion			# Dispersion per pixel

int	i, rec_no
char	padline[SZ_LINE], bufline[SZ_LINE]

int	strmatch()

begin
	# Fill in header parameters.

	call strcpy (IM_TITLE(im), label, SZ_IDSTITLE)

	# The following two calculations were causing floating overflows
	# when the header values were indefinite.  SEH 7-23-86
	if (IS_INDEF(UT(ids)))
	    uttime = INDEFI
	else
	    uttime = UT(ids) * 3600.

	if (IS_INDEF(ST(ids)))
	    st = INDEFI
	else
	    st     = ST(ids) * 3600.

	ra         = RA(ids)
	dec        = DEC(ids)
	ha         = HA(ids)
	airmass    = AIRMASS(ids)
	itime      = ITM(ids)
	wavelen1   = W0(ids)
	dispersion = WPC(ids)

	# Create a padding line to fill the IDSOUT block to 1024 points.

	call sprintf (padline, SZ_LINE,
	    "%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e\n")
	do i = 1, 8
	    call pargr (0.)

	# Line 1 -- Record number, etc.
	rec_no = strmatch (image, ".")
	call sscan (image[rec_no])
	    call gargi (record)

	    call sprintf (bufline, SZ_LINE, 
		"%5d%5d%15.7e%15.7e%5d%5d%5d%5d%5d%5d%10d")
		call pargi (record)
		call pargi (itime)
		call pargr (wavelen1)
		call pargr (dispersion)
		call pargi (0)
		call pargi (IM_LEN(im, 1))
		call pargi (BEAM(ids))
		call pargi (-1)
		call pargi (-1)
		call pargi (0)
		call pargi (uttime)

	    call putcard (mfd, bufline, ebcdic)

	    # Line 2 -- Siderial time, RA, and Dec.

	    call sprintf (bufline, SZ_LINE, 
		"%10d%15.7e%15.7e%5d%5d%5d%5d%5d%5d%5d%5d")
		call pargi (st)
		call pargr (ra)
		call pargr (dec)
		call pargi (0)
		call pargi (DF_FLAG(ids))
		call pargi (SM_FLAG(ids))
		call pargi (QF_FLAG(ids))
		call pargi (DC_FLAG(ids))
		call pargi (QD_FLAG(ids))
		call pargi (EX_FLAG(ids))
		call pargi (BS_FLAG(ids))

	    call putcard (mfd, bufline, ebcdic)

	    # Line 3 -- Hour angle, air mass, UT date, and exposure title.

	    call sprintf (bufline, SZ_LINE, 
		"%5d%5d%2w%-3.3s%5d%15.7e%15.7e%27wEND")
		call pargi (CA_FLAG(ids))
		call pargi (CO_FLAG(ids))
		call pargstr ("IRF")
		call pargi (OFLAG(ids))
		call pargr (ha)
		call pargr (airmass)

	    call putcard (mfd, bufline, ebcdic)

	    # Line 4 -- Record label.
	    call sprintf (bufline, SZ_LINE, "%-77sEND")
		call pargstr (IM_TITLE(im))

	    call putcard (mfd, bufline, ebcdic)

	    # Lines 5 to 132

	    call putdata (mfd, pix, IM_LEN(im, 1), padline, bufline, ebcdic)

	    # Line 133 -- Blank line

	    call sprintf (bufline, SZ_LINE, "%80w")
	    call putcard (mfd, bufline, ebcdic)
end


# PUTDATA -- Format and output extraction data to IDSOUT length of 1024 points.
# Special effort is made to make the zero padding efficient.

procedure putdata (mfd, data, npts, padline, bufline, ebcdic)

int	mfd				# IDSOUT file descriptor
real	data[npts]			# Data
int	npts				# Number of data points
char	padline[ARB]			# Padding string
char	bufline[ARB]			# Output buffer string
bool	ebcdic				# Convert to ebcdic

int	i, j, k, l, n
int	index
double	ddata

int	dtoc3()

begin
	j = min (1024, npts)	# Maximum number of data points
	k = j / 8 * 8		# Index of last data point in last complete line
	if (k < j)
	   l = k + 8		# Index of last point in last line with data
	else
	   l = k

	# Write all complete data lines.

	index = 1
	do i = 1, k {
	    ddata = double (data[i])
	    n = dtoc3 (ddata, bufline[index], 10, 4, 'e', 10)
	    while (n < 10) {
		bufline[index+n] = ' '
		n = n + 1
	    }
	    index = index + 10
	    if (mod (i, 8) == 0) {
	        call putcard (mfd, bufline, ebcdic)
		index = 1
	    }
	}

	# Write partial data line.

	index = 1
	do i = k + 1, l {
	    if (i <= j) {
		ddata = double (data[i])
	        n = dtoc3 (ddata, bufline[index], 11, 5, 'e', 10)
	    } else
	        n = dtoc3 (0.D0, bufline[index], 11, 5, 'e', 10)
	    while (n < 10) {
		bufline[index+n] = ' '
		n = n + 1
	    }
	    index = index + 10
	    if (mod (i, 8) == 0) {
	        call putcard (mfd, bufline, ebcdic)
		index = 1
	    }
	}

	# Write remaining padding lines.

	do i = l + 1, 1024, 8
	    call putcard (mfd, padline, ebcdic)
end

# PUTCARD -- Convert to ebcdic if desired and write out card

procedure putcard (mfd, bufline, ebcdic)

int	mfd
char	bufline[ARB]
bool	ebcdic

char	packline[SZ_LINE]

begin
	if (ebcdic) {
	    call ascii_to_ebcdic (bufline, packline, SZ_CARD)
	    call achtsb (packline, packline, SZ_CARD)
	} else
	    call chrpak (bufline, 1, packline, 1, SZ_CARD)

	call write (mfd, packline, SZ_CARD/SZB_CHAR)
end
