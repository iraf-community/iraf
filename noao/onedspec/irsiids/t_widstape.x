include	<mach.h>
include	<error.h>
include	<imhdr.h>
include <smw.h>

define	SZ_IDSTITLE	64		# Length of IDSOUT title
define	SZ_CARD		80		# Columns on a card

# T_WIDSTAPE -- Convert each line of an IRAF image to IDSOUT text format.
# Each image line is treated as a one dimensional spectrum.
# A maximum IDSOUT length of 1024 points is enforced silently.
#
# There are two types of output:
#    single   -- All image lines are appended to a single IDSOUT file.
#    multiple -- Each image line is appended to a different IDSOUT file.

procedure t_widstape ()

pointer	image			# Image to be converted
pointer	recs			# Record numbers
pointer	idsout			# IDSOUT file or root name to be written
int	block_size		# Block size
bool	ebcdic			# ASCII or EBCDIC

int	i, mfd, root, nrecs
pointer	sp, im, mw, sh, ptr

int	open(), mtopen(), clgeti(), clpopni()
int	get_next_image(), decode_ranges(), mtfile(), mtneedfileno()
bool	clgetb()
pointer	immap(), smw_openim()
errchk	immap, smw_openim, shdr_open, wrt_ids_rec

begin
	call smark (sp)
	call salloc (image, SZ_LINE, TY_CHAR)
	call salloc (idsout, SZ_FNAME, TY_CHAR)
	call salloc (recs, 300, TY_INT)

	# Parameters
	root   = clpopni ("input")
	call clgstr ("records", Memc[image], SZ_LINE)
	call clgstr ("idsout", Memc[idsout], SZ_FNAME)
	block_size = clgeti ("block_size")
	ebcdic = clgetb ("ebcdic")

	# Set record numbers
	if (decode_ranges (Memc[image], Memi[recs], 100, nrecs) == ERR)
	    call error (0, "Bad range specification")

	# Check that a realistic block size was requested
	if (mod (block_size, SZ_CARD) == 0)
	    block_size = block_size / SZB_CHAR
	else
	    call error (0, "Blocks not integral number of cards")

	# Open output tape file
	# First determine if a file number was specified
	if (mtfile (Memc[idsout]) == YES) {

	    # If no file, check if new_tape was specified and if so,
	    # force file=1; otherwise force file=EOT

	    if (mtneedfileno (Memc[idsout]) == YES) {
		if (!clgetb("new_tape"))
		    call mtfname (Memc[idsout], EOT, Memc[idsout], SZ_FNAME)

		else
		    call mtfname (Memc[idsout], 1, Memc[idsout], SZ_FNAME)
	    }
	    mfd = mtopen (Memc[idsout], WRITE_ONLY, block_size)
	} else
	    mfd = open (Memc[idsout], NEW_FILE, BINARY_FILE)

	# Loop over all files
	call reset_next_image ()
	while (get_next_image (root, Memi[recs], nrecs, Memc[image],
	    SZ_LINE) != EOF) {
	    iferr {
		im = NULL
		mw = NULL
		ptr = immap (Memc[image], READ_ONLY, 0); im = ptr
		ptr = smw_openim (im); mw = ptr

		# Write out a spectrum for each line in the image
		do i = 1, IM_LEN (im,2) {
		    call shdr_open (im, mw, i, 1, INDEFI, SHDATA, sh)
		    call wrt_ids_rec (mfd, sh, Memc[image], ebcdic)
		}

		call printf ("copied - [%s]: %s\n")
		call pargstr (IMNAME(sh))
		call pargstr (TITLE(sh))
		call flush (STDOUT)
	    } then
		call erract (EA_WARN)

	    if (mw != NULL)
		call smw_close (mw)
	    if (im != NULL)
		call imunmap (im)
	}

	call shdr_close (sh)
	call close (mfd)
	call sfree (sp)
end


# WRT_IDS_REC -- Write one IIDS/IRS format record in IDSOUT form

procedure wrt_ids_rec (mfd, sh, image, ebcdic)

int	mfd
pointer	sh
char	image[SZ_FNAME]
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

int     i, rec_no, df, sm, qf, qd, bs, co
pointer	sp, padline, bufline

int	strmatch(), imgeti()

begin
	call smark (sp)
	call salloc (padline, SZ_LINE, TY_CHAR)
	call salloc (bufline, SZ_LINE, TY_CHAR)

	# Fill in header parameters.

	call strcpy (TITLE(sh), label, SZ_IDSTITLE)

	# The following two calculations were causing floating overflows
	# when the header values were indefinite.  SEH 7-23-86
	if (IS_INDEF(UT(sh)))
	    uttime = INDEFI
	else
	    uttime = UT(sh) * 3600.

	if (IS_INDEF(ST(sh)))
	    st = INDEFI
	else
	    st     = ST(sh) * 3600.

	ra         = RA(sh)
	dec        = DEC(sh)
	ha         = HA(sh)
	airmass    = AM(sh)
	itime      = IT(sh)
	wavelen1   = W0(sh)
	dispersion = WP(sh)

	iferr (df = imgeti (IM(sh), "DF-FLAG"))
	    df = -1
	iferr (sm = imgeti (IM(sh), "SM-FLAG"))
	    sm = -1
	iferr (qf = imgeti (IM(sh), "QF-FLAG"))
	    qf = -1
	iferr (qd = imgeti (IM(sh), "QD-FLAG"))
	    qd = -1
	iferr (bs = imgeti (IM(sh), "BS-FLAG"))
	    bs = -1
	iferr (co = imgeti (IM(sh), "CO-FLAG"))
	    co = -1

	# Create a padding line to fill the IDSOUT block to 1024 points.

	call sprintf (Memc[padline], SZ_LINE,
	    "%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e%10.4e\n")
	do i = 1, 8
	    call pargr (0.)

	# Line 1 -- Record number, etc.
	rec_no = strmatch (image, ".")
	call sscan (image[rec_no])
	    call gargi (record)

	    call sprintf (Memc[bufline], SZ_LINE, 
		"%5d%5d%15.7e%15.7e%5d%5d%5d%5d%5d%5d%10d")
		call pargi (record)
		call pargi (itime)
		call pargr (wavelen1)
		call pargr (dispersion)
		call pargi (0)
		call pargi (SN(sh))
		call pargi (BEAM(sh))
		call pargi (-1)
		call pargi (-1)
		call pargi (0)
		call pargi (uttime)

	    call putcard (mfd, Memc[bufline], ebcdic)

	    # Line 2 -- Siderial time, RA, and Dec.

	    call sprintf (Memc[bufline], SZ_LINE, 
		"%10d%15.7e%15.7e%5d%5d%5d%5d%5d%5d%5d%5d")
		call pargi (st)
		call pargr (ra)
		call pargr (dec)
		call pargi (0)
		call pargi (df)
		call pargi (sm)
		call pargi (qf)
		call pargi (DC(sh))
		call pargi (qd)
		call pargi (EC(sh))
		call pargi (bs)

	    call putcard (mfd, Memc[bufline], ebcdic)

	    # Line 3 -- Hour angle, air mass, UT date, and exposure title.

	    call sprintf (Memc[bufline], SZ_LINE, 
		"%5d%5d%2w%-3.3s%5d%15.7e%15.7e%27wEND")
		call pargi (FC(sh))
		call pargi (co)
		call pargstr ("IRF")
		call pargi (OFLAG(sh))
		call pargr (ha)
		call pargr (airmass)

	    call putcard (mfd, Memc[bufline], ebcdic)

	    # Line 4 -- Record label.
	    call sprintf (Memc[bufline], SZ_LINE, "%-77sEND")
		call pargstr (TITLE(sh))

	    call putcard (mfd, Memc[bufline], ebcdic)

	    # Lines 5 to 132

	    call putdata (mfd, Memr[SY(sh)], SN(sh), Memc[padline],
		Memc[bufline], ebcdic)

	    # Line 133 -- Blank line

	    call sprintf (Memc[bufline], SZ_LINE, "%80w")
	    call putcard (mfd, Memc[bufline], ebcdic)
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
