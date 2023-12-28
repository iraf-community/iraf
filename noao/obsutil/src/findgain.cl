# FINDGAIN - calculate the gain and readnoise given two flats and two
# bias frames.  Algorithm (method of Janesick) courtesy Phil Massey.
#
#	flatdif = flat1 - flat2
#	biasdif = bias1 - bias2
#
#	e_per_adu = ((mean(flat1)+mean(flat2)) - (mean(bias1)+mean(bias2))) /
#		    ((rms(flatdif))**2 - (rms(biasdif))**2)
#
#	readnoise = e_per_adu * rms(biasdif) / sqrt(2)
#
# In our implementation, `mean' may actually be any of `mean',
# `midpt', or `mode' as in the IMSTATISTICS task.

procedure findgain (flat1, flat2, zero1, zero2)

string	flat1			{prompt="First flat frame"}
string	flat2			{prompt="Second flat frame"}
string	zero1			{prompt="First zero frame"}
string	zero2			{prompt="Second zero frame"}

string	section = ""		{prompt="Selected image section"}
string	center = "mean"		{prompt="Central statistical measure",
				    enum="mean|midpt|mode"}
int	nclip = 3		{prompt="Number of clipping iterations"}
real	lsigma = 4		{prompt="Lower clipping sigma factor"}
real	usigma = 4		{prompt="Upper clipping sigma factor"}
real	binwidth = 0.1		{prompt="Bin width of histogram in sigma"}
bool	verbose = yes		{prompt="Verbose output?"}

string	*fd

begin
	bool	err
	file	f1, f2, z1, z2, lf1, lf2, lz1, lz2
	file	flatdif, zerodif, statsfile
	real	e_per_adu, readnoise, m_f1, m_f2, m_b1, m_b2, s_fd, s_bd, junk
	struct	images

	# Temporary files.
	flatdif = mktemp ("tmp$iraf")
	zerodif = mktemp ("tmp$iraf")
	statsfile = mktemp ("tmp$iraf")

	# Query parameters.
	f1	= flat1
	f2	= flat2
	z1	= zero1
	z2	= zero2

	lf1 = f1 // section
	lf2 = f2 // section
	lz1 = z1 // section
	lz2 = z2 // section

	imarith (lf1, "-", lf2, flatdif)
	imarith (lz1, "-", lz2, zerodif)

	printf ("%s,%s,%s,%s,%s,%s\n",
	    lf1, lf2, lz1, lz2, flatdif, zerodif) | scan (images)
	imstat (images, fields=center//",stddev", lower=INDEF, upper=INDEF,
	    nclip=nclip, lsigma=lsigma, usigma=usigma, binwidth=binwidth,
	    format-, > statsfile)
	imdelete (flatdif, verify-)
	imdelete (zerodif, verify-)

	fd = statsfile
	err = NO
	if (fscan (fd, m_f1, junk) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s\n", lf1)
	    err = YES
	}
	if (fscan (fd, m_f2, junk) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s\n", lf2)
	    err = YES
	}
	if (fscan (fd, m_b1, junk) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s\n", lz1)
	    err = YES
	}
	if (fscan (fd, m_b2, junk) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s\n", lz1)
	    err = YES
	}
	if (fscan (fd, junk, s_fd) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s - %s\n",
		lf1, lf2)
	    err = YES
	}
	if (fscan (fd, junk, s_bd) != 2) {
	    printf ("WARNING: Failed to compute statisics for %s - %s\n",
		lz1, lz2)
	    err = YES
	}
	fd = ""; delete (statsfile, verify-)

	if (err == YES)
	    error (1, "Can't compute gain and readout noise")

	e_per_adu = ((m_f1 + m_f2) - (m_b1 + m_b2)) / (s_fd**2 - s_bd**2)
	readnoise = e_per_adu * s_bd / sqrt(2)

	# round to three decimal places
	e_per_adu = real (nint (e_per_adu * 1000.)) / 1000.
	readnoise = real (nint (readnoise * 1000.)) / 1000.

	# print results
	if (verbose) {
	    printf ("FINDGAIN:\n")
	    printf ("  center = %s, binwidth = %g\n", center, binwidth)
	    printf ("  nclip = %d, lsigma = %g, usigma = %g\n",
		nclip, lsigma, usigma)
	    printf ("\n  Flats      = %s  &  %s\n", lf1, lf2)
	    printf ("  Zeros      = %s  &  %s\n", lz1, lz2)
	    printf ("  Gain       = %5.2f electrons per ADU\n", e_per_adu)
	    printf ("  Read noise = %5.2f electrons\n", readnoise)
	} else
	    printf ("%5.2f\t%5.2f\n", e_per_adu, readnoise)
end
