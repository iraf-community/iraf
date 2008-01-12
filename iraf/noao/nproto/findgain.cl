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


procedure findgain (flat1, flat2, bias1, bias2)

string	flat1			{prompt="First flat frame"}
string	flat2			{prompt="Second flat frame"}
string	bias1			{prompt="First bias frame"}
string	bias2			{prompt="Second bias frame"}

string	section = "[*,*]"	{prompt="Selected image section"}

string	center = "mean"		{prompt="Central statistical measure",
				    enum="mean|midpt|mode"}
real	binwidth = 0.1		{prompt="Bin width of histogram in sigma"}

bool	verbose = yes		{prompt="Verbose output?"}

string	*list

begin
	string	lflat1, lflat2, lbias1, lbias2, flatdif, biasdif, statsfile
	real	e_per_adu, readnoise, m_f1, m_f2, m_b1, m_b2, s_fd, s_bd, junk
	bool	sc_err

	flatdif = mktemp ("tmp$FG")
	biasdif = mktemp ("tmp$FG")
	statsfile = mktemp ("tmp$FG")

	lflat1 = flat1 // section
	lflat2 = flat2 // section
	lbias1 = bias1 // section
	lbias2 = bias2 // section

	imarith (lflat1, "-", lflat2, flatdif)
	imarith (lbias1, "-", lbias2, biasdif)

	imstatistics (lflat1//","//lflat2//","//lbias1//","//lbias2//
	    ","//flatdif//","//biasdif, fields=center//",stddev",
	    lower=INDEF, upper=INDEF, binwidth=binwidth, format-, > statsfile)

	list = statsfile
	sc_err = no

	if (fscan (list, m_f1, junk) != 2)
	    sc_err = yes
	if (fscan (list, m_f2, junk) != 2)
	    sc_err = yes
	if (fscan (list, m_b1, junk) != 2)
	    sc_err = yes
	if (fscan (list, m_b2, junk) != 2)
	    sc_err = yes
	if (fscan (list, junk, s_fd) != 2)
	    sc_err = yes
	if (fscan (list, junk, s_bd) != 2)
	    sc_err = yes
	list = ""

	if (! sc_err) {
	     e_per_adu = ((m_f1 + m_f2) - (m_b1 + m_b2)) / (s_fd**2 - s_bd**2)
	     readnoise = e_per_adu * s_bd / sqrt(2)

	     # round to three decimal places
	     e_per_adu = real (nint (e_per_adu * 1000.)) / 1000.
	     readnoise = real (nint (readnoise * 1000.)) / 1000.

	     if (verbose) {
	         print ("Gain       = ", e_per_adu, " electrons per ADU")
	         print ("Read noise = ", readnoise, " electrons\n")

	         print ("Flats      = ", lflat1, "  &  ", lflat2)
	         print ("Biases     = ", lbias1, "  &  ", lbias2)
	     } else {
	         print (e_per_adu, "\t", readnoise)
	     }
	}

	delete (statsfile, ver-, >& "dev$null")
	imdelete (flatdif, ver-, >& "dev$null")
	imdelete (biasdif, ver-, >& "dev$null")
end
