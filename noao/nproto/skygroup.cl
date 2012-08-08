# SKYGROUP -- Group coordinate list on the sky.
#
# The input is list of ra (0h-24h or 0d-360d) and dec (-90d to 90d) in the
# first two columns followed by arbitrary data (usually a filename).
# This is complicated by the periodicities at 0h.

procedure skygroup (input, output)

file	input			{prompt="Input list"}
string	output			{prompt="Output rootname"}
string	extn = ""		{prompt="Optional output extension"}
real	sep = 60		{prompt="Separation between groups (arcsec)"}
string	raunit = "hr"		{prompt="RA unit (hr|deg)"}
bool	keepcoords = yes	{prompt="Keep coordinates in output?"}
string	raformat = "%.2h"	{prompt="Output RA format"}
string	decformat = "%.1h"	{prompt="Output DEC format"}

struct	*fd1, *fd2

begin
	file	in, out, fname, temp1, temp2, temp3
	int	i, j, n, n1
	real	dra, r1, d1, r2, d2, r3, d3, r4
	struct	fmtstr, data1, data2, data3

	# Temporary files.
	fname = mktemp ("tmp")
	in = fname // "in"
	temp1 = fname // "1"
	temp2 = fname // "2"
	temp3 = fname // "3"

	# Set parameters.
	fname = input
	out = output

	# Check for existing output files.
	files (out//"_[0-9][0-9][0-9][0-9]*"//extn, > temp1)
	count (temp1) | scan (n); delete (temp1, verify-)
	if (access(out) || n > 0)
	    error (1, "Output files already exist")

	if (raunit == "hr")
	    dra = 24
	else
	    dra = 360

	if (keepcoords)
	    fmtstr = "%d " // raformat // " " // decformat // " %s"
	else
	    fmtstr = "%d %s"
	fmtstr += "\n"

	# We start by sorting in dec.
	sort (fname, col=2, num+, > in)

	# Find jumps in dec bigger than the separation and then sort
	# in ra and find jumps in ra bigger than separation.  Handle
	# the wrap around at 0h by duplicating to extend beyond 24h.
	# The duplicates will be eliminated during the merging process.

	n = 1
	fd1 = in
	if (fscan (fd1, r1, d1, data1) == EOF)
	    error (1, "No data or badly formated data")
	while (fscan (fd1, r2, d2, data2) != EOF) {
	    print (r1, d1, data1, >> temp1)
	    if (r1 / max (0.001, dcos(d1)) * 3600 <= sep) {
		r4 = r1 + dra
		print (r4, d1, data1, >> temp1)
	    }
	    if (abs(d2-d1) <= sep) {
		r1 = r2; d1 = d2; data1 = data2
		next
	    }

	    r3 = r2; d3 = d2; data3 = data2

	    sort (temp1, col=1, num+, > temp2)
	    delete (temp1, verify-)
	   
	    fd2 = temp2
	    if (fscan (fd2, r1, d1, data1) == EOF);
	    while (fscan (fd2, r2, d2, data2) != EOF) {
		if (keepcoords)
		    printf (fmtstr, n, r1, d1, data1, >> temp3)
		else
		    printf (fmtstr, n, data1, >> temp3)
		skysep (r1, d1, r2, d2, raunit=raunit, verb-)
		if (skysep.sep <= sep) {
		    r1 = r2; d1 = d2; data1 = data2
		    next
		}

		n += 1
		r1 = r2; d1 = d2; data1 = data2
	    }
	    fd2 = ""; delete (temp2, verify-)

	    if (keepcoords)
		printf (fmtstr, n, r1, d1, data1, >> temp3)
	    else
		printf (fmtstr, n, data1, >> temp3)
	    n += 1

	    r1 = r3; d1 = d3; data1 = data3
	}
	fd1 = ""; delete (in, verify-)

	print (r1, d1, data1, >> temp1)
	if (r1 / max (0.001, dcos(d1)) * 3600 <= sep) {
	    r4 = r1 + dra
	    print (r4, d1, data1, >> temp1)
	}

	sort (temp1, col=1, num+, > temp2)
	delete (temp1, verify-)
       
	fd2 = temp2
	if (fscan (fd2, r1, d1, data1) == EOF);
	while (fscan (fd2, r2, d2, data2) != EOF) {
	    if (keepcoords)
		printf (fmtstr, n, r1, d1, data1, >> temp3)
	    else
		printf (fmtstr, n, data1, >> temp3)
	    skysep (r1, d1, r2, d2, raunit=raunit, verb-)
	    if (skysep.sep <= sep) {
		r1 = r2; d1 = d2; data1 = data2
		next
	    }

	    n += 1
	    r1 = r2; d1 = d2; data1 = data2
	}
	fd2 = ""; delete (temp2, verify-)

	if (keepcoords)
	    printf (fmtstr, n, r1, d1, data1, >> temp3)
	else
	    printf (fmtstr, n, data1, >> temp3)

	# Now write out the lists and check for duplicate which must be
	# merged.

	sort (temp3, col=1, num+, > temp1); delete (temp3, verify-)
	touch (temp2)
	fd1 = temp1
	if (fscan (fd1, i, data1) == EOF);
	while (fscan (fd1, j, data2) != EOF) {
	    if (data1 == data2) {
	        print (j, i, >> temp2)
		next
	    }
	    printf ("%s_%03d%s\n", out, i, extn) | scan (fname)
	    print (data1, >> fname)
	    i = j; data1 = data2
	}
	fd1 = ""; delete (temp1, verify-)
	printf ("%s_%03d%s\n", out, i, extn) | scan (fname)
	print (data1, >> fname)
	sort (temp2, col=1, num+, rev+) | unique (> temp1)
	delete (temp2, verify-)

	# Merge the lists.
	n1 = n
	fd1 = temp1
	while (fscan (fd1, j, i) != EOF) {
	    printf ("%s_%03d%s\n", out, j, extn) | scan (fname)
	    if (access (fname)) {
		printf ("%s_%03d%s\n", out, i, extn) | scan (in)
		concat (in, fname, append+)
		delete (in, verify-)
		n1 -= 1
	    }
	}
	fd1 = ""; delete (temp1, verify-)

	# Renumber if needed.
	if (n1 != n) {
	    i = 1
	    for (j=1; j<=n; j+=1) {
		printf ("%s_%03d%s\n", out, j, extn) | scan (fname)
	        if (access(fname)) {
		    if (i != j) {
			printf ("%s_%03d%s\n", out, i, extn) | scan (in)
			rename (fname, in)
		    }
		    i += 1
		}
	    }
	}

	# Create the final output list of lists.
	files (out//"_[0-9]*", > out//extn)
end
