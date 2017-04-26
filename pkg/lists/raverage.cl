# RAVERAGE -- Running average, standard deviation, and enevelope of a list.

procedure raverage (input, nwin)

file	input			{prompt="Input file"}
int	nwin			{prompt="Number of points in window", min=1}
bool	sort = no		{prompt="Sort input?"}
real	nsig = 0		{prompt="Number of sigma for envelope"}

struct	*fd1, *fd2

begin
	file	in, tmp
	int	i, j, n, nin
	real	x, y, z, x1, y1, sum1, sum2, sum3, xavg, yavg, sig
	real	ylow, yhigh

	# Get query parameters.
	in = input
	nin = nwin

	if (nwin < 1)
	    error (1, "Window size must be greater than zero")

	# Buffer the standard input and sort if requested.
	tmp = in
	if (in == "STDIN") {
	    tmp = mktemp ("tmp$iraf")
	    i = 0
	    while (YES) {
		j = scan(x,y)
		if (j != 2) {
		    if (j == EOF)
			break
		    else if (j < 1)
			next
		    else if (j < 2) {
			y = x
			x = i + 1
		    }
		}
		i += 1
		print (x, y, >> tmp)
	    }
	    if (sort) {
	        rename (tmp, tmp//"a")
	        sort (tmp//"a", col=1, num+, rev-, > tmp)
		delete (tmp//"a", verify-)
	    }
	} else if (sort) {
	    tmp = mktemp ("tmp$iraf")
	    sort (in,, col=1, num+, rev-, > tmp)
	}

	# Initialize.
	i = 0; n = 0; sum1 = 0; sum2 = 0; sum3 = 0

	# Accumulate the first window.
	fd1 = tmp
	while (n<nin) {
	    j = fscan (fd1, x, y)
	    if (j != 2) {
		if (j == EOF)
		    break
		else if (j < 1)
		    next
		else if (j < 2) {
		    y = x
		    x = i + 1
		}
	    }
	    i += 1
	    n += 1
	    sum1 += x
	    sum2 += y
	    sum3 += y*y
	}

	# Read the rest of the list adding and subtracting.
	fd2 = tmp
	while (YES) {
	    j = fscan (fd1, x, y)
	    if (j != 2) {
		if (j == EOF)
		    break
		else if (j == 0)
		    next
		else if (j == 1) {
		    y = x
		    x = i + 1
		}
	    }

	    while (YES) {
	        j = fscan(fd2, x1, y1)
		if (j != 2) {
		    if (j == 0)
			next
		    else if (j == 1) {
			y1 = x1
			x1 = i - n + 1
		    }
		}
		break
	    }

	    xavg = sum1 / n
	    yavg = sum2 / n
	    sig = sqrt (max (0., (n * sum3 - sum2 * sum2) / (n * max(1,n-1))))
	    if (nsig > 0) {
	        ylow = yavg - nsig * sig
	        yhigh = yavg + nsig * sig
		printf ("%g %g %g %d %g %g\n", xavg, yavg, sig, n, ylow, yhigh)
	    } else
		printf ("%g %g %g %d\n", xavg, yavg, sig, n)

	    i += 1
	    sum1 += x - x1
	    sum2 += y - y1
	    sum3 += y*y - y1*y1
	}
	fd1 = ""; fd2 = ""

	# Compute the final values.
	if (n <= 0) {
	    xavg = INDEF
	    yavg = INDEF
	    sig = INDEF
	    ylow = INDEF
	    yhigh = INDEF
	} else {
	    xavg = sum1 / n
	    yavg = sum2 / n
	    sig = sqrt (max (0., (n * sum3 - sum2 * sum2) / (n * max(1,n-1))))
	    ylow = yavg - nsig * sig
	    yhigh = yavg + nsig * sig
	}
	if (nsig > 0)
	    printf ("%g %g %g %d %g %g\n", xavg, yavg, sig, n, ylow, yhigh)
	else
	    printf ("%g %g %g %d\n", xavg, yavg, sig, n)

	# Delete temporary files.
	if (tmp != in)
	    delete (tmp, verify-)
end
