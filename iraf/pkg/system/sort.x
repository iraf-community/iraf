# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<ctype.h>

# SORT -- Sort a text file, alphabetically or numerically, in forward or
# reverse order, by line or by the contents of any column.  Derived from
# Chap. 4 of Software Tools.  This is old code, not very pretty.

define	MERGEORDER	8
define	MAXPTR		20000
define	SZ_LINBUF	200000
define	LOGPTR		32
define	swap		{temp=$1;$1=$2;$2=temp}


# SORT -- The main CL callable routine.

procedure t_sort()

pointer	linbuf, linptr
int	infil[MERGEORDER], nlines
char	name[SZ_FNAME], source_file[SZ_FNAME]
int	high, lim, low, fd, outfil, t, list, junk

bool	clgetb()
int	ss_gtext(), ss_mkfile()
int	open(), clpopni(), clplen(), clgfil(), clgeti(), btoi()
include	"sort.com"

begin
	list = clpopni ("input_file")
	if (clplen (list) > 1) {
	    call clpcls (list)
	    call error (1, "Cannot yet sort more than one file at a time")
	}

	# Determine type of sort to be applied.
	column = clgeti ("column")
	numeric_sort = btoi (clgetb ("numeric_sort"))
	reverse_sort = btoi (clgetb ("reverse_sort"))

	ignore_whitespace = NO
	if (column == 0 && numeric_sort == NO)
	    if (clgetb ("ignore_whitespace"))
		ignore_whitespace = YES

	# The standard STRSRT is fastest if we can use it.
	use_strsrt = NO
	if (ignore_whitespace == NO && numeric_sort == NO &&
	    reverse_sort == NO && column == 0)
	    use_strsrt = YES

	# Allocate buffer space.
	call malloc (linbuf, SZ_LINBUF, TY_CHAR)
	call malloc (linptr, MAXPTR, TY_INT)

	# Perform the sort. 
	junk = clgfil (list, source_file, SZ_FNAME)
	fd = open (source_file, READ_ONLY, TEXT_FILE)

	# Initial formation of runs.
	high = 0
	repeat {
	    t = ss_gtext (fd, Memi[linptr], nlines, Memc[linbuf])
	    if (use_strsrt == YES)
		call strsrt (Memi[linptr], Memc[linbuf], nlines)
	    else
		call ss_quick (Memi[linptr], Memc[linbuf], nlines)

	    high = high + 1
	    outfil = ss_mkfile (high)
	    call ss_ptext (outfil, Memi[linptr], nlines, Memc[linbuf])
	    call close (outfil)

	} until (t == EOF)

	# Merge sorted chunks.
	for (low = 1;  low < high;  low = low + MERGEORDER) {
	    lim = min (low + MERGEORDER - 1, high)
	    call ss_gopen (infil, low, lim)
	    high = high + 1
	    outfil = ss_mkfile (high)
	    call ss_merge (infil, outfil, lim - low + 1)
	    call close (outfil)
	    call ss_gremov (infil, low, lim)
	}

	call mfree (linbuf, TY_CHAR)
	call mfree (linptr, TY_INT)

	call ss_gname (high, name)		# final cleanup
	outfil = open (name, READ_ONLY, TEXT_FILE)
	call fcopyo (outfil, STDOUT)
	call close (outfil)

	call delete (name)
	call close (fd)
end


# SS_MKFILE -- Open a temporary file.

int procedure ss_mkfile (n)

int	n
char	name[SZ_FNAME]
int	open()
errchk	open, ss_gname

begin
	call ss_gname (n, name)
	return (open (name, NEW_FILE, TEXT_FILE))
end


# SS_GNAME -- Quickie routine to generate a temporary file name in tmp$ (the
# original tools version would put the temp files in the cwd).  Make a name
# of the form "tmp$srtPID_XX", where XX is the file number argument N,
# and PID is the process pid.  The tools sort code requires that the file
# name be easily reproduced given only N, so we cannot use mktemp.

procedure ss_gname (n, name)

int	n
char	name[ARB]
int	pid

begin
	call zgtpid (pid)
	call sprintf (name, SZ_FNAME, "tmp$srt%d_%d")
	    call pargi (pid)
	    call pargi (n)
end


# SS_GOPEN -- Open a range of files.

procedure ss_gopen (infil, low, lim)

int	infil[ARB], lim, low
char	name[SZ_FNAME]
int	i, open()
errchk	open, ss_gname

begin
	for (i=1;  i <= lim-low+1;  i=i+1) {
	    call ss_gname (low+i-1, name)
	    infil[i] = open (name, READ_ONLY, TEXT_FILE)
	}
end


# SS_GREMOV -- Remove a range of files.

procedure ss_gremov (infil, low, lim)

int	infil[ARB], low, lim

int	i
char	name[SZ_FNAME]
errchk	ss_gname, delete, close

begin
	for (i=1;  i <= lim-low+1;  i=i+1) {
	    call close (infil[i])
	    call ss_gname (low+i-1, name)
	    call delete (name)
	}
end


# SS_MERGE -- Merge file onto outfil.

procedure ss_merge (infil, outfil, nfiles)

int	infil[ARB]		# input file numbers
int	outfil			# output file number
int	nfiles			# number of files to be merged

pointer	sp, linbuf
int	linptr[MERGEORDER]
int	i, inf, lbp, lp1, nf
int	getline()
errchk	getline, putline, ss_quick
include	"sort.com"

begin
	call smark (sp)
	call salloc (linbuf, MERGEORDER * SZ_LINE, TY_CHAR)

	lbp = 1
	nf = 0
	
	# Get one line from each file.
	for (i=1;  i <= nfiles;  i=i+1) {
	    if (getline (infil[i], Memc[linbuf+lbp-1]) != EOF) {
		nf = nf + 1
		linptr[nf] = lbp
		lbp = lbp + SZ_LINE
	    }
	}

	# Make initial heap.
	if (use_strsrt == YES)
	    call strsrt (linptr, Memc[linbuf], nf)
	else
	    call ss_quick (linptr, Memc[linbuf], nf)

	while (nf > 0) {
	    lp1 = linptr[1]
	    call putline (outfil, Memc[linbuf+lp1-1])
	    inf = lp1 / SZ_LINE + 1		# compute file index
	    if (getline (infil[inf], Memc[linbuf+lp1-1]) == EOF) {
		linptr[1] = linptr[nf]
		nf = nf - 1
	    }
	    call ss_reheap (linptr, Memc[linbuf], nf)
	}

	call sfree (sp)
end


# SS_REHEAP -- propagate linbuf[linptr[1]] to proper place in heap.

procedure ss_reheap (linptr, linbuf, nf)

int	linptr[ARB]
char	linbuf[ARB]
int	nf

int	i, j, temp
int	ss_compare()

begin
	for (i=1;  2 * i <= nf;  i=j) {
	    j = 2 * i
	    if (j < nf)			# find smaller child
		if (ss_compare (linptr[j], linptr[j+1], linbuf) > 0)
		    j = j + 1
	    if (ss_compare (linptr[i], linptr[j], linbuf) <= 0)
		break			# proper position found

	    swap (linptr[i], linptr[j])
	}
end


# SS_QUICK -- Quicksort for text data.  NOTE -- This algorithm is quadratic in
# the worst case, i.e., when the data is already sorted.  A random method of
# selecting the pivot should be used to improve the behaviour on sorted arrays.

procedure ss_quick (linptr, linbuf, nlines)

int	linptr[ARB]		# indices of strings in buffer
char	linbuf[ARB]		# string buffer
int	nlines			# number of strings

int	i, j, k, temp, lv[LOGPTR], p, pivlin, uv[LOGPTR]
int	ss_compare()

begin
	lv[1] = 1
	uv[1] = nlines
	p = 1

	while (p > 0) {
	    if (lv[p] >= uv[p])		# only one elem in this subset
		p = p - 1		# pop stack
	    else {
		# Dummy loop to trigger optimizer.
		do p = p, ARB {
		    i = lv[p] - 1
		    j = uv[p]

		    # Select pivot element at midpoint of interval to avoid
		    # quadratic behavior on a sorted list.

		    k = (lv[p] + uv[p]) / 2
		    swap (linptr[j], linptr[k])
		    pivlin = linptr[j]

		    while (i < j) {
			for (i=i+1; ss_compare (linptr[i], pivlin, linbuf) < 0;
			    i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (ss_compare (linptr[j], pivlin, linbuf) <= 0)
				break
			if (i < j)		# out of order pair
			    swap (linptr[i], linptr[j])
		    }

		    j = uv[p]			# move pivot to position i
		    swap (linptr[i], linptr[j])

		    if (i-lv[p] < uv[p] - i) {	# stack so shorter done first
			lv[p+1] = lv[p]
			uv[p+1] = i - 1
			lv[p] = i + 1
		    } else {
			lv[p+1] = i + 1
			uv[p+1] = uv[p]
			uv[p] = i - 1
		    }

		    break
		}

		p = p + 1			# push onto stack
	    }
	}
end


# SS_COMPARE -- Compare two strings.  Return -1 if str1<str2, 1 if str1>str2,
# or 0 if the two strings are equal.

int procedure ss_compare (lp1, lp2, linbuf)

int	lp1, lp2		# pointers to substrings in linbuf
char	linbuf[ARB]		# text buffer

double	num1, num2
int	ip1, ip2, answer, len1, len2
int	strcmp(), ss_findcolumn(), gctod()
include	"sort.com"

begin
	if (column != 0) {
	    ip1 = ss_findcolumn (linbuf, lp1, column)
	    ip2 = ss_findcolumn (linbuf, lp2, column)
	} else if (ignore_whitespace == YES) {
	    for (ip1=lp1;  IS_WHITE(linbuf[ip1]);  ip1=ip1+1)
		;
	    for (ip2=lp2;  IS_WHITE(linbuf[ip2]);  ip2=ip2+1)
		;
	} else {
	    ip1 = lp1
	    ip2 = lp2
	}

	if (numeric_sort == YES) {
	    len1 = gctod (linbuf, ip1, num1)
	    len2 = gctod (linbuf, ip2, num2)

	    # If fields are nonnumeric, compare as strings.
	    if (len1 == 0 || len2 == 0)
		answer = strcmp (linbuf[ip1-len1], linbuf[ip2-len2])
	    else if (num1 < num2)
		answer = -1
	    else if (num1 > num2)
		answer = 1
	    else
		answer = 0
	} else
	    answer = strcmp (linbuf[ip1], linbuf[ip2])

	if (reverse_sort == YES)
	    return (-answer)
	else
	    return (answer)
end


# SS_FINDCOLUMN -- Determine the offset of the first character in the 
# desired column.

int procedure ss_findcolumn (buf, start, column)

char	buf[ARB]
int	start, column
int	ip, col

begin
	for (ip=start;  IS_WHITE(buf[ip]);  ip=ip+1)
	    ;
	for (col=1;  col < column;  col=col+1) {
	    for (;  !IS_WHITE(buf[ip]) && buf[ip] != EOS;  ip=ip+1)
		;
	    for (;  IS_WHITE(buf[ip]);  ip=ip+1)
		;
	}

	return (ip)
end


# SS_GTEXT -- Get text lines into linbuf.

int procedure ss_gtext (infile, linptr, nlines, linbuf)

int	infile, linptr[ARB], nlines
char	linbuf[ARB]

int	lbp, len, getline()
errchk	getline

begin
	nlines = 0
	lbp = 1

	repeat {
	    len = getline (infile, linbuf[lbp])
	    if (len == EOF)
		break
	    else if (len == 1)
		# ignore blank lines
	    else {
		nlines = nlines + 1
		linptr[nlines] = lbp
		lbp = lbp + len + 1		# '1' = room for EOS
	    }
	} until (lbp >= SZ_LINBUF - SZ_LINE || nlines >= MAXPTR)

	return (len)
end


# SS_PTEXT -- Output text lines from linbuf to outfile.

procedure ss_ptext (outfil, linptr, nlines, linbuf)

int	outfil, linptr[ARB], nlines
char	linbuf[ARB]
int	i, j
errchk	putline

begin
	for (i=1;  i <= nlines;  i=i+1) {
	    j = linptr[i]
	    call putline (outfil, linbuf[j])
	}
end
