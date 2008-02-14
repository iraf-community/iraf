# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <ctype.h>
include "xhelp.h"

define  MAXPTR          20000
define  SZ_LINBUF       300000


# XH_SORT_LIST -- Take a list of words (as with a package list) and sort them.

procedure xh_sort_list (list)

char	list[ARB]					#u list to be sorted

pointer	sp, index, buf, ip
int	i, j, count, len

int	strlen()

begin
	len = strlen (list)

	call smark (sp)
	call salloc (index, SZ_HELPLIST, TY_INT)
	call salloc (buf, len+2, TY_CHAR)

	# Build up the index array.
	count = 1
	Memi[index] = 1
	for (i=2; i<len; i=i+1) {
	    if (list[i] == ' ') {
		list[i] = EOS
		Memi[index+count] = i + 1
		count = count + 1
	    }
	}

	# Sort the list.
	call strsrt (Memi[index], list, count)

	# Restore the list.
	ip = buf
	do i = 1, count {
	    for (j=0; list[Memi[index+i-1]+j] != EOS; j=j+1) {
	        Memc[ip] = list[Memi[index+i-1]+j]
	        ip = ip + 1
	    }
	    Memc[ip] = ' '
	    ip = ip + 1
	}
	Memc[ip-1] = EOS
	call strcpy (Memc[buf], list, strlen(Memc[buf]))
	call sfree (sp)
end


# XH_FILE_SORT -- Sort the lines in the named file.

procedure xh_file_sort (fname)

char	fname[SZ_FNAME]				#i file to be sorted

pointer	linbuf, linptr
int	nlines, fd
int	open()

begin
        call calloc (linptr, MAXPTR, TY_INT)
        call calloc (linbuf, SZ_LINBUF, TY_CHAR)

	# Sort the file then write it back out.
	fd = open (fname, READ_ONLY, TEXT_FILE)
	call xh_gtext (fd,  Memi[linptr], nlines, Memc[linbuf])
	call close (fd)
	call delete (fname)

        call xh_quick (Memi[linptr], Memc[linbuf], nlines)

	fd = open (fname, NEW_FILE, TEXT_FILE)
        call xh_ptext (fd, Memi[linptr], nlines, Memc[linbuf])

	call mfree (linbuf, TY_CHAR)
	call mfree (linptr, TY_INT)
	call close (fd)
end


# XH_GTEXT -- Get text lines into linbuf.

procedure xh_gtext (infile, linptr, nlines, linbuf)

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
end


# XH_PTEXT -- Output text lines from linbuf to outfile.

procedure xh_ptext (outfil, linptr, nlines, linbuf)

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


# XH_QUICK -- Quicksort for text data.  NOTE -- This algorithm is quadratic in
# the worst case, i.e., when the data is already sorted.  A random method of
# selecting the pivot should be used to improve the behaviour on sorted arrays.

procedure xh_quick (linptr, linbuf, nlines)

int	linptr[ARB]		# indices of strings in buffer
char	linbuf[ARB]		# string buffer
int	nlines			# number of strings

define  LOGPTR          32
define  swap            {temp=$1;$1=$2;$2=temp}

int	i, j, k, temp, lv[LOGPTR], p, pivlin, uv[LOGPTR]
int	xh_compare()

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
			for (i=i+1; xh_compare (linptr[i], pivlin, linbuf) < 0;
			    i=i+1)
			    ;
			for (j=j-1;  j > i;  j=j-1)
			    if (xh_compare (linptr[j], pivlin, linbuf) <= 0)
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


# XH_COMPARE -- Compare two strings.  Return -1 if str1<str2, 1 if str1>str2,
# or 0 if the two strings are equal.

int procedure xh_compare (lp1, lp2, linbuf)

int	lp1, lp2		# pointers to substrings in linbuf
char	linbuf[ARB]		# text buffer

int	ip1, ip2, answer
int	strcmp()

begin
	for (ip1=lp1;  IS_WHITE(linbuf[ip1]);  ip1=ip1+1)
		;
	for (ip2=lp2;  IS_WHITE(linbuf[ip2]);  ip2=ip2+1)
		;
	answer = strcmp (linbuf[ip1], linbuf[ip2])
	return (answer)
end
