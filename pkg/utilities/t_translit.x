# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<pattern.h>
include	<ctype.h>
include	<chars.h>
include <fset.h>

# TRANSLIT -- Copy a file or files, replacing specified characters by
# other characters, or deleting specified characters.

define	NCHARS	128
define	ON	1
define	OFF	0

procedure t_translit()

char	from_string[NCHARS], to_string[NCHARS]

char	to[NCHARS], from[NCHARS], lut[NCHARS], infile[SZ_FNAME], endto
char	line[SZ_LINE], lastchar
int	del[NCHARS], collap[NCHARS]
int	list, delete, allbut, lastfrom, lastto, collapse, in, i, op, nchars

bool	clgetb()
int	clpopni(), makeset(), strlen(), clgfil(), open(), getline()

begin
	call fseti (STDOUT, F_FLUSHNL, YES)
	list = clpopni ("infile")

	# Make from and to character sets
	call clgstr ("from_string", from_string, NCHARS)
	if (from_string[1] == CH_NOT) {
	    allbut = YES
	    if (makeset (from_string, 2, from, NCHARS) == ERR)
		call error (1, "From_string too large.")
	} else {
	    allbut = NO
	    if (makeset (from_string, 1, from, NCHARS) == ERR)
		call error (2, "From_string too large.")
	}

	if (clgetb ("delete")) {
	    delete = YES
	    to[1] = EOS
        } else {
	    delete = NO
	    call clgstr ("to_string", to_string, NCHARS)
	    if (makeset (to_string, 1, to, NCHARS) == ERR)
		call error (3, "To_string too large.")
	}
	
	lastfrom = strlen (from)
	lastto = strlen (to)
	endto = to[lastto]

	# Expand to set
	if (delete == NO) {
	    for (i = lastto + 1; i <= NCHARS; i = i + 1)
	        to[i] = endto
	    to[i] = EOS
	}

	# Collapse data ?
	if (delete == YES) {
	    collapse = NO
	} else if (allbut == YES) {
	    collapse = YES
	} else if (lastfrom > lastto) {
	    if (! clgetb ("collapse"))
		collapse = NO
	    else
		collapse = YES
	} else {
	    collapse = NO
	}

	# Set up transformations

	# Initialize lookup table, delete and collapse vectors
	call makelut (lut, NCHARS)
	call amovki (OFF, del, NCHARS)
	call amovki (OFF, collap, NCHARS)
	
	# Delete array
	if (delete == YES) {
	    do i = 1, lastfrom
		del[from[i] + 1] = ON
	}

	# Collapse array
	do i = 1, lastfrom
	    collap[from[i] + 1] = ON

	# Allbut?
	if (allbut == YES) {
	    if (delete == YES)
	        call axorki (del, ON,  del, NCHARS)
	    call axorki (collap, ON, collap, NCHARS)
	}

	# Set up the transformation
	if (delete == NO) {
	    op = 1
	    do i = 1, NCHARS {
		if (collap[i] == ON) {
		    lut[i] = to[op]
		    op = op + 1
		}
	    }
	}

	# Loop over the files
	while (clgfil (list, infile, SZ_FNAME) != EOF) {

	    in = open (infile, READ_ONLY, TEXT_FILE)
	    lastchar = EOF

	    repeat {

		nchars = getline (in, line)
		if (nchars == EOF)
		    break
		op = 1

		if (delete == YES) {
		    call del_line (line, line, nchars, op, lut, del)
		} else if (collapse == YES) {
		    call col_line (line, line, nchars, op, lut, collap, endto,
				    lastchar)
		} else {
		    call map_line (line, line, nchars, op, lut)
		}

		call putline (STDOUT, line)

	    }
	    call close (in)
	}

	call clpcls (list)
end
	

# MAKESET -- Procedure to make to and from character sets.
	
int procedure makeset (array, k, set, size)

char	array[ARB], set[ARB]
int	k, size

int	i, j
	
begin
	i = k
	j = 1

	call filset ("", array, i, set, j, size)
	call chdeposit ("", set, size + 1, j)

	if (j > size + 1)
	    return (ERR)
	else
	    return (OK)
end


# FILSET -- Process a character class into a simple list of characters.

procedure filset (delim, patstr, ip, patbuf, op, sz_pat)

char	patstr[ARB], delim, patbuf[ARB]
int	ip, sz_pat, op
char	ch, ch1, ch2
int	cctoc()

begin
	for (;  patstr[ip] != delim && patstr[ip] != EOS;  ip=ip+1) {
	    if (patstr[ip] == ESCAPE) {				# escape seq.
		if (cctoc (patstr, ip, ch) == 1)
		    ch = patstr[ip]
		else
		    ip = ip - 1
		call chdeposit (ch, patbuf, sz_pat, op)

	    } else if (patstr[ip] != CH_RANGE) {
		call chdeposit (patstr[ip], patbuf, sz_pat, op)

	    } else if (op <= 1 || patstr[ip+1] == EOS) {	# literal "-"
		ch = CH_RANGE
		call chdeposit (ch, patbuf, sz_pat, op)

	    # Here if char is CH_RANGE, denoting a range of characters to
	    # be included in the character class.  Range is valid only if
	    # limit chars are both digits, both lower case, or both upper case.

	    } else {
		ch1 = patbuf[op-1]		# not same as patstr[ip-1]
		ch2 = patstr[ip+1]

		if ((IS_DIGIT (ch1) && IS_DIGIT (ch2)) ||
		    (IS_LOWER (ch1) && IS_LOWER (ch2)) ||
		    (IS_UPPER (ch1) && IS_UPPER (ch2))) {
			if (ch1 <= ch2)
			    for (ch=ch1+1;  ch <= ch2;  ch=ch+1)
				call chdeposit (ch, patbuf, sz_pat, op)
			else
			    for (ch=ch1-1;  ch >= ch2;  ch=ch-1)
				call chdeposit (ch, patbuf, sz_pat, op)
			ip = ip + 1
		} else {
		    ch = CH_RANGE
		    call chdeposit (ch, patbuf, sz_pat, op)
		}
	    }
	}
end


# MAKELUT -- Make lookup table

procedure makelut (lut, nchars)

char	lut[ARB]
int	nchars

int	i

begin
	do i = 1, nchars
	    lut[i] = char (i - 1)
end


# DEL_LINE -- Procedure to delete characters from a line

procedure del_line (inline, outline, nchars, op, lut, delete)

char	inline[ARB], outline[ARB], lut[ARB]
int	nchars, op, delete[ARB]

int	i

begin
	do i = 1, nchars {
	    if (delete[inline[i] + 1] == OFF) {
		outline[op] = lut[inline[i] + 1]
		op = op + 1
	    }
	}
	outline[op] = EOS
end


# MAP_LINE -- Procedure to map a line

procedure map_line (inline, outline, nchars, op, lut)

char	inline[ARB], outline[ARB], lut[ARB]
int	nchars, op

int	i

begin
	do i = 1, nchars {
	    outline[op] = lut[inline[i] + 1]
	    op = op + 1
	}
	outline[op] = EOS
end


# COL_LINE -- Procedure to collapse line

procedure col_line (inline, outline, nchars, op, lut, collap, endto, lastchar)

char	inline[ARB], outline[ARB], lut[ARB], endto, lastchar
int	nchars, op, collap[ARB]

int	i

begin
	do i = 1, nchars {
	    if (collap[inline[i] +1] == ON && lut[inline[i] + 1] == endto &&
		    lastchar == endto) {
		;
	    } else {
		outline[op] = lut[inline[i] + 1]
		op = op + 1
	    }
	    lastchar = lut[inline[i] + 1]
	}
	outline[op] = EOS
end
