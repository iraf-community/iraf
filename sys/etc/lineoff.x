# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# LINEOFF -- Textfile line offset package.  This is a simple little package
# used to keep track of the file offsets of the lines in a text file.
# The entry points are as follows.
#
#	  lp = lno_open (maxlines)
#	      lno_close (lp)
#	       lno_save (lp, line, loffset, tag)
#    OK|ERR = lno_fetch (lp, line, loffset, tag)
#
# The SAVE procedure is used to save line offsets in the database, and the
# FETCH procedure is used to look up line offsets, returning ERR if the offset
# of the line is not stored.

define	MIN_NLINES	64
define	LEN_LNODES	5
define	LNO_MAXLINES	Memi[$1]	# number of lines stored
define	LNO_SLOT	Memi[$1+1]	# cycles around available slots
define	LNO_LINENUMP	Memi[$1+2]	# pointer to array of line numbers
define	LNO_LINEOFFP	Memi[$1+3]	# pointer to array of line offsets
define	LNO_LINETAGP	Memi[$1+4]	# pointer to array of line tags

# LNO_OPEN -- Open the line offset descriptor.

pointer procedure lno_open (maxlines)

int	maxlines	# max lines to store offsets for
int	nlines
pointer	lp
errchk	calloc, malloc

begin
	nlines = max (MIN_NLINES, maxlines)

	call calloc (lp, LEN_LNODES, TY_STRUCT)
	LNO_MAXLINES(lp) = nlines
	call calloc (LNO_LINENUMP(lp), nlines, TY_LONG)
	call malloc (LNO_LINEOFFP(lp), nlines, TY_LONG)
	call malloc (LNO_LINETAGP(lp), nlines, TY_LONG)

	return (lp)
end


# LNO_CLOSE -- Return the line offset descriptor.

procedure lno_close (lp)

pointer	lp		# line offset descriptor

begin
	call mfree (LNO_LINENUMP(lp), TY_LONG)
	call mfree (LNO_LINEOFFP(lp), TY_LONG)
	call mfree (LNO_LINETAGP(lp), TY_LONG)
	call mfree (lp, TY_STRUCT)
end


# LNO_SAVE -- Save a line number/offset pair in the LNO database.

procedure lno_save (lp, line, loffset, ltag)

pointer	lp		# line offset descriptor
int	line		# line number
long	loffset		# line offset from NOTE
long	ltag		# tag value assoc. with line
int	slot

begin
	slot = LNO_SLOT(lp) + 1
	if (slot > LNO_MAXLINES(lp))
	    slot = 1
	LNO_SLOT(lp) = slot

	Memi[LNO_LINENUMP(lp)+slot-1] = line
	Meml[LNO_LINEOFFP(lp)+slot-1] = loffset
	Meml[LNO_LINETAGP(lp)+slot-1] = ltag
end


# LNO_FETCH -- Search the LNO database for an entry for the indicated line and
# return its file offset if found.  No assumptions are made about the ordering
# of the data since lines could have been entered in any order.  ERR is
# returned if the line is not found in the database.  A simple linear search
# is sufficient given that the applications using this package are not expected
# to look up a line often.

int procedure lno_fetch (lp, line, loffset, ltag)

pointer	lp		# line offset descriptor
int	line		# line number to search for
long	loffset		# receives line offset if entry for line is found
long	ltag		# receives tag value assoc. with line

int	maxl, i
pointer	nump, offp, tagp

begin
	maxl = LNO_MAXLINES(lp) - 1
	nump = LNO_LINENUMP(lp)
	offp = LNO_LINEOFFP(lp)
	tagp = LNO_LINETAGP(lp)
	
	do i = 0, maxl
	    if (Memi[nump+i] == line) {
		loffset = Meml[offp+i]
		ltag = Meml[tagp+i]
		return (OK)
	    }

	return (ERR)
end
