# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	"qpoe.h"

.help qp_gnfn
.nf --------------------------------------------------------------------------
QP_GNFN -- Access the file header as a parameter list.

	     list = qp_ofnl[su] (qp, template)
	   nchars|EOF = qp_gnfn (list, outstr, maxch)
	        len = qp_lenfnl (list)
		     qp_seekfnl (list, pos)
			qp_cfnl (list)

These routines are used to determine the names of the fields (parameters) in
the QPOE file header, e.g., to list out the header.  QP_OFNLS and QP_ONFLU open
the file header (sorted or unsorted).  QP_GNFN returns the next parameter name,
returning as the function value the string length of the parameter name or EOF
when the end of the list is reached.  QP_SFNL seeks on or rewinds the list.
QP_CFNL closes the list descriptor.
.endhelp --------------------------------------------------------------------

# Size limiting definitions.
define	DEF_LENOFFV	128		# initial length of keywd-offset vector
define	INC_LENOFFV	128		# increment to above
define	DEF_SZSBUF	1024		# initial size of string buffer
define	INC_SZSBUF	1024		# increment to above

# List descriptor.
define	LEN_FL		3
define	FL_LEN		Memi[$1]	# number of names in list
define	FL_POS		Memi[$1+1]	# current position
define	FL_SBUF		Memi[$1+2]	# pointer to string buffer
define	FL_OFFV		Memi[$1+3]	# pointer to offset vector


# QP_OFNLS -- Open a sorted field name list.

pointer procedure qp_ofnls (qp, template)

pointer	qp			#I QPOE descriptor
char	template[ARB]		#I field name template

pointer	qp_ofnl()

begin
	return (qp_ofnl (qp, template, true))
end


# QP_OFNLU -- Open an unsorted field name list.

pointer procedure qp_ofnlu (qp, template)

pointer	qp			#I QPOE descriptor
char	template[ARB]		#I field name template

pointer	qp_ofnl()

begin
	return (qp_ofnl (qp, template, false))
end


# QP_OFNL -- Open a sorted or unsorted field name list.

pointer procedure qp_ofnl (qp, template, sort)

pointer	qp			#I QPOE descriptor
char	template[ARB]		#I field name template
bool	sort			#I sort list of matched names?

pointer	sp, patbuf, pattern, sym, fl, st, offv, sbuf, ip, op
int	len_offv, sz_sbuf, nsyms, nc, junk, nchars, i, nmatch

pointer	sthead(), stnext(), stname()
int	patmake(), patmatch(), strlen()
define	swap {junk=$1;$1=$2;$2=junk}
errchk	calloc, malloc, realloc

begin
	call smark (sp)
	call salloc (pattern, SZ_LINE, TY_CHAR)
	call salloc (patbuf, SZ_LINE, TY_CHAR)

	# Allocate the list descriptor.
	call calloc (fl, LEN_FL, TY_STRUCT)
	call malloc (offv, DEF_LENOFFV, TY_INT)
	call malloc (sbuf, DEF_SZSBUF, TY_CHAR)

	len_offv = DEF_LENOFFV
	sz_sbuf = DEF_SZSBUF
	st = QP_ST(qp)
	nsyms = 0
	nc = 0

	# Default to match all; map '*' into '?*', which is probably what
	# the user intends.  Match only at the beginning of line as we want
	# to match only entire field name strings.

	if (template[1] == EOS)
	    call strcpy ("?*", Memc[pattern], SZ_LINE)
	else {
	    op = pattern
	    Memc[op] = '^'
	    op = op + 1
	    for (ip=1;  template[ip] != EOS && ip < SZ_LINE;  ip=ip+1) {
		if (template[ip] == '*')
		    if (ip == 1 || (ip > 1 && template[ip-1] != ']')) {
			Memc[op] = '?'
			op = op + 1
		    }
		Memc[op] = template[ip]
		op = op + 1
	    }
	    Memc[op] = EOS
	}

	# Compile the pattern matching template.
	junk = patmake (Memc[pattern], Memc[patbuf], SZ_LINE)

	# Scan the symbol table and generate the unsorted list.
	for (sym=sthead(st);  sym != NULL;  sym=stnext(st,sym)) {
	    if (and (S_FLAGS(sym), SF_DELETED) != 0)
		next

	    # Get the symbol name.
	    ip = stname (st, sym)
	    nchars = strlen (Memc[ip])

	    # Save in list if it matches.
	    nmatch = patmatch (Memc[ip], Memc[patbuf]) - 1
	    if (nmatch > 0 && nmatch == nchars) {
		nsyms = nsyms + 1

		# Make room in offset vector?
		if (nsyms > len_offv) {
		    len_offv = len_offv + INC_LENOFFV
		    call realloc (offv, len_offv, TY_INT)
		}

		# Make room in string buffer?
		if (nc + nchars + 1 > sz_sbuf) {
		    sz_sbuf = sz_sbuf + INC_SZSBUF
		    call realloc (sbuf, sz_sbuf, TY_CHAR)
		}

		# Add the symbol.
		Memi[offv+nsyms-1] = nc + 1
		call strcpy (Memc[ip], Memc[sbuf+nc], nchars)
		nc = nc + nchars + 1
	    }
	}

	# Sort the list if indicated, else reverse the order of the list
	# to get a time-ordered (FIFO) list.

	if (sort)
	    call strsrt (Memi[offv], Memc[sbuf], nsyms)
	else {
	    do i = 1, nsyms / 2
		swap (Memi[offv+i-1], Memi[offv+nsyms-i])
	}

	# Finish setting up the descriptor.
	FL_LEN(fl)  = nsyms
	FL_SBUF(fl) = sbuf
	FL_OFFV(fl) = offv

	call sfree (sp)
	return (fl)
end


# QP_GNFN -- Return the next element from the field name list.  The string
# length is returned as the function value, or EOF at the end of the list.

int procedure qp_gnfn (fl, outstr, maxch)

pointer	fl			#I list descriptor
char	outstr[maxch]		#O output string
int	maxch			#I max chars out

int	pos, off, nchars 
int	gstrcpy()

begin
	pos = FL_POS(fl)
	if (pos >= FL_LEN(fl))
	    return (EOF)

	off = Memi[FL_OFFV(fl) + pos]
	nchars = gstrcpy (Memc[FL_SBUF(fl)+off-1], outstr, maxch)

	FL_POS(fl) = pos + 1
	return (nchars)
end


# QP_LENFNL -- Return the length of (number of names in) the field name list.

int procedure qp_lenfnl (fl)

pointer	fl			#I list descriptor

begin
	return (FL_LEN(fl))
end


# QP_SEEKFNL -- Seek on the field name list.

procedure qp_seekfnl (fl, pos)

pointer	fl			#I list descriptor
int	pos			#I desired list element, BOF, EOF

begin
	switch (pos) {
	case BOF:
	    FL_POS(fl) = 0
	case EOF:
	    FL_POS(fl) = FL_LEN(fl)
	default:
	    FL_POS(fl) = max(0, min(FL_LEN(fl), pos - 1))
	}
end


# QP_CFNL -- Close a field name list.

procedure qp_cfnl (fl)

pointer	fl			#I list descriptor

begin
	call mfree (FL_SBUF(fl), TY_CHAR)
	call mfree (FL_OFFV(fl), TY_INT)
	call mfree (fl, TY_STRUCT)
end
