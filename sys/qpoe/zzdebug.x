# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<mach.h>
include	<ctype.h>
include	<imhdr.h>
include	<qpexset.h>
include	<qpioset.h>
include	<qpset.h>
include	<gset.h>
include	"qpoe.h"
include	"qpex.h"
include	"qpio.h"

# ZZDEBUG -- Debug routines for the QPOE package.

task	parsei		= t_parsei,	# parse integer range list
	parser		= t_parser,	# parse floating range list
	qpparse		= t_qpparse,	# test qp_parse
	tokens		= t_tokens,	# test get token, macro replacement
	comp		= t_comp,	# test QPEX compile
	expand		= t_expand,	# perform macro expansion on text
	recio		= t_recio,	# test general record i/o
	newcopy		= t_newcopy,	# test inheritance
	syms		= t_syms,	# dump symbol table
	hlist		= t_hlist,	# list file header
	dumpevl		= t_dumpevl,	# dump event list descriptor
	mkpoe		= t_mkpoe,	# convert CFA poefile to QPOE poefile
	testpoe		= t_testpoe,	# make a test QPOE file
	countpoe	= t_countpoe,	# count photons in regions
	tfilter		= t_tfilter,	# verify time filtering
	plotpoe		= t_plotpoe,	# read and plot photons
	sum		= t_sum,	# sum counts in an image section
	setwcs		= t_setwcs,	# store a wcs in a qpoe file
	setmask		= t_setmask,	# set the default mask
	mergei		= t_mergei,	# test merging of range lists
	clear		= t_clear	# clear the screen

define	SZ_EXPR		256
define	SZ_OBUF		128000
define	SZ_TBUF		8
define	MAX_EVENTS	8192
define	SZ_RLBUF	512


# PARSEI -- Test integer range list decoding and expression optimization.

procedure t_parsei()

pointer	xs, xe
char	lbuf[SZ_LINE], left[SZ_TBUF], right[SZ_TBUF]
int	nranges, xlen, i1, i2, i

int	getline(), qpex_parsei()
bool	streq()

begin
	xlen = 0

	repeat {
	    # Get next expression.
	    call printf ("parse> ")
	    call flush (STDOUT)
	    if (getline (STDIN, lbuf) == EOF)
		break
	    else if (streq (lbuf, "bye\n"))
		break

	    # Parse the expression.
	    nranges = qpex_parsei (lbuf, xs, xe, xlen)

	    # List the ranges.
	    do i = 1, nranges {
		i1 = Memi[xs+i-1]
		i2 = Memi[xe+i-1]

		if (IS_LEFTI(i1))
		    call strcpy ("LEFT", left, SZ_TBUF)
		else {
		    call sprintf (left, SZ_TBUF, "%d")
			call pargi (i1)
		}

		if (IS_RIGHTI(i2))
		    call strcpy ("RIGHT", right, SZ_TBUF)
		else {
		    call sprintf (right, SZ_TBUF, "%d")
			call pargi (i2)
		}

		call printf ("%2d: %8s %8s\n")
		    call pargi (i)
		    call pargstr (left)
		    call pargstr (right)
	    }

	    call flush (STDOUT)
	}

	call mfree (xs, TY_INT)
	call mfree (xe, TY_INT)
end


# PARSER -- Test real range list decoding and expression optimization.

procedure t_parser()

pointer	xs, xe
char	lbuf[SZ_LINE]
int	nranges, xlen, i

int	getline(), qpex_parser()
bool	streq()

begin
	xlen = 0

	repeat {
	    # Get next expression.
	    call printf ("parse> ")
	    call flush (STDOUT)
	    if (getline (STDIN, lbuf) == EOF)
		break
	    else if (streq (lbuf, "bye\n"))
		break

	    # Parse the expression.
	    nranges = qpex_parser (lbuf, xs, xe, xlen)
	    do i = 1, nranges {
		call printf ("%2d: %7g %7g\n")
		    call pargi (i)
		    call pargr (Memr[xs+i-1])
		    call pargr (Memr[xe+i-1])
	    }
	    call flush (STDOUT)
	}

	call mfree (xs, TY_REAL)
	call mfree (xe, TY_REAL)
end


# QPPARSE -- Test qp_parse.

procedure t_qpparse()

char	expr[SZ_LINE]
char	root[SZ_FNAME]
char	filter[SZ_FNAME]

begin
	call clgstr ("expr", expr, SZ_LINE)
	call qp_parse (expr, root, SZ_FNAME, filter, SZ_FNAME)

	call printf ("root=`%s', filter=`%s'\n")
	    call pargstr (root)
	    call pargstr (filter)
end


# TOKENS -- Translate an input character stream into a stream of QPOE tokens.
# Macro replacement is performed on the input text.

procedure t_tokens()

char	input[SZ_FNAME], refpoe[SZ_FNAME]
char	tokbuf[SZ_FNAME], num[SZ_FNAME]

pointer	qp, in
int	token, junk
int	qp_gettok(), gltoc()
pointer	qp_open, qp_opentext()

begin
	input[1] = '@'
	call clgstr ("input", input[2], SZ_FNAME-1)
	call clgstr ("refpoe", refpoe, SZ_FNAME)

	if (refpoe[1] != EOS)
	    qp = qp_open (refpoe, READ_ONLY, NULL)
	else
	    qp = NULL
	in = qp_opentext (qp, input)

	repeat {
	    token = qp_gettok (in, tokbuf, SZ_FNAME)
	    if (token != EOF) {
		call printf ("%10s: %s\n")
		switch (token) {
		case TOK_IDENTIFIER:
		    call pargstr ("IDENT")
		case TOK_NUMBER:
		    call pargstr ("NUMBER")
		case TOK_STRING:
		    call pargstr ("STRING")
		case TOK_COMMAND:
		    call pargstr ("COMMAND")
		case TOK_PLUSEQUALS:
		    call pargstr ("PLUSEQ")
		case TOK_COLONEQUALS:
		    call pargstr ("COLONEQ")
		default:
		    junk = gltoc (token, num, SZ_FNAME, 8)
		    call pargstr (num)
		}
		if (IS_PRINT(tokbuf[1]))
		    call pargstr (tokbuf)
		else
		    call pargstr ("")
	    }
	} until (token == EOF)

	call printf ("EOF\n")
	call flush (STDOUT)

	call qp_closetext (in)
	if (qp != NULL)
	    call qp_close (qp)
end


# COMP -- Compile an expression with QPEX and print out the contents of
# the resultant descriptor, including the assembler translation of the
# expression.

procedure t_comp()

int	out
pointer	qp, ex
char	text[SZ_LINE]
char	output[SZ_FNAME]

int	open()
bool	streq()
pointer	qp_open(), qpex_open()

begin
	call clgstr ("poefile", text, SZ_LINE)
	qp = qp_open (text, READ_ONLY, 0)

	call clgstr ("output", output, SZ_FNAME)
	if (output[1] != EOS)
	    out = open (output, APPEND, TEXT_FILE)
	else
	    out = NULL

	repeat {
	    call clgstr ("expr", text, SZ_LINE)
	    if (streq (text, "bye"))
		break
	    else if (text[1] != EOS) {
		ex = qpex_open (qp, text)
		call qpex_debug (ex, STDOUT, QPEXD_SHOWALL)
		if (out != NULL) {
		    call fprintf (out, "\f")
		    call qpex_debug (ex, out, QPEXD_SHOWALL)
		}
		call qpex_close (ex)
		call flush (STDOUT)
	    }
	}

	call close (out)
	call qp_close (qp)
end


# EXPAND -- Perform macro expansion on text input by the user.

procedure t_expand()

pointer	qp, sp, ip, text, obuf
int	getline(), qp_expandtext(), strncmp(), clgeti()
pointer	qp_open()

begin
	call smark (sp)
	call salloc (text, SZ_LINE, TY_CHAR)
	call salloc (obuf, SZ_OBUF, TY_CHAR)

	call clgstr ("poefile", Memc[text], SZ_LINE)
	qp = qp_open (Memc[text], READ_ONLY, 0)
	call qp_seti (qp, QPOE_DEBUGLEVEL, clgeti("debug"))

	call printf ("Q> ")
	call flush (STDOUT)

	while (getline (STDIN, Memc[text]) != EOF) {
	    for (ip=text;  IS_WHITE(Memc[ip]);  ip=ip+1)
		;
	    if (strncmp (Memc[ip], "bye", 3) == 0)
		break
	    else if (Memc[ip] != '\n') {
		call write (STDOUT, Memc[obuf],
		    qp_expandtext (qp, Memc[text], Memc[obuf], SZ_OBUF))
		call printf ("\n")
	    }

	    call printf ("Q> ")
	    call flush (STDOUT)
	}

	call qp_close (qp)
	call sfree (sp)
end


# RECIO -- Test general record i/o.

procedure t_recio()

int	i, n, nrec
pointer	sp, qp, rp, poefile, data
int	qp_read(), qp_accessf(), clgeti()
pointer	qp_open()

begin
	call smark (sp)
	call salloc (poefile, SZ_FNAME, TY_CHAR)
	call salloc (data, 4096 * 3, TY_STRUCT)

	call clgstr ("poefile", Memc[poefile], SZ_FNAME)
	qp = qp_open (Memc[poefile], READ_WRITE, 0)
	call qp_seti (qp, QPOE_DEBUGLEVEL, clgeti("debug"))
	nrec = clgeti ("nrec")

	# Initialize the data array.
	do i = 1, nrec {
	    rp = data + (i-1) * 3
	    Memr[rp] = i
	    Memi[rp+1] = i
	    Mems[P2S(rp)+4] = i*10+1
	    Mems[P2S(rp)+5] = i*10+2
	}

	if (qp_accessf (qp, "urec") == NO)
	    call qp_addf (qp, "urec", "{r,i,s,s}", 0, "User record type", 0)
	if (qp_accessf (qp, "data") == NO)
	    call qp_addf (qp, "data", "urec", nrec, "User records", 0)

	# Initialize the parameter.
	call qp_write (qp, "data", Memi[data], nrec, 1, "urec")

	call eprintf ("---------------- Full array read test:")
	call aclri (Memi[data], nrec * 3)
	n = qp_read (qp, "data", Memi[data], nrec, 1, "urec")
	call eprintf (" n=%d\n");  call pargi(n)
	do i = 1, nrec {
	    rp = data + (i-1) * 3
	    call eprintf ("%8.1f %4d %4d %4d\n")
	        call pargr (Memr[rp])
	        call pargi (Memi[rp+1])
	        call pargs (Mems[P2S(rp)+4])
	        call pargs (Mems[P2S(rp)+5])
	}

	call eprintf ("---------------- Array element read test:\n")
	call aclri (Memi[data], nrec * 3)
	do i = 1, nrec {
	    rp = data + (i-1) * 3
	    n = qp_read (qp, "data", Memi[rp], 1, i, "urec")
	    call eprintf ("%4d %8.1f %4d %4d %4d\n")
		call pargi (i)
		call pargr (Memr[rp])
		call pargi (Memi[rp+1])
		call pargs (Mems[P2S(rp)+4])
		call pargs (Mems[P2S(rp)+5])
	}

	call eprintf ("---------------- Array element read test (reversed):\n")
	call aclri (Memi[data], nrec * 3)
	do i = nrec, 1, -1 {
	    rp = data + (i-1) * 3
	    n = qp_read (qp, "data", Memi[rp], 1, i, "urec")
	    call eprintf ("%4d %8.1f %4d %4d %4d\n")
		call pargi (i)
		call pargr (Memr[rp])
		call pargi (Memi[rp+1])
		call pargs (Mems[P2S(rp)+4])
		call pargs (Mems[P2S(rp)+5])
	}

	call qp_close (qp)
	call sfree (sp)
end


# NEWCOPY -- Test inheritance occurring during a new-copy open.

procedure t_newcopy()

char	iname[SZ_FNAME]		# input name
char	oname[SZ_FNAME]		# output name

pointer iqp, oqp
pointer	qp_open()

begin
	call clgstr ("input", iname, SZ_FNAME)
	call clgstr ("output", oname, SZ_FNAME)

	iqp = qp_open (iname, READ_ONLY, NULL)
	oqp = qp_open (oname, NEW_COPY, iqp)

	call printf ("iqp=%x; oqp=%x\n")
	    call pargi (iqp)
	    call pargi (oqp)

	call qp_close (oqp)
	call qp_close (iqp)
end


# SYMS -- Dump the symbol table of a QPOE datafile.

procedure t_syms()

char	fname[SZ_FNAME]
pointer	qp, qp_open()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	qp = qp_open (fname, READ_ONLY, 0)
	call qp_dsym (qp, STDOUT)
	call qp_close (qp)
end


# HLIST -- List selected header parameters.

procedure t_hlist()

pointer	qp, list, sym
int	nelem, maxelem, flags
char	datatype[SZ_DATATYPE], comment[SZ_COMMENT]
char	fname[SZ_FNAME], param[SZ_FNAME], pattern[SZ_FNAME]
pointer	qp_open(), qp_ofnlu(), qp_ofnls(), qp_gpsym()
int	qp_queryf(), qp_gnfn()
bool	clgetb()

begin
	call clgstr ("fname", fname, SZ_FNAME)
	call clgstr ("pattern", pattern, SZ_FNAME)
	
	qp = qp_open (fname, READ_ONLY, 0)
	if (clgetb ("sort"))
	    list = qp_ofnls (qp, pattern)
	else
	    list = qp_ofnlu (qp, pattern)

	call printf ("          PARAM  DTYPE NELEM MAXEL LF OFF FLG COMMENT\n")
	while (qp_gnfn (list, param, SZ_FNAME) != EOF) {
	    nelem = qp_queryf (qp, param, datatype, maxelem, comment, flags)
	    sym = qp_gpsym (qp, param)

	    call printf ("%15s %6s %5d %5d %2d%4d %3o %s\n")
		call pargstr (param)
		call pargstr (datatype)
		call pargi (nelem)
		call pargi (maxelem)
		if (sym != NULL) {
		    call pargi (S_LFILE(sym))
		    call pargi (S_OFFSET(sym))
		    call pargi (and (flags, 777B))
		    call pargstr (comment)
		} else {
		    call pargi (0)
		    call pargi (0)
		    call pargi (0)
		    call pargstr ("[could not find symbol (macro?)]")
		}
	}

	call qp_cfnl (list)
	call qp_close (qp)
end


# DUMPEVL -- Dump an event list descriptor.

procedure t_dumpevl()

pointer	qp, io, dd, ev
char	poefile[SZ_FNAME], param[SZ_FNAME]
char	datatype[SZ_DATATYPE], comment[SZ_COMMENT]
int	offset, dtype, size, nelem, maxelem, flags, i, j
pointer	qp_open(), qpio_open(), qpio_stati(), coerce()
int	qp_queryf(), sizeof()

begin
	call clgstr ("poefile", poefile, SZ_FNAME)
	qp = qp_open (poefile, READ_ONLY, NULL)

	call clgstr ("eventlist", param, SZ_FNAME)
	if (param[1] == EOS)
	    call strcpy ("events", param, SZ_FNAME)
	io = qpio_open (qp, param, READ_ONLY)

	call printf ("%s.%s:\n")
	    call pargstr (poefile)
	    call pargstr (param)

	nelem = qp_queryf (qp, param, datatype, maxelem, comment, flags)
	call printf ("dtype=%s nelem=%d maxel=%d flg=%o comment=%s\n")
	    call pargstr (datatype)
	    call pargi (nelem)
	    call pargi (maxelem)
	    call pargi (and (flags, 777B))
	    call pargstr (comment)

	call printf ("%s=%dx%d ")
	    call pargstr ("blockfactor")
	    call pargi (qpio_stati(io, QPIO_XBLOCKFACTOR))
	    call pargi (qpio_stati(io, QPIO_YBLOCKFACTOR))
	call printf ("%s=%d ")
	    call pargstr ("bucketlen")
	    call pargi (qpio_stati(io, QPIO_BUCKETLEN))
	call printf ("%s=%d ")
	    call pargstr ("debug")
	    call pargi (qpio_stati(io, QPIO_DEBUG))
	call printf ("%s=%d ")
	    call pargstr ("evxoff")
	    call pargi (qpio_stati(io, QPIO_EVXOFF))
	call printf ("%s=%d ")
	    call pargstr ("evxtype")
	    call pargi (qpio_stati(io, QPIO_EVXTYPE))
	call printf ("%s=%d ")
	    call pargstr ("evyoff")
	    call pargi (qpio_stati(io, QPIO_EVYOFF))
	call printf ("%s=%d ")
	    call pargstr ("evytype")
	    call pargi (qpio_stati(io, QPIO_EVYTYPE))
	call printf ("\n")

	call printf ("%s=%xX ")
	    call pargstr ("ex")
	    call pargi (qpio_stati(io, QPIO_EX))
	call printf ("%s=%d ")
	    call pargstr ("noindex")
	    call pargi (qpio_stati(io, QPIO_NOINDEX))
	call printf ("%s=%d ")
	    call pargstr ("optbufsize")
	    call pargi (qpio_stati(io, QPIO_OPTBUFSIZE))
	call printf ("%s=%xX ")
	    call pargstr ("pl")
	    call pargi (qpio_stati(io, QPIO_PL))
	call printf ("%s=%d ")
	    call pargstr ("eventlen")
	    call pargi (qpio_stati(io, QPIO_EVENTLEN))
	call printf ("%s=%d ")
	    call pargstr ("fd")
	    call pargi (qpio_stati(io, QPIO_FD))
	call printf ("\n")

	call printf ("%s=%d ")
	    call pargstr ("indexlen")
	    call pargi (qpio_stati(io, QPIO_INDEXLEN))
	call printf ("%s=%d ")
	    call pargstr ("ixxoff")
	    call pargi (qpio_stati(io, QPIO_IXXOFF))
	call printf ("%s=%d ")
	    call pargstr ("ixxtype")
	    call pargi (qpio_stati(io, QPIO_IXXTYPE))
	call printf ("%s=%d ")
	    call pargstr ("ixyoff")
	    call pargi (qpio_stati(io, QPIO_IXYOFF))
	call printf ("%s=%d ")
	    call pargstr ("ixytype")
	    call pargi (qpio_stati(io, QPIO_IXYTYPE))
	call printf ("%s=%d ")
	    call pargstr ("lf")
	    call pargi (qpio_stati(io, QPIO_LF))
	call printf ("%s=%xX ")
	    call pargstr ("maskp")
	    call pargi (qpio_stati(io, QPIO_MASKP))
	call printf ("\n")

	call printf ("%s=%xX ")
	    call pargstr ("maxevp")
	    call pargi (qpio_stati(io, QPIO_MAXEVP))
	call printf ("%s=%xX ")
	    call pargstr ("minevp")
	    call pargi (qpio_stati(io, QPIO_MINEVP))
	call printf ("%s=%d ")
	    call pargstr ("ncols")
	    call pargi (qpio_stati(io, QPIO_NCOLS))
	call printf ("%s=%d ")
	    call pargstr ("nlines")
	    call pargi (qpio_stati(io, QPIO_NLINES))
	call printf ("%s=%xX ")
	    call pargstr ("paramp")
	    call pargi (qpio_stati(io, QPIO_PARAMP))
	call printf ("%s=%xX ")
	    call pargstr ("qp")
	    call pargi (qpio_stati(io, QPIO_QP))
	call printf ("\n")

	# Print domain attributes.
	dd = IO_DD(io)
	call printf ("Domain `%s': len=%d nfields=%d xfield=%d yfield=%d\n")
	    call pargstr (datatype)
	    call pargi (DD_STRUCTLEN(dd))
	    call pargi (DD_NFIELDS(dd))
	    call pargi (DD_XFIELD(dd))
	    call pargi (DD_YFIELD(dd))

	# Print min/max evl records.
	do j = 1, 2 {
	    if (j == 1) {
		call printf ("minevl: ")
		ev = qpio_stati (io, QPIO_MINEVP)
	    } else {
		call printf ("maxevl: ")
		ev = qpio_stati (io, QPIO_MAXEVP)
	    }

	    do i = 1, DD_NFIELDS(dd) {
		offset = DD_FOFFSET(dd,i)
		dtype  = DD_FTYPE(dd,i)
		size   = sizeof(dtype)

		switch (dtype) {
		case TY_SHORT:
		    call printf (" s%d=%d")
			call pargi (offset * size * SZB_CHAR)
		    call pargs (Mems[coerce(ev,TY_SHORT,dtype) + offset])
		case TY_INT:
		    call printf (" i%d=%d")
			call pargi (offset * size * SZB_CHAR)
		    call pargi (Memi[coerce(ev,TY_SHORT,dtype) + offset])
		case TY_LONG:
		    call printf (" l%d=%d")
			call pargi (offset * size * SZB_CHAR)
		    call pargl (Meml[coerce(ev,TY_SHORT,dtype) + offset])
		case TY_REAL:
		    call printf (" r%d=%0.5g")
			call pargi (offset * size * SZB_CHAR)
		    call pargr (Memr[coerce(ev,TY_SHORT,dtype) + offset])
		case TY_DOUBLE:
		    call printf (" d%d=%0.8g")
			call pargi (offset * size * SZB_CHAR)
		    call pargd (Memd[coerce(ev,TY_SHORT,dtype) + offset])
		default:
		    call printf (" type=%d")
			call pargi (dtype)
		}
	    }

	    call printf ("\n")
	}
end


# MKPOE -- Convert CFA poefile to QPOE poefile.
# -------------------------

# Size limiting defintions.
define	LEN_EVBUF	512		# size of output event buffer
define	LEN_CVBUF	1000		# max number of mask regions
define	SZ_KEY		20

# CFA Poefile definitions.
define	SZ_EVENT	SZ_OEVENT
define	SZ_IEVENT	10		# size of input event struct, chars
define	SZ_OEVENT	12		# size of output event struct, chars
define	SZ_FILEHEADER	256		# size of file header, chars

# File header fields of interest.
define	O_MISSION	1		# byte offset of "mission" field
define	T_MISSION	TY_SHORT	# datatype of mission field
define	O_INSTRUMENT	1		# byte offset of "instrument" field
define	T_INSTRUMENT	TY_SHORT	# datatype of instrument field
define	O_XDIM		129		# byte offset of Xdim field
define	T_XDIM		TY_SHORT	# datatype of Xdim field
define	O_YDIM		131		# byte offset of Ydim field
define	T_YDIM		TY_SHORT	# datatype of Ydim field
define	O_PSTART	505		# byte offset of PhotonStart field
define	T_PSTART	TY_LONG		# datatype of PhotonStart field
define	O_PSTOP		509		# byte offset of PhotonStop field
define	T_PSTOP		TY_LONG		# datatype of PhotonStop field

# Input event struct fields of interest.
define	O_X		1		# sky coordinates
define	T_X		TY_SHORT
define	O_Y		3
define	T_Y		TY_SHORT
define	O_TIME		5		# arrival time
define	T_TIME		TY_DOUBLE
define	O_PHA		13		# pulse height
define	T_PHA		TY_SHORT
define	O_PI		15		# energy
define	T_PI		TY_SHORT
define	O_DX		17		# detector coordinates
define	T_DX		TY_SHORT
define	O_DY		19
define	T_DY		TY_SHORT

# The event struct to be stored in the QPOE file.
define	EVTYPE		"event"
define	FIELDLIST	"{s:x,s:y,s,s,d,s,s}"

define	EV_X		Mems[$1]
define	EV_Y		Mems[$1+1]
define	EV_PHA		Mems[$1+2]
define	EV_PI		Mems[$1+3]
define	EV_TIME		Memd[($1+4-1)/SZ_DOUBLE+1]
define	EV_DX		Mems[$1+8]
define	EV_DY		Mems[$1+9]

# Define an event structure with short coordinates
define	S_FIELDLIST	"{s:x,s:y,s,s,d,s,s}"
define	S_SZ_EVENT	10
define	S_EV_X		Mems[$1]
define	S_EV_Y		Mems[$1+1]
define	S_EV_PHA	Mems[$1+2]
define	S_EV_PI		Mems[$1+3]
define	S_EV_TIME	Memd[($1+4-1)/SZ_DOUBLE+1]
define	S_EV_DX		Mems[$1+8]
define	S_EV_DY		Mems[$1+9]

# Define an event structure with integer coordinates
define	I_FIELDLIST	"{d,i:x,i:y,i,i,s,s}"
define	I_SZ_EVENT	14
define	I_EV_TIME	Memd[($1-1)/SZ_DOUBLE+1]
define	I_EV_X		Memi[($1+4-1)/SZ_INT+1]
define	I_EV_Y		Memi[($1+6-1)/SZ_INT+1]
define	I_EV_DX		Memi[($1+8-1)/SZ_INT+1]
define	I_EV_DY		Memi[($1+10-1)/SZ_INT+1]
define	I_EV_PHA	Mems[$1+12]
define	I_EV_PI		Mems[$1+13]

# Define an event structure with real coordinates
define	R_FIELDLIST	"{d,r:x,r:y,r,r,s,s}"
define	R_SZ_EVENT	14
define	R_EV_TIME	Memd[($1-1)/SZ_DOUBLE+1]
define	R_EV_X		Memr[($1+4-1)/SZ_REAL+1]
define	R_EV_Y		Memr[($1+6-1)/SZ_REAL+1]
define	R_EV_DX		Memr[($1+8-1)/SZ_REAL+1]
define	R_EV_DY		Memr[($1+10-1)/SZ_REAL+1]
define	R_EV_PHA	Mems[$1+12]
define	R_EV_PI		Mems[$1+13]

# Define an event structure with double coordinates
define	D_FIELDLIST	"{d,d:x,d:y,d,d,s,s}"
define	D_SZ_EVENT	22
define	D_EV_TIME	Memd[($1-1)/SZ_DOUBLE+1]
define	D_EV_X		Memd[($1+4-1)/SZ_DOUBLE+1]
define	D_EV_Y		Memd[($1+8-1)/SZ_DOUBLE+1]
define	D_EV_DX		Memd[($1+12-1)/SZ_DOUBLE+1]
define	D_EV_DY		Memd[($1+16-1)/SZ_DOUBLE+1]
define	D_EV_PHA	Mems[$1+20]
define	D_EV_PI		Mems[$1+21]


# MKPOE -- Write out a new POEFILE, taking a CFA POE file as input.
# The input file uses big-endian format for integers, and IEEE for floats.
# The input file must be sorted in order for the output file to be indexed.

procedure t_mkpoe()

char	infile[SZ_FNAME]		# input CFA-format poefile
char	outfile[SZ_FNAME]		# output QPOE-format poefile

char	key[SZ_KEY]
pointer	sp, hdr, obuf, optr, ph, ev, qp, io
int	datastart, dataend, mission, instrument, now
int	debug, nphotons, naxes, axlen[2], dmin[8], dmax[8], in, op, i

bool	clgetb()
double	mp_getd()
pointer	qp_open(), qpio_open()
int	open(), read(), mp_geti(), clgeti(), clktime()

begin
	call smark (sp)
	call salloc (hdr, SZ_FILEHEADER, TY_CHAR)
	call salloc (obuf, LEN_EVBUF * SZ_OEVENT / SZ_SHORT, TY_SHORT)
	call salloc (optr, LEN_EVBUF, TY_POINTER)
	call salloc (ph, SZ_IEVENT, TY_CHAR)

	call clgstr ("infile", infile, SZ_FNAME)
	call clgstr ("outfile", outfile, SZ_FNAME)

	# Open the input and output files.  Clobber the output file if
	# it already exists.

	in = open (infile, READ_ONLY, BINARY_FILE)
	iferr (call qp_delete (outfile))
	    ;
	qp = qp_open (outfile, NEW_FILE, NULL)

	# Set the datafile page size.
	call qp_seti (qp, QPOE_PAGESIZE, clgeti("pagesize"))

	# Set the bucket length in units of number of events.
	call qp_seti (qp, QPOE_BUCKETLEN, clgeti("bucketlen"))

	# Set the debug level.
	debug = clgeti ("debug")
	call qp_seti (qp, QPOE_DEBUGLEVEL, debug)

	# Read and decode the input file header.
	if (read (in, Memc[hdr], SZ_FILEHEADER) < SZ_FILEHEADER)
	    call error (1, "cannot read input file header")

	naxes      = 2
	axlen[1]   = mp_geti (Memc[hdr], O_XDIM, T_XDIM)
	axlen[2]   = mp_geti (Memc[hdr], O_YDIM, T_YDIM)
	datastart  = mp_geti (Memc[hdr], O_PSTART, T_PSTART)
	dataend    = mp_geti (Memc[hdr], O_PSTOP, T_PSTOP)
	nphotons   = (dataend - datastart + 1) / (SZ_IEVENT * SZB_CHAR)

	mission    = mp_geti (Memc[hdr], O_MISSION, T_MISSION)
	instrument = mp_geti (Memc[hdr], O_INSTRUMENT, T_INSTRUMENT)

	call eprintf ("xdim=%d, ydim=%d, datastart=%d, nphotons=%d\n")
	    call pargi (axlen[1])
	    call pargi (axlen[2])
	    call pargi (datastart)
	    call pargi (nphotons)

	# Setup the QPOE file header.
	call qp_addf (qp, "naxes", "i", 1, "number of image axes", 0)
	call qp_puti (qp, "naxes", naxes)
	call qp_addf (qp, "axlen", "i", 2, "length of each axis", 0)
	call qp_write (qp, "axlen", axlen, 2, 1, "i")

	now = clktime(0)
	call qp_addf (qp, "cretime", "i", 1, "image creation time", 0)
	call qp_puti (qp, "cretime", now)
	call qp_addf (qp, "modtime", "i", 1, "image modify time", 0)
	call qp_puti (qp, "modtime", now)
	call qp_addf (qp, "limtime", "i", 1, "data min/max update time", 0)
	call qp_puti (qp, "limtime", now)

	# Invent some data min/max values for now.
	do i = 1, 8 {
	    dmin[i] = 0
	    dmax[i] = 64
	}
	call qp_addf (qp, "datamin", "i", 8, "minimum pixel value", 0)
	call qp_write (qp, "datamin", dmin, 8, 1, "i")
	call qp_addf (qp, "datamax", "i", 8, "maximum pixel value", 0)
	call qp_write (qp, "datamax", dmax, 8, 1, "i")

	# Throw in a few miscellaneous params for testing purposes.
	call qp_addf (qp, "mission", "s", 1, "mission type code", 0)
	call qp_puti (qp, "mission", mission)
	call qp_addf (qp, "instrument", "s", 1, "instrument type code", 0)
	call qp_puti (qp, "instrument", instrument)

	# Define the event structure for the QPOE output file.
	call qp_addf (qp, EVTYPE, FIELDLIST, 1, "event record type", 0)

	# Copy the event (photon) list.
	call qp_addf (qp, "events", "event", 0, "main event list", 0)
	io = qpio_open (qp, "events", NEW_FILE)
	call seek (in, datastart / SZB_CHAR + 1)
	op = 0

	do i = 1, nphotons {
	    # Read next event.
	    if (read (in, Memc[ph], SZ_IEVENT) < SZ_IEVENT)
		call error (2, "photon event list truncated")

	    # Copy/transform event struct (not very efficient, but this
	    # is only debug code).

	    ev = obuf + (op * SZ_OEVENT / SZ_SHORT)
	    Memi[optr+op] = ev

	    EV_TIME(ev) = mp_getd (Memc[ph], O_TIME, T_TIME)
	    EV_X(ev)    = mp_geti (Memc[ph], O_X, T_X)
	    EV_Y(ev)    = mp_geti (Memc[ph], O_Y, T_Y)
	    EV_PHA(ev)  = mp_geti (Memc[ph], O_PHA, T_PHA)
	    EV_PI(ev)   = mp_geti (Memc[ph], O_PI, T_PI)
	    EV_DX(ev)   = mp_geti (Memc[ph], O_DX, T_DX)
	    EV_DY(ev)   = mp_geti (Memc[ph], O_DY, T_DY)

	    if (debug > 4) {
		call eprintf ("%4d %4d %4d %4d %7d %7d %g\n")
		    call pargs (EV_X(ev))
		    call pargs (EV_Y(ev))
		    call pargs (EV_DX(ev))
		    call pargs (EV_DY(ev))
		    call pargs (EV_PHA(ev))
		    call pargs (EV_PI(ev))
		    call pargd (EV_TIME(ev))
	    }

	    # Bump output pointer and flush output buffer when it fills.
	    op = op + 1
	    if (op >= LEN_EVBUF) {
		call qpio_putevents (io, Memi[optr], op)
		op = 0
	    }
	}

	# Flush any remaining buffered data.
	if (op > 0)
	    call qpio_putevents (io, Memi[optr], op)

	# Construct index.
	if (clgetb ("mkindex")) {
	    call clgstr ("key", key, SZ_KEY)
	    call qpio_mkindex (io, key)
	}

	# Clean up.
	call qpio_close (io)
	call qp_close (qp)
	call close (in)

	call sfree (sp)
end


# MP_GETI -- Get an integer field from the raw input data.

int procedure mp_geti (buf, boffset, dtype)

char	buf[ARB]		# byte-stream data buffer
int	boffset			# byte offset of desired field
int	dtype			# datatype of stored field

short	sval
int	nbytes, ival
int	sizeof()

begin
	nbytes = sizeof(dtype) * SZB_CHAR

	switch (dtype) {
	case TY_SHORT:
	    if (BYTE_SWAP2 == YES)
		call bswap2 (buf, boffset, sval, 1, nbytes)
	    else
		call bytmov (buf, boffset, sval, 1, nbytes)
	    return (sval)
	case TY_INT, TY_LONG:
	    if (BYTE_SWAP4 == YES)
		call bswap4 (buf, boffset, ival, 1, nbytes)
	    else
		call bytmov (buf, boffset, ival, 1, nbytes)
	    return (ival)
	default:
	    call error (3, "bad dtype switch in mp_geti")
	}
end


# MP_GETD -- Get a double field from the raw input data.  We assume that both
# the input and output are IEEE floating, hence all we are really doing here
# is providing for arbitrarily aligned fields, and providing type conversion.

double procedure mp_getd (buf, boffset, dtype)

char	buf[ARB]		# byte-stream data buffer
int	boffset			# byte offset of desired field
int	dtype			# datatype of stored field

double	dval
real	rval
int	nbytes, half
int	sizeof()

begin
	nbytes = sizeof(dtype) * SZB_CHAR
	half   = nbytes / 2

	switch (dtype) {
	case TY_REAL:
	    if (BYTE_SWAP4 == YES)
		call bswap4 (buf, boffset, rval, 1, nbytes)
	    else
		call bytmov (buf, boffset, rval, 1, nbytes)
	    return (rval)
	case TY_DOUBLE:
	    if (BYTE_SWAP4 == YES) {
		call bswap4 (buf, boffset, dval, 1+half, half)
		call bswap4 (buf, boffset+half, dval, 1, half)
	    } else
		call bytmov (buf, boffset, dval, 1, nbytes)
	    return (dval)
	default:
	    call error (3, "bad dtype switch in mp_getd")
	}
end


# TESTPOE -- Make a test QPOE file, generating an artificial sequence of
# events (for testing special cases).  The event list is not sorted or
# indexed.

procedure t_testpoe()

char	outfile[SZ_FNAME]		# output QPOE-format poefile

pointer	sp, ev, qp, io, evl[1]
int	nevents, naxes, axlen[2], i, datatype
pointer	qp_open(), qpio_open()
int	clgeti()
char	clgetc()

begin
	# Open the output file.
	call clgstr ("outfile", outfile, SZ_FNAME)
	iferr (call qp_delete (outfile))
	    ;
	qp = qp_open (outfile, NEW_FILE, NULL)

	naxes = 2
	axlen[1] = clgeti ("ncols")
	axlen[2] = clgeti ("nlines")
	datatype = clgetc ("datatype")

	# Setup the QPOE file header.
	call qp_addf (qp, "naxes", "i", 1, "number of image axes", 0)
	call qp_puti (qp, "naxes", naxes)
	call qp_addf (qp, "axlen", "i", 2, "length of each axis", 0)
	call qp_write (qp, "axlen", axlen, 2, 1, "i")

	# Define the event structure for the QPOE output file.
	call smark (sp)
	switch (datatype) {
	case 's':
	    call salloc (ev, S_SZ_EVENT / SZ_SHORT, TY_SHORT)
	    call qp_addf (qp, EVTYPE, S_FIELDLIST, 1, "event record type", 0)
	case 'i':
	    call salloc (ev, I_SZ_EVENT / SZ_SHORT, TY_SHORT)
	    call qp_addf (qp, EVTYPE, I_FIELDLIST, 1, "event record type", 0)
	case 'r':
	    call salloc (ev, R_SZ_EVENT / SZ_SHORT, TY_SHORT)
	    call qp_addf (qp, EVTYPE, R_FIELDLIST, 1, "event record type", 0)
	case 'd':
	    call salloc (ev, D_SZ_EVENT / SZ_SHORT, TY_SHORT)
	    call qp_addf (qp, EVTYPE, D_FIELDLIST, 1, "event record type", 0)
	}


	# Copy the event (photon) list.
	call qp_addf (qp, "events", "event", 0, "main event list", 0)
	io = qpio_open (qp, "events", NEW_FILE)

	# Generate some dummy events.
	nevents = clgeti ("nevents")
	evl[1] = ev

	# Hack this to generate different types of test files.
	do i = 1, nevents {
	    switch (datatype) {

	    case 's':
	        S_EV_TIME(ev) = 1.0D0 + double(i) / 10.0D0
	        if (mod(i,2) == 0) {
		    S_EV_X(ev) = (i - 1) * 10 + 1
		    S_EV_Y(ev) = (i - 1) * 10 + 1
	        } else {
		    S_EV_X(ev) = axlen[1] - ((i - 1) * 10 + 1)
		    S_EV_Y(ev) = axlen[2] - ((i - 1) * 10 + 1)
	        }
	        S_EV_PHA(ev)  = mod (nint(i * 11.1111), 20)
	        S_EV_PI(ev)   = i / 2
	        S_EV_DX(ev)   = nevents - i + 1
	        S_EV_DY(ev)   = nevents - i + 1

	    case 'i':
	        I_EV_TIME(ev) = 1.0D0 + double(i) / 10.0D0
	        if (mod(i,2) == 0) {
		    I_EV_X(ev) = (i - 1) * 10 + 1
		    I_EV_Y(ev) = (i - 1) * 10 + 1
	        } else {
		    I_EV_X(ev) = axlen[1] - ((i - 1) * 10 + 1)
		    I_EV_Y(ev) = axlen[2] - ((i - 1) * 10 + 1)
	        }
	        I_EV_PHA(ev)  = mod (nint(i * 11.1111), 20)
	        I_EV_PI(ev)   = i / 2
	        I_EV_DX(ev)   = nevents - i + 1
	        I_EV_DY(ev)   = nevents - i + 1

	    case 'r':
	        R_EV_TIME(ev) = 1.0D0 + double(i) / 10.0D0
	        if (mod(i,2) == 0) {
		    R_EV_X(ev) = (i - 1) * 10 + 1
		    R_EV_Y(ev) = (i - 1) * 10 + 1
	        } else {
		    R_EV_X(ev) = axlen[1] - ((i - 1) * 10 + 1)
		    R_EV_Y(ev) = axlen[2] - ((i - 1) * 10 + 1)
	        }
	        R_EV_PHA(ev)  = mod (nint(i * 11.1111), 20)
	        R_EV_PI(ev)   = i / 2
	        R_EV_DX(ev)   = nevents - i + 1
	        R_EV_DY(ev)   = nevents - i + 1

	    case 'd':
	        D_EV_TIME(ev) = 1.0D0 + double(i) / 10.0D0
	        if (mod(i,2) == 0) {
		    D_EV_X(ev) = (i - 1) * 10 + 1
		    D_EV_Y(ev) = (i - 1) * 10 + 1
	        } else {
		    D_EV_X(ev) = axlen[1] - ((i - 1) * 10 + 1)
		    D_EV_Y(ev) = axlen[2] - ((i - 1) * 10 + 1)
	        }
	        D_EV_PHA(ev)  = mod (nint(i * 11.1111), 20)
	        D_EV_PI(ev)   = i / 2
	        D_EV_DX(ev)   = nevents - i + 1
	        D_EV_DY(ev)   = nevents - i + 1

	    }

	    call qpio_putevents (io, evl, 1)
	}

	# Clean up.
	call qpio_close (io)
	call qp_close (qp)
	call sfree (sp)
end


# COUNTPOE -- Count photons in regions.  Whether or not there are any regions
# depends upon whether the user specifies a region mask, or upon whether the
# image has a default mask.   If there is no mask the entire image is counted.
# If the user specifies a filter then event attribute filtering will be
# performed as well.  Mask region values should be restricted to the range
# 0-999.

procedure t_countpoe()

bool	list_events
int	debug, nev, mval, m, i
pointer	sp, qp, poefile, evlist, evl, cv, ev, io

bool	clgetb()
pointer	qp_open(), qpio_open()
int	qpio_getevents(), clgeti()

begin
	call smark (sp)
	call salloc (poefile, SZ_FNAME, TY_CHAR)
	call salloc (evlist, SZ_EXPR, TY_CHAR)
	call salloc (evl, LEN_EVBUF, TY_POINTER)
	call salloc (cv,  LEN_CVBUF, TY_INT)

	call clgstr ("poefile", Memc[poefile], SZ_FNAME)
	qp = qp_open (Memc[poefile], READ_ONLY, NULL)

	debug = clgeti ("debug")
	call qp_seti (qp, QPOE_DEBUGLEVEL, debug)

	call clgstr ("eventlist", Memc[evlist], SZ_EXPR)
	io = qpio_open (qp, Memc[evlist], READ_ONLY)

	list_events = clgetb ("list_events")
	if (list_events)
	    call printf ("  EV    X    Y   DX   DY     PHA      PI   TIME\n")

	call aclri (Memi[cv], LEN_CVBUF)

	# Scan the event list.
	while (qpio_getevents (io, Memi[evl], mval, LEN_EVBUF, nev) != EOF) {
	    if (list_events) {
		do i = 1, nev {
		    ev = Memi[evl+i-1]
		    call printf ("%4d %4d %4d %4d %4d %7d %7d   %g\n")
			call pargi (i)
			call pargs (EV_X(ev))
			call pargs (EV_Y(ev))
			call pargs (EV_DX(ev))
			call pargs (EV_DY(ev))
			call pargs (EV_PHA(ev))
			call pargs (EV_PI(ev))
			call pargd (EV_TIME(ev))
		}
	    }

	    m = min (LEN_CVBUF, mval)
	    Memi[cv+m] = Memi[cv+m] + nev
	}

	call qpio_close (io)

	# Print the count of the number of photons in each region.
	if (list_events)
	    call printf ("\n")

	call printf ("REGION: ")
	do i = 0, LEN_CVBUF-1
	    if (Memi[cv+i] > 0) {
		call printf (" %6oB")
		    call pargi (i)
	    }
	call printf ("\n")

	call printf ("COUNTS: ")
	do i = 0, LEN_CVBUF-1
	    if (Memi[cv+i] > 0) {
		call printf (" %7d")
		    call pargi (Memi[cv+i])
	    }
	call printf ("\n")

	call qp_close (qp)
	call sfree (sp)
end


# TFILTER -- Perform a brute force time filtering operation upon an event
# list, and compare against the results of a standard QPEX time filter.
# This is used to verify the operation of the optimized QPEX time filter.

procedure t_tfilter()

bool	open_left, open_right, pass
int	nev1, nev2, totev, mval, nev, xlen, nranges, fd1, fd2, i, j
pointer	sp, poefile, filter, output, fname, evl, x1, y1, t1, x2, y2, t2
pointer	qp, io, ex, ev, xs, xe
double	t

bool	clgetb()
int	qpio_getevents(), qpex_attrld(), open()
pointer	qp_open(), qpio_open(), qpex_open()

begin
	call smark (sp)
	call salloc (poefile, SZ_FNAME, TY_CHAR)
	call salloc (output, SZ_FNAME, TY_CHAR)
	call salloc (fname, SZ_FNAME, TY_CHAR)
	call salloc (filter, SZ_EXPR, TY_CHAR)
	call salloc (evl, LEN_EVBUF, TY_POINTER)

	call salloc (x1, MAX_EVENTS, TY_SHORT)
	call salloc (y1, MAX_EVENTS, TY_SHORT)
	call salloc (t1, MAX_EVENTS, TY_DOUBLE)
	call salloc (x2, MAX_EVENTS, TY_SHORT)
	call salloc (y2, MAX_EVENTS, TY_SHORT)
	call salloc (t2, MAX_EVENTS, TY_DOUBLE)

	nev1 = 0
	nev2 = 0
	totev = 0

	call clgstr ("poefile", Memc[poefile], SZ_FNAME)
	qp = qp_open (Memc[poefile], READ_ONLY, NULL)
	io = qpio_open (qp, "", READ_ONLY)

	call clgstr ("filter", Memc[filter], SZ_EXPR)
	ex = qpex_open (qp, Memc[filter])

	call clgstr ("output", Memc[output], SZ_FNAME)

	if (clgetb ("showfilter")) {
	    call qpex_debug (ex, STDOUT, QPEXD_SHOWALL)
	    call flush (STDOUT)
	}

	# Scan the event list using the given filter.
	call printf ("scan event list using optimized filter: ")
	call flush (STDOUT)

	call qpio_seti (io, QPIO_EX, ex)
	while (qpio_getevents (io, Memi[evl], mval, LEN_EVBUF, nev) != EOF) {
	    do i = 1, nev {
		ev = Memi[evl+i-1]
		Mems[x1+nev1] = EV_X(ev)
		Mems[y1+nev1] = EV_Y(ev)
		Memd[t1+nev1] = EV_TIME(ev)
		nev1 = min (MAX_EVENTS, nev1 + 1)
	    }
	}

	call printf ("%d events\n")
	    call pargi (nev1)
	xlen = 128
	call malloc (xs, TY_DOUBLE, xlen)
	call malloc (xe, TY_DOUBLE, xlen)

	# Get the time filter as a list of ranges.
	xs = NULL;  xe = NULL;  xlen = 0
	nranges = qpex_attrld (ex, "time", xs, xe, xlen)
	if (nranges > 0) {
	    open_left = IS_INDEF(Memd[xs])
	    open_right = IS_INDEF(Memd[xe+nranges-1])
	} else {
	    open_left = false
	    open_right = false
	}

	# Scan the event list, applying a brute force time filter.
	call qpio_seti (io, QPIO_EX, NULL)
	call printf ("scan event list using brute force filter: ")
	call flush (STDOUT)

	while (qpio_getevents (io, Memi[evl], mval, LEN_EVBUF, nev) != EOF) {
	    do i = 1, nev {
		ev = Memi[evl+i-1]
		t = EV_TIME(ev)

		# Apply the time filter.
		if (open_left && open_right && nranges == 1)
		    pass = true
		else {
		    pass = false
		    do j = 1, nranges {
			if (j == 1 && open_left) {
			    if (t <= Memd[xe]) {
				pass = true
				break
			    }
			} else if (j == nranges && open_right) {
			    if (t >= Memd[xs+nranges-1]) {
				pass = true
				break
			    }
			} else if (t >= Memd[xs+j-1] && t <= Memd[xe+j-1]) {
			    pass = true
			    break
			}
		    }
		}

		if (pass) {
		    Mems[x2+nev2] = EV_X(ev)
		    Mems[y2+nev2] = EV_Y(ev)
		    Memd[t2+nev2] = EV_TIME(ev)
		    nev2 = min (MAX_EVENTS, nev2 + 1)
		}
	    }

	    totev = totev + nev
	}

	call printf ("%d events\n")
	    call pargi (nev2)
	call printf ("out of a total of %d events\n")
	    call pargi (totev)
	call flush (STDOUT)

	# Dump the two event lists if an output root filename was given.
	if (Memc[output] != EOS) {
	    call sprintf (Memc[fname], SZ_FNAME, "%s.1")
		call pargstr (Memc[output])
	    iferr (call delete (Memc[fname]))
		;
	    fd1 = open (Memc[fname], NEW_FILE, TEXT_FILE)

	    call sprintf (Memc[fname], SZ_FNAME, "%s.2")
		call pargstr (Memc[output])
	    iferr (call delete (Memc[fname]))
		;
	    fd2 = open (Memc[fname], NEW_FILE, TEXT_FILE)

	    do i = 1, max (nev1, nev2) {
		if (i <= nev1) {
		    call fprintf (fd1, "%d %d %g\n")
			call pargs (Mems[x1+i-1])
			call pargs (Mems[y1+i-1])
			call pargd (Memd[t1+i-1])
		}
		if (i <= nev2) {
		    call fprintf (fd2, "%d %d %g\n")
			call pargs (Mems[x2+i-1])
			call pargs (Mems[y2+i-1])
			call pargd (Memd[t2+i-1])
		}
	    }

	    call close (fd1)
	    call close (fd2)
	}

	# Compare the results of the two filters for equality.
	pass = true
	do i = 1, min (nev1, nev2) {
	    if (Mems[x1+i-1] != Mems[x2+i-1] || Mems[y1+i-1] != Mems[y2+i-1]) {
		call printf ("bad compare at event %d: ")
		    call pargi (i)
		call printf ("[%d,%d,%0.4f] != [%d,%d,%0.4f]\n")
		    call pargs (Mems[x1+i-1])
		    call pargs (Mems[y1+i-1])
		    call pargd (Memd[t1+i-1])
		    call pargs (Mems[x2+i-1])
		    call pargs (Mems[y2+i-1])
		    call pargd (Memd[t2+i-1])
		pass = false
		break
	    }
	}

	if (pass) {
	    call printf ("first %d events are identical\n")
		call pargi (min (nev1, nev2))
	}

	call mfree (xs, TY_DOUBLE)
	call mfree (xe, TY_DOUBLE)
	call qpex_close (ex)
	call qpio_close (io)
	call qp_close (qp)

	call sfree (sp)
end


# PLOTPOE -- Read and plot photons, showing the position of each photon
# in the image matrix, according to the current coordinate system.

procedure t_plotpoe()

int	ncols, nlines, xblock, yblock, mval, nev, i
pointer	sp, poefile, evlist, evl, xv, yv, qp, io, ev, gp
pointer	qp_open(), gopen(), qpio_open
int	clgeti(), qp_stati(), qp_geti(), qpio_getevents()

begin
	call smark (sp)
	call salloc (poefile, SZ_FNAME, TY_CHAR)
	call salloc (evlist, SZ_EXPR, TY_CHAR)
	call salloc (evl, LEN_EVBUF, TY_POINTER)
	call salloc (xv, LEN_EVBUF, TY_REAL)
	call salloc (yv, LEN_EVBUF, TY_REAL)

	call clgstr ("poefile", Memc[poefile], SZ_FNAME)
	qp = qp_open (Memc[poefile], READ_ONLY, NULL)

	call qp_seti (qp, QPOE_DEBUGLEVEL, clgeti ("debug"))
	call qp_seti (qp, QPOE_XBLOCKFACTOR, clgeti ("xblock"))
	call qp_seti (qp, QPOE_YBLOCKFACTOR, clgeti ("yblock"))

	xblock = qp_stati (qp, QPOE_XBLOCKFACTOR)
	yblock = qp_stati (qp, QPOE_YBLOCKFACTOR)
	ncols  = qp_geti (qp, "axlen[1]") / xblock
	nlines = qp_geti (qp, "axlen[2]") / yblock

	gp = gopen ("stdgraph", NEW_FILE, STDGRAPH)
	call gswind (gp, 1., real(ncols), 1., real(nlines))
	call gsetr (gp, G_ASPECT, 1.0)

	call clgstr ("eventlist", Memc[evlist], SZ_EXPR)
	io = qpio_open (qp, Memc[evlist], READ_ONLY)

	if (Memc[evlist] == EOS)
	    call glabax (gp, "events", "X", "Y")
	else
	    call glabax (gp, Memc[evlist], "X", "Y")

	# Scan the event list.
	while (qpio_getevents (io, Memi[evl], mval, LEN_EVBUF, nev) != EOF) {
	    do i = 1, nev {
		ev = Memi[evl+i-1]
		Memr[xv+i-1] = EV_X(ev) / xblock + 1.0
		Memr[yv+i-1] = EV_Y(ev) / yblock + 1.0
	    }
	    call gpmark (gp, Memr[xv], Memr[yv], nev, GM_POINT, 0.0, 0.0)
	    call gflush (gp)
	}

	call qpio_close (io)
	call gclose (gp)

	call qp_close (qp)
	call sfree (sp)
end


# SUM -- Sum the counts in an image section.

procedure t_sum()

double	sum
char	image[SZ_LINE]
int	ncols, nlines, i
pointer	im, immap(), imgl2i()
real	asumi()

begin
	call clgstr ("image", image, SZ_LINE)
	im = immap (image, READ_ONLY, 0)

	ncols  = IM_LEN(im,1)
	nlines = IM_LEN(im,2)

	call printf ("ncols=%d, nlines=%d, pixtype=%d\n")
	    call pargi (ncols)
	    call pargi (nlines)
	    call pargi (IM_PIXTYPE(im))
	call flush (STDOUT)

	sum = 0
	do i = 1, nlines
	    sum = sum + asumi (Memi[imgl2i(im,i)], ncols)

	call printf ("total pixels = %d, counts = %14.0f\n")
	    call pargi (ncols * nlines)
	    call pargd (sum)

	call imunmap (im)
end


# SETWCS -- Store a wcs in a QPOE file.

procedure t_setwcs()

pointer	qp, mw
char	text[SZ_LINE]
pointer	qp_open, mw_open

begin
	call clgstr ("poefile", text, SZ_LINE)
	qp = qp_open (text, READ_WRITE, 0)
	
	mw = mw_open (NULL, 2)
	call qp_savewcs (qp, mw)

	call mw_close (mw)
	call qp_close (qp)
end


# SETFILT -- Set the default filter in a QPOE file.

procedure t_setfilt()

pointer	qp
char	poefile[SZ_FNAME]
char	filter[SZ_LINE]
pointer	qp_open()

begin
	call clgstr ("poefile", poefile, SZ_FNAME)
	qp = qp_open (poefile, READ_WRITE, 0)

	call clgstr ("deffilt", filter, SZ_LINE)
	call qp_astr (qp, "deffilt", filter, "default filter")

	call qp_close (qp)
end


# SETMASK -- Set the default mask in a QPOE file.

procedure t_setmask()

pointer	qp
char	poefile[SZ_FNAME]
char	mask[SZ_LINE]
pointer	qp_open()

begin
	call clgstr ("poefile", poefile, SZ_FNAME)
	qp = qp_open (poefile, READ_WRITE, 0)

	call clgstr ("defmask", mask, SZ_LINE)
	call qp_astr (qp, "defmask", mask, "default mask")

	call qp_close (qp)
end


# MERGEI -- Test the merge range list routine (integer version).
# The lists may be specified either as strings, or as @file-name.

procedure t_mergei()

int	p1, p2
char	list1[SZ_LINE], list2[SZ_LINE]
pointer	sp, rl1, rl2, op, xs, xe, ys, ye, os, oe
int	fd, ch, xlen, ylen, olen, nx, ny, nout, i
int	open(), getci(), qpex_parsei(), qp_rlmergei()

begin
	call smark (sp)
	call salloc (rl1, SZ_RLBUF, TY_CHAR)
	call salloc (rl2, SZ_RLBUF, TY_CHAR)

	# Get the first range list.
	call clgstr ("list1", list1, SZ_LINE)
	if (list1[1] == '@') {
	    fd = open (list1[2], READ_ONLY, TEXT_FILE)
	    op = rl1
	    while (getci (fd, ch) != EOF) {
		if (ch == '\n')
		    ch = ' '
		Memc[op] = ch
		op = op + 1
	    }
	    Memc[op] = EOS
	} else
	    call strcpy (list1, Memc[rl1], SZ_RLBUF)

	# Get the second range list.
	call clgstr ("list2", list2, SZ_LINE)
	if (list2[1] == '@') {
	    fd = open (list2[2], READ_ONLY, TEXT_FILE)
	    op = rl2
	    while (getci (fd, ch) != EOF) {
		if (ch == '\n')
		    ch = ' '
		Memc[op] = ch
		op = op + 1
	    }
	    Memc[op] = EOS
	} else
	    call strcpy (list2, Memc[rl2], SZ_RLBUF)

	# Parse the lists.
	xlen = 100
	call malloc (xs, xlen, TY_INT)
	call malloc (xe, xlen, TY_INT)
	nx = qpex_parsei (Memc[rl1], xs, xe, xlen)

	ylen = 100
	call malloc (ys, ylen, TY_INT)
	call malloc (ye, ylen, TY_INT)
	ny = qpex_parsei (Memc[rl2], ys, ye, ylen)

	# Merge the lists.
	olen = 100
	call malloc (os, olen, TY_INT)
	call malloc (oe, olen, TY_INT)
	nout = qp_rlmergei (os,oe,olen,
	    Memi[xs],Memi[xe],nx, Memi[ys],Memi[ye],ny)

	# Print results:
	call printf ("---- list 1 -----\n")
	do i = 1, nx {
	    p1 = Memi[xs+i-1]
	    p2 = Memi[xe+i-1]
	    call printf ("%8d  %8s : %8s\n")
		call pargi (i)
		if (IS_LEFTI(p1))
		    call pargstr ("left")
		else
		    call pargi (p1)
		if (IS_RIGHTI(p2))
		    call pargstr ("right")
		else
		    call pargi (p2)
	}

	call printf ("---- list 2 -----\n")
	do i = 1, ny {
	    p1 = Memi[ys+i-1]
	    p2 = Memi[ye+i-1]
	    call printf ("%8d  %8s : %8s\n")
		call pargi (i)
		if (IS_LEFTI(p1))
		    call pargstr ("left")
		else
		    call pargi (p1)
		if (IS_RIGHTI(p2))
		    call pargstr ("right")
		else
		    call pargi (p2)
	}

	call printf ("---- merged -----\n")
	do i = 1, nout {
	    p1 = Memi[os+i-1]
	    p2 = Memi[oe+i-1]
	    call printf ("%8d  %8s : %8s\n")
		call pargi (i)
		if (IS_LEFTI(p1))
		    call pargstr ("left")
		else
		    call pargi (p1)
		if (IS_RIGHTI(p2))
		    call pargstr ("right")
		else
		    call pargi (p2)
	}

	# Free list storage.
	call mfree (xs, TY_INT);  call mfree (xe, TY_INT)
	call mfree (ys, TY_INT);  call mfree (ye, TY_INT)
	call mfree (os, TY_INT);  call mfree (oe, TY_INT)

	call sfree (sp)
end


# CLEAR -- Clear the terminal screen.

procedure t_clear()

pointer	tty
pointer	ttyodes()
errchk	ttyodes

begin
	# Clear the screen.
	tty = ttyodes ("terminal")
	call ttyclear (STDOUT, tty)
	call ttycdes (tty)
end
