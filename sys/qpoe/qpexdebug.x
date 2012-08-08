# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<qpexset.h>
include	<mach.h>
include	"qpex.h"
include	"qpoe.h"

define	NLUTPERLINE	15
define	SZ_TEXT		4
define	SZ_FILTERBUF	32768

# QPEX_DEBUG -- Output text describing the state and contents of the QPEX
# descriptor (compiled event attribute filter).

procedure qpex_debug (ex, out, what)

pointer	ex			#I QPEX descriptor
int	out			#I output stream
int	what			#I bitflags defining what to print

char	binval[SZ_TEXT]
pointer	sp, text, label, lutp, et, lt, pb, ip
int	neterms, lutno, proglen, dest, nout, ch, i
int	qpex_getfilter()
define	lut_ 91

begin
	call smark (sp)

	neterms = 0
	for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et))
	    neterms = neterms + 1
	proglen = (EX_PBOP(ex) - EX_PB(ex)) / LEN_INSTRUCTION

	# Print summary information.
	if (and (what, QPEXD_SUMMARY) != 0) {
	    call fprintf (out,
		"QPEX_DEBUG: ex=%xX, neterms=%d, proglen=%d/%d\n")
		call pargi (ex)
		call pargi (neterms)
		call pargi (proglen)
		call pargi ((EX_PBTOP(ex) - EX_PB(ex)) / LEN_INSTRUCTION)

	    call fprintf (out, "pb=%xX, pbtop=%xX, pbop=%xX, start=%xX\n")
		call pargi (EX_PB(ex))
		call pargi (EX_PBTOP(ex))
		call pargi (EX_PBOP(ex))
		call pargi (EX_START(ex))

	    call fprintf (out, "db=%xX, dbtop=%xX, dbop=%xX, datalen=%d/%d\n")
		call pargi (EX_DB(ex))
		call pargi (EX_DBTOP(ex))
		call pargi (EX_DBOP(ex))
		call pargi (EX_DBOP(ex) - EX_DB(ex))
		call pargi (EX_DBTOP(ex) - EX_DB(ex))

	    call fprintf (out, "max_frlutlen=%d, max_rrlutlen=%d, ")
		call pargi (EX_MAXFRLUTLEN(ex))
		call pargi (EX_MAXRRLUTLEN(ex))
	    call fprintf (out, "lut_scale=%d, lut_minranges=%d\n")
		call pargi (EX_LUTSCALE(ex))
		call pargi (EX_LUTMINRANGES(ex))

	    call fprintf (out, "ethead=%xX, ettail=%xX, lthead=%xX\n")
		call pargi (EX_ETHEAD(ex))
		call pargi (EX_ETTAIL(ex))
		call pargi (EX_LTHEAD(ex))
	}

	# Regenerate and print the compiled expression.
	if (and (what, QPEXD_SHOWEXPR) != 0) {
	    call salloc (text, SZ_FILTERBUF, TY_CHAR)
	    call fprintf (out,
		"==================== expr ========================\n")
	    if (qpex_getfilter (ex, Memc[text], SZ_FILTERBUF) > 0) {
		call putline (out, Memc[text])
		call fprintf (out, "\n")
	    }
	}

	# Decode the compiled program (print assembled instructions).
	if (and (what, QPEXD_PROGRAM) != 0) {
	    pb = EX_PB(ex)

	    call salloc (label, proglen+1, TY_INT)
	    call aclri (Memi[label], proglen+1)

	    # Flag those instructions which are the destinations of branches.
	    do i = 1, proglen {
		ip = pb + (i - 1) * LEN_INSTRUCTION
		switch (OPCODE(ip)) {
		case GOTO:
		    dest = (IARG1(ip) - pb) / LEN_INSTRUCTION
		    Memi[label+dest] = YES
		case LUTXS, LUTXI, LUTXR, LUTXD:
		    if (IARG3(ip) == NULL)
			dest = i
		    else
			dest = (IARG3(ip) - pb) / LEN_INSTRUCTION
		    Memi[label+dest] = YES
		}
	    }

	    # Do the same for code segments pointed to by lookup tables.
	    for (lt=EX_LTHEAD(ex);  lt != NULL;  lt=LT_NEXT(lt)) {
		lutp  = LT_LUTP(lt)
		do i = 0, LT_NBINS(lt) - 1 {
		    dest = Mems[lutp+i]
		    if (dest > 1) {
			dest = dest / LEN_INSTRUCTION
			Memi[label+dest] = YES
		    }
		}
	    }

	    # Output the program.
	    call fprintf (out,
		"==================== program =====================\n")

	    do i = 1, proglen + 1 {
		ip = pb + (i - 1) * LEN_INSTRUCTION

		# Output instruction label if target of a branch.
		if (Memi[label+i-1] == YES) {
		    call fprintf (out, "L%d:\t")
			call pargi (i)
		} else
		    call fprintf (out, "\t")

		# Decode and output the instruction itself.
		switch (OPCODE(ip)) {
		case NOP:
		    call fprintf (out, "nop")
		case GOTO:
		    dest = (IARG1(ip) - pb) / LEN_INSTRUCTION + 1
		    call fprintf (out, "goto L%d")
			call pargi (dest)
		case XIFT:
		    call fprintf (out, "xift")
		case XIFF:
		    call fprintf (out, "xiff")
		case PASS:
		    call fprintf (out, "pass")
		case RET:
		    call fprintf (out, "ret")

		case LDSI:
		    call fprintf (out, "ldsi\t(%d)")
			call pargi (IARG1(ip))
		case LDII:
		    call fprintf (out, "ldii\t(%d)")
			call pargi (IARG1(ip))
		case LDRR:
		    call fprintf (out, "ldrr\t(%d)")
			call pargi (IARG1(ip))
		case LDRD:
		    call fprintf (out, "ldrd\t(%d)")
			call pargi (IARG1(ip))
		case LDDD:
		    call fprintf (out, "lddd\t(%d)")
			call pargi (IARG1(ip))

		case BTTI:
		    call fprintf (out, "btti\t%oB")
			call pargi (IARG1(ip))
		case EQLI:
		    call fprintf (out, "eqli\t%d")
			call pargi (IARG1(ip))
		case EQLR:
		    call fprintf (out, "eqlr\t%g")
			call pargr (RARG1(ip))
		case EQLD:
		    call fprintf (out, "eqld\t%g")
			call pargd (DARG1(ip))
		case LEQI:
		    call fprintf (out, "leqi\t%d")
			call pargi (IARG1(ip))
		case LEQR:
		    call fprintf (out, "leqr\t%g")
			call pargr (RARG1(ip))
		case LEQD:
		    call fprintf (out, "leqd\t%g")
			call pargd (DARG1(ip))
		case GEQI:
		    call fprintf (out, "geqi\t%d")
			call pargi (IARG1(ip))
		case GEQR:
		    call fprintf (out, "geqr\t%g")
			call pargr (RARG1(ip))
		case GEQD:
		    call fprintf (out, "geqd\t%g")
			call pargd (DARG1(ip))

		case RNGI:
		    call fprintf (out, "rngi\t%d, %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case RNGR:
		    call fprintf (out, "rngr\t%g, %g")
			call pargr (RARG1(ip))
			call pargr (RARG2(ip))
		case RNGD:
		    call fprintf (out, "rngd\t%g, %g")
			call pargd (DARG1(ip))
			call pargd (DARG2(ip))

		case BTTXS:
		    call fprintf (out, "bttxs\t(%d), %oB")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case BTTXI:
		    call fprintf (out, "bttxi\t(%d), %oB")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))

		case NEQXS:
		    call fprintf (out, "neqxs\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case NEQXI:
		    call fprintf (out, "neqxi\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case NEQXR:
		    call fprintf (out, "neqxr\t(%d), %g")
			call pargi (IARG1(ip))
			call pargr (RARG2(ip))
		case NEQXD:
		    call fprintf (out, "neqxd\t(%d), %g")
			call pargi (IARG1(ip))
			call pargd (DARG2(ip))

		case EQLXS:
		    call fprintf (out, "eqlxs\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case EQLXI:
		    call fprintf (out, "eqlxi\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case EQLXR:
		    call fprintf (out, "eqlxr\t(%d), %g")
			call pargi (IARG1(ip))
			call pargr (RARG2(ip))
		case EQLXD:
		    call fprintf (out, "eqlxd\t(%d), %g")
			call pargi (IARG1(ip))
			call pargd (DARG2(ip))

		case LEQXS:
		    call fprintf (out, "leqxs\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case LEQXI:
		    call fprintf (out, "leqxi\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case LEQXR:
		    call fprintf (out, "leqxr\t(%d), %g")
			call pargi (IARG1(ip))
			call pargr (RARG2(ip))
		case LEQXD:
		    call fprintf (out, "leqxd\t(%d), %g")
			call pargi (IARG1(ip))
			call pargd (DARG2(ip))

		case GEQXS:
		    call fprintf (out, "geqxs\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case GEQXI:
		    call fprintf (out, "geqxi\t(%d), %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
		case GEQXR:
		    call fprintf (out, "geqxr\t(%d), %g")
			call pargi (IARG1(ip))
			call pargr (RARG2(ip))
		case GEQXD:
		    call fprintf (out, "geqxd\t(%d), %g")
			call pargi (IARG1(ip))
			call pargd (DARG2(ip))

		case RNGXS:
		    call fprintf (out, "rngxs\t(%d), %d, %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
			call pargi (IARG3(ip))
		case RNGXI:
		    call fprintf (out, "rngxi\t(%d), %d, %d")
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
			call pargi (IARG3(ip))
		case RNGXR:
		    call fprintf (out, "rngxr\t(%d), %g, %g")
			call pargi (IARG1(ip))
			call pargr (RARG2(ip))
			call pargr (RARG3(ip))
		case RNGXD:
		    call fprintf (out, "rngxd\t(%d), %g, %g")
			call pargi (IARG1(ip))
			call pargd (DARG2(ip))
			call pargd (DARG3(ip))

		case LUTXS:
		    ch = 's'
		    goto lut_
		case LUTXI:
		    ch = 'i'
		    goto lut_
		case LUTXR:
		    ch = 'r'
		    goto lut_
		case LUTXD:
		    ch = 'd'
lut_		    call fprintf (out, "lutx%c\t(%d), %xX, L%d")
			call pargi (ch)
			call pargi (IARG1(ip))
			call pargi (IARG2(ip))
			if (IARG3(ip) != NULL)
			    call pargi ((IARG3(ip) - pb) / LEN_INSTRUCTION + 1)
			else
			    call pargi (i + 1)
		}

		call fprintf (out, "\n")
	    }
	}

	# Output expression terms list.
	if (and (what, QPEXD_ETLIST) != 0) {
	    call fprintf (out,
		"==================== eterms ======================\n")
	    if (neterms > 0) {
		call fprintf (out,
		    " N TYPE OFF  IP  LEN DEL ATTRIBUTE OP EXPR\n")
		neterms = 0
		for (et=EX_ETHEAD(ex);  et != NULL;  et=ET_NEXT(et)) {
		    neterms = neterms + 1
		    call fprintf (out,
			"%2d %4d %3d %3d %4d %3d %9.9s %2s ")
			call pargi (neterms)
			call pargi (ET_ATTTYPE(et))
			call pargi (ET_ATTOFF(et))
			call pargi ((ET_PROGPTR(et) - pb) / LEN_INSTRUCTION + 1)
			call pargi (ET_NINSTR(et))
			call pargi (ET_DELETED(et))
			call pargstr (Memc[ET_ATNAME(et)])
			call pargstr (Memc[ET_ASSIGNOP(et)])
		    call putline (out, Memc[ET_EXPRTEXT(et)])
		    call putline (out, "\n")
		}
	    }
	}

	# Output lookup table list.
	if (and (what, QPEXD_LTLIST+QPEXD_SHOWLUTS) != 0) {
	    if (EX_LTHEAD(ex) != NULL) {
		call fprintf (out,
		    "==================== lutlist =====================\n")

		# Output column labels.
		call fprintf (out,
		    " N     LT   LUTP TYPE NBINS  L R %*wZERO  SCALE\n")
		    if (LT_TYPE(EX_LTHEAD(ex)) == TY_DOUBLE)
			call pargi (NDIGITS_DP - 4)
		    else
			call pargi (NDIGITS_RP - 4)

		# Output lookup table descriptors.
		lutno = 0
		for (lt=EX_LTHEAD(ex);  lt != NULL;  lt=LT_NEXT(lt)) {
		    lutno = lutno + 1
		    call fprintf (out, "%2d %6x %6x %4d %5d  %d %d %*g  %g\n")
			call pargi (lutno)
			call pargi (lt)
			call pargi (LT_LUTP(lt))
			call pargi (LT_TYPE(lt))
			call pargi (LT_NBINS(lt))
			call pargi (LT_LEFT(lt))
			call pargi (LT_RIGHT(lt))

			switch (LT_TYPE(lt)) {
			case TY_INT:
			    call pargi (NDIGITS_RP)
			    call pargr (LT_I0(lt))
			    call pargr (LT_IS(lt))
			case TY_REAL:
			    call pargi (NDIGITS_RP)
			    call pargr (LT_R0(lt))
			    call pargr (LT_RS(lt))
			case TY_DOUBLE:
			    call pargi (NDIGITS_DP)
			    call pargd (LT_D0(lt))
			    call pargd (LT_DS(lt))
			}
		}
	    }

	    # Dump the lookup table data.
	    if (and (what, QPEXD_SHOWLUTS) != 0) {
		lutno = 0
		for (lt=EX_LTHEAD(ex);  lt != NULL;  lt=LT_NEXT(lt)) {
		    lutno = lutno + 1
		    lutp  = LT_LUTP(lt)
		    call fprintf (out,
			"================== LUT %d (%x) ==================\n")
			call pargi (lutno)
			call pargi (lutp)
		    nout = 0
		    do i = 0, LT_NBINS(lt) - 1 {
			if (i == 0 || nout >= NLUTPERLINE) {
			    if (i > 0)
				call fprintf (out, "\n")
			    call fprintf (out, "%04d")
				call pargi (i)
			    nout = 0
			}

			# Print the bin value as 0, 1, or a statement label.
			dest = Mems[lutp+i]
			if (dest <= 1) {
			    call fprintf (out, " %4d")
				call pargi (dest)
			} else {
			    call sprintf (binval, SZ_TEXT, "L%d")
				call pargi (dest / LEN_INSTRUCTION + 1)
			    call fprintf (out, " %4s")
				call pargstr (binval)
			}

			nout = nout + 1
		    }
		    if (nout > 0)
			call fprintf (out, "\n")
		}
	    }
	}

	call sfree (sp)
end
