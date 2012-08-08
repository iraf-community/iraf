include	<tbset.h>
define	HARMLESS	0.1d0
define  MAXROWS		10000

# T_TCALC -- perform arithmetic operation on columns of a table
#
# B.Simon	03-May-91	Original
# B.Simon	24-Jun-97	Long columns done in pieces
# B.Simon	16-Jul-97	Error message for string columns
# B.Simon	30-Mar-00	Allow wild cards in table names

procedure t_tcalc()

#--
pointer	table			# input/output table name
pointer	outcol			# output column
pointer	equals			# expression
pointer	colunits		# output col units
pointer	colfmt			# output col format
pointer	datatype		# output col datatype

include	"../tabvar.com"

bool	done
double	nil
pointer sp, tp, list, buffer, colptr, code
int	nrows, nbuf, coltype, exptype

string	badtype  "Invalid data type for output column"

int	tbnget(), tbpsta(), tbcigi()
pointer tbnopenp(), tbtopn(), vex_compile()

extern	tabvar

begin
	call smark (sp)
	call salloc (table, SZ_FNAME, TY_CHAR)
	call salloc (outcol, SZ_FNAME, TY_CHAR)
	call salloc (equals, SZ_FNAME, TY_CHAR)
	call salloc (datatype, SZ_FNAME, TY_CHAR)
	call salloc (colunits, SZ_FNAME, TY_CHAR)
	call salloc (colfmt, SZ_FNAME, TY_CHAR)

	list = tbnopenp ("table")
	call clgstr ("outcol", Memc[outcol], SZ_FNAME)
	call clgstr ("equals", Memc[equals], SZ_FNAME)

	code = vex_compile (Memc[equals])

	while (tbnget (list, Memc[table], SZ_FNAME) != EOF) {
	    tp = tbtopn (Memc[table], READ_WRITE, 0)
	    nrows = tbpsta (tp, TBL_NROWS)

	    call tbcfnd (tp, Memc[outcol], colptr, 1)
	    if (colptr != NULL) {
		coltype = tbcigi (colptr, TBL_COL_DATATYPE)

	    } else {
		call clgstr ("datatype", Memc[datatype], SZ_FNAME)
		call clgstr ("colunits", Memc[colunits], SZ_FNAME)
		call clgstr ("colfmt"  , Memc[colfmt], SZ_FNAME)

		switch (Memc[datatype]) {
		case 'r':
		    coltype = TY_REAL
		case 'd':
		    coltype = TY_DOUBLE
		case 's':
		    coltype = TY_SHORT
		case 'i':
		    coltype = TY_INT
		default:
		    call tbtclo (tp)
		    call error (1, badtype)
		}

		call tbbftp (Memc[colfmt], Memc[colfmt]) 
		call tbcdef (tp, colptr, Memc[outcol], Memc[colunits],
			     Memc[colfmt], coltype, 1, 1)
	    }

	    # Initialize common block used by tabvar()

	    tabptr = tp
	    firstrow = 1
	    lastrow = MAXROWS
	    nullval = HARMLESS

	    done = false
	    nil = HARMLESS

	    repeat {
		if (lastrow >= nrows) {
		    done = true
		    lastrow = nrows
		}

		nbuf = (lastrow - firstrow) + 1
		call vex_eval (code, tabvar, nil, exptype)

		switch (coltype) {
		case TY_SHORT, TY_INT, TY_LONG:
		    call malloc (buffer, nbuf, TY_INT)
		    call vex_copyi (code, INDEFI, Memi[buffer], nbuf)
		    call tbcpti (tp, colptr, Memi[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_INT)
		case TY_REAL:
		    call malloc (buffer, nbuf, TY_REAL)
		    call vex_copyr (code, INDEFR, Memr[buffer], nbuf)
		    call tbcptr (tp, colptr, Memr[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_REAL)
		case TY_DOUBLE:
		    call malloc (buffer, nbuf, TY_DOUBLE)
		    call vex_copyd (code, INDEFD, Memd[buffer], nbuf)
		    call tbcptd (tp, colptr, Memd[buffer], firstrow, lastrow)
		    call mfree (buffer, TY_DOUBLE)
		default:
		    call tbtclo (tp)
		    call error (1, badtype)
		}

		firstrow = firstrow + MAXROWS
		lastrow = lastrow + MAXROWS
	    } until (done)

	    call tbtclo(tp)
	}

	call vex_free (code)
	call sfree (sp)
end
