include	<error.h>
include	<evexpr.h>
include <ctype.h>
include	<tbset.h>

define	SZ_TABLENAME	(SZ_FNAME)	# max size of a table name
define	SZ_KEYWORDNAME	31		# max size of a keyword name


# thselect -- Print table keyword values, if the specified expression is true.
#
# Phil Hodge, 19-Jul-2000  Task created, based on hselect.
# Phil Hodge,  4-Mar-2002  Free memory allocated by evexpr.

procedure t_thselect()

pointer keywords		# template listing keywords to be processed
pointer expr			# boolean expression to be evaluated

pointer tnt
pointer sp, table
pointer tp			# pointer to table struct
int	i

pointer tbtopn()
int	tbnopenp(), tbnget()
int	strlen()
errchk	he_select

begin
	call smark (sp)
	call salloc (table,     SZ_FNAME, TY_CHAR)
	call salloc (keywords,  SZ_LINE,  TY_CHAR)
	call salloc (expr,      SZ_LINE,  TY_CHAR)

	# Get the list of table names.
	tnt = tbnopenp ("table")

	# Get the list of keyword names.
	call clgstr ("keywords", Memc[keywords], SZ_LINE)
	do i = 1, strlen (Memc[keywords]) {
	    if (Memc[keywords+i-1] == ',')
		Memc[keywords+i-1] = ' '	# replace comma with blank
	}

	# Get the boolean expression.
	call clgstr ("expr", Memc[expr], SZ_LINE)

	# Main processing loop.  A table is processed in each pass through
	# the loop.

	while (tbnget (tnt, Memc[table], SZ_FNAME) != EOF) {

	    # Open the current table.
	    iferr {
		tp = tbtopn (Memc[table], READ_ONLY,  NULL)
	    } then {
		call erract (EA_WARN)
		next
	    }

	    # Get the full table name (including extension if FITS).
	    call tbtnam (tp, Memc[table], SZ_FNAME)

	    call he_getopsettable (tp, Memc[table], Memc[keywords])

	    iferr {
		call hs_select (tp, Memc[table], Memc[keywords], Memc[expr])
	    } then {
		call erract (EA_WARN)
		call tbtclo (tp)
		next
	    }

	    call tbtclo (tp)
	}

	call tbnclose (tnt)
	call sfree (sp)
end

procedure hs_select (tp, table, keywords, expr)

pointer tp		# i: pointer to table struct
char table[ARB]		# i: name of current table
char keywords[ARB]	# i: blank-separated list of keyword names
char expr[ARB]		# i: boolean expression
#--
pointer sp
pointer template	# one keyword name (may include wildcard characters)
pointer value, comment
char	keyword[SZ_KEYWORD]	# current keyword name
pointer o
pointer evexpr()
int	locpr()
extern	he_getop()
pointer kw, tkw_open()
int	ip, ctowrd()
int	nkw		# number of keywords
int	k		# loop index in list of matched keywords
int	tkw_len()
bool	first		# true if first keyword (template) in keywords
errchk	evexpr

begin
	call smark (sp)
	call salloc (template, SZ_FNAME, TY_CHAR)
	call salloc (value, SZ_FNAME, TY_CHAR)
	call salloc (comment, SZ_FNAME, TY_CHAR)

	# Evaluate the boolean expression.
	o = evexpr (expr, locpr(he_getop), 0)
	if (O_TYPE(o) != TY_BOOL)
	    call error (1, "expression must be boolean")

	# Print the values of the listed keywords if the expression is true.
	if (O_VALB(o)) {

	    # Get a list of all the keywords in the header.
	    kw = tkw_open (tp)

	    # for each keyword or template in blank-separated list ...
	    ip = 1
	    first = true
	    while (ctowrd (keywords, ip, Memc[template], SZ_FNAME) > 0) {

		# Find all keywords that match the current keyword template.
		call tkw_find (tp, kw, Memc[template])
		nkw = tkw_len (kw)

		# Get and print the keyword values.
		do k = 1, nkw {
		    call he_gval (tp, kw, k,
				keyword, Memc[value], Memc[comment], SZ_FNAME)
		    if (!first)
			call printf ("\t")
		    call printf ("%s")
			call he_pargstr (Memc[value])
		    first = false
		}
	    }
	    call printf ("\n")
	    call flush (STDOUT)
	    call tkw_close (kw)
	}

	call xev_freeop (o)
	call mfree (o, TY_STRUCT)
	call sfree (sp)
end
