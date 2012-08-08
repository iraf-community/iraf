%{
include	<ctype.h>

define	YYMAXDEPTH	32
define	YYOPLEN		1
define	yyparse		unit_parse

define	SZ_SHORTSTR	31

%L
include	"parseunits.com"

char		units[SZ_FNAME]

int		num_unstr()
pointer		mul_unstr(), div_unstr(), pow_unstr(), set_unstr()

%}

%token		Y_WRONG Y_DONE Y_LPAR Y_RPAR Y_CU Y_SQ Y_ID Y_NUM

%left		Y_DIV
%left		Y_MUL
%right		Y_POW

%%

unit	:	expr Y_DONE {
		    # Normal exit. Return pointer to units structure
		    if (debug == YES)
			call eprintf ("\n")

		    tun = Memi[$1]
		    return (OK)
		}
	|	error {
		    # Syntax error
		    if (debug == YES)
			call eprintf ("\n")

		    return (ERR)
		}
	;

expr	:	Y_LPAR expr Y_RPAR {
		    # Parenthesized expression
		    Memi[$$] = Memi[$1]
		}
	|	expr Y_MUL expr {
		    # Multiply two units expressions
		    Memi[$$] = mul_unstr (Memi[$1], Memi[$3])
		    call free_unstr (Memi[$1])
		    call free_unstr (Memi[$3])

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	expr Y_DIV expr {
		    # Divide two units expressions
		    Memi[$$] = div_unstr (Memi[$1], Memi[$3])
		    call free_unstr (Memi[$1])
		    call free_unstr (Memi[$3])

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	expr Y_POW Y_NUM {
		    # Raise expression to a power
		    Memi[$$] = pow_unstr (Memi[$1], num_unstr (Memc[Memi[$3]]))
		    call free_unstr (Memi[$1])

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	termlist {
		    # List of terms
		    Memi[$$] = Memi[$1]
		}
	;
termlist:	termlist term {
		    # Implicit multiplication
		    Memi[$$] = mul_unstr (Memi[$1], Memi[$2])
		    call free_unstr (Memi[$1])
		    call free_unstr (Memi[$2])

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	term {
		    # Simple term
		    Memi[$$] = Memi[$1]
		}
	;
term	:	Y_ID Y_POW Y_NUM {
		    # Raise units to a power
		    Memi[$$] = set_unstr (abrev, Memc[Memi[$1]], 
				    	num_unstr (Memc[Memi[$3]]))

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	Y_ID Y_NUM {
		    # Implicitly raise to a power
		    Memi[$$] = set_unstr (abrev, Memc[Memi[$1]], 
				    num_unstr (Memc[Memi[$2]]))

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	Y_CU Y_ID {
		    # Cubic prefix
		    Memi[$$] = set_unstr (abrev, Memc[Memi[$2]], 3)

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	Y_SQ Y_ID {
		    # Square prefix
		    Memi[$$] = set_unstr (abrev, Memc[Memi[$2]], 2)

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	|	Y_ID {
		    # Simple name
		    Memi[$$] = set_unstr (abrev, Memc[Memi[$1]], 1)

		    if (debug == YES) {
			call str_unstr (Memi[$$], units, SZ_FNAME)
			call eprintf ("Units are %s\n")
			call pargstr (units)
		    }
		}
	;
%%

# PARSE_UNITS -- Parse a units string into the internal format

pointer procedure parse_units (ab, units)

pointer	ab		# i: abbreviation hash table
char	units[ARB]	# i: expression to be parsed
#--
include "parseunits.com"

int	len, fd
pointer	sp

string	syntax	"Syntax error in units"

bool	yydebug
int	strlen(), stropen(), yyparse()
int	get_token()
extern	get_token

begin
	len = strlen (units) + 1
	fd = stropen (units, len, READ_ONLY)

	call smark (sp)
	call salloc (tokbuf, SZ_FNAME, TY_CHAR)

	debug = NO
	yydebug = (debug == YES)
	nxttok = 0
	abrev = ab
	tun = NULL	

	if (yyparse (fd, yydebug, get_token) == ERR)
	    call tuniterr (syntax, units)

	call close (fd)
	call sfree (sp)
	return (tun)
end

# GET_TOKEN -- Retrieve next token from units string

int procedure get_token (fd, value)

int	fd		# i: File containing expression to be lexed
pointer	value		# o: Address on parse stack to store token
#--
include	"parseunits.com"

char	ch
int	type, index, powers[4]
pointer	sp, typename, token

string	pownames  "sq,square,cu,cubic"
data	powers    / Y_SQ, Y_SQ, Y_CU, Y_CU /

bool	streq()
int	getc(), word_match()

begin
	call smark (sp)
	call salloc (typename, SZ_FNAME, TY_CHAR)

	token = tokbuf + nxttok
	Memi[value] = token

	repeat {
	    ch = getc (fd, ch)
	} until (ch != ' ' && ch != '\t')

	if (ch == EOF) {
	    type = Y_DONE
	    call strcpy ("END", Memc[typename], SZ_FNAME)

	} else if (IS_ALPHA (ch)) {
	    type = Y_ID
	    call strcpy ("IDENT", Memc[typename], SZ_FNAME)

	    while (IS_ALPHA (ch)) {
		Memc[tokbuf+nxttok] = ch
		nxttok = nxttok + 1
		ch = getc (fd, ch)
	    }
	    call ungetc (fd, ch)

	    Memc[tokbuf+nxttok] = EOS
	    index = word_match (Memc[token], pownames)

	    if (index > 0) {
		type = powers[index]
		call strcpy ("POWER", Memc[typename], SZ_FNAME)

	    } else if (streq (Memc[token], "per")) {
		type = Y_DIV
		call strcpy ("DIV", Memc[typename], SZ_FNAME)
	    }

	} else if (ch == '-' || IS_DIGIT (ch)) {
	    type = Y_NUM
	    call strcpy ("NUMBER", Memc[typename], SZ_FNAME)

	    Memc[tokbuf+nxttok] = ch
	    nxttok = nxttok + 1
	    ch = getc (fd, ch)

	    while (IS_DIGIT (ch)) {
		Memc[tokbuf+nxttok] = ch
		nxttok = nxttok + 1
		ch = getc (fd, ch)
	    }
	    call ungetc (fd, ch)

	} else {
	    Memc[tokbuf+nxttok] = ch
	    nxttok = nxttok + 1

	    switch (ch) {
	    case '*':
		ch = getc (fd, ch)
		if (ch == '*') {
		    type = Y_POW
		    call strcpy ("EXPON", Memc[typename], SZ_FNAME)

		    Memc[tokbuf+nxttok] = ch
		    nxttok = nxttok + 1
		} else {
		    type = Y_MUL
		    call strcpy ("MUL", Memc[typename], SZ_FNAME)

		    call ungetc (fd, ch)
		}

	    case '/':
		type = Y_DIV
		call strcpy ("DIV", Memc[typename], SZ_FNAME)

	    case '^':
		type = Y_POW
		call strcpy ("EXPON", Memc[typename], SZ_FNAME)

	    default:
		type = Y_WRONG
		call strcpy ("ERROR", Memc[typename], SZ_FNAME)
	    }
	}

	Memc[tokbuf+nxttok] = EOS
	nxttok = nxttok + 1

	if (debug == YES) {
	    call eprintf ("Token is %s [%s]\n")
	    if (Memc[token] == EOS)  {
		call pargstr ("EOS")
	    } else {
		call pargstr (Memc[token])
	    }
	    call pargstr (Memc[typename])
	}

	call sfree (sp)
	return (type)
end
