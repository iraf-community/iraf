# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include	<evexpr.h>
include	<lexnum.h>

task	ev	= t_ev,
	lex	= t_lex,
	eq	= t_eq,
	ne	= t_ne,
	lt	= t_lt,
	le	= t_le,
	gt	= t_gt,
	ge	= t_ge,
	cmp	= t_cmp,
	ncmp	= t_ncmp,
	mat	= t_mat,
	srch	= t_srch,
	ctowrd	= t_ctowrd


# EV -- Text EVEXPR.

procedure t_ev

char	expr[SZ_LINE]
pointer	o, evexpr()
int	clglstr()

begin
	while (clglstr ("expr", expr, SZ_LINE) != EOF) {
	    o = evexpr (expr, 0, 0)

	    switch (O_TYPE(o)) {
	    case TY_BOOL:
		call printf ("%b = %s\n")
		    call pargb (O_VALB(o))
		    call pargstr (expr)

	    case TY_CHAR:
		call printf ("%s = %s\n")
		    call pargstr (O_VALC(o))
		    call pargstr (expr)

	    case TY_INT:
		call printf ("%d = %s\n")
		    call pargi (O_VALI(o))
		    call pargstr (expr)

	    case TY_REAL:
		call printf ("%g = %s\n")
		    call pargr (O_VALR(o))
		    call pargstr (expr)

	    default:
		call error (1, "expression datatype unknown")
	    }
	} 

	call printf ("\n")
end


# LEX -- Test LEXNUM.

procedure t_lex()

int	ip, nchars, toktype
char	token[SZ_FNAME]
int	lexnum(), strlen()

begin
	repeat {
	    call clgstr ("token", token, SZ_FNAME)
	    if (strlen (token) == 0)
		break

	    ip = 1
	    toktype = lexnum (token, ip, nchars)

	    call printf ("tokchars=%d, type = %s\n")
		call pargi (nchars)

		switch (toktype) {
		case LEX_OCTAL:
		    call pargstr ("octal")
		case LEX_DECIMAL:
		    call pargstr ("decimal")
		case LEX_HEX:
		    call pargstr ("hex")
		case LEX_REAL:
		    call pargstr ("real")
		case LEX_NONNUM:
		    call pargstr ("nonnumeric")
		default:
		    call pargstr ("unknown")
		}
	}
end


# EQ -- Test string equals.

procedure t_eq()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	streq()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s == %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (streq (s1, s2))
	    call flush (STDOUT)
	}
end


# NE -- Test string not equals.

procedure t_ne()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	strne()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s != %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (strne (s1, s2))
	    call flush (STDOUT)
	}
end


# LT -- Test string less than.

procedure t_lt()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	strlt()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s < %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (strlt (s1, s2))
	    call flush (STDOUT)
	}
end


# LE -- Test string less than or equals.

procedure t_le()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	strle()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s <= %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (strle (s1, s2))
	    call flush (STDOUT)
	}
end


# GT -- Test string greater than.

procedure t_gt()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	strgt()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s > %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (strgt (s1, s2))
	    call flush (STDOUT)
	}
end


# GE -- Test string greater than or equals.

procedure t_ge()

char	s1[SZ_FNAME], s2[SZ_FNAME]
bool	strge()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("%s >= %s: %b\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargb (strge (s1, s2))
	    call flush (STDOUT)
	}
end


# CMP -- Test string compare.

procedure t_cmp()

char	s1[SZ_FNAME], s2[SZ_FNAME]
int	strcmp()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("compare %s, %s: %d\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargi (strcmp (s1, s2))
	    call flush (STDOUT)
	}
end


# NCMP -- Test counted string compare.

procedure t_ncmp()

char	s1[SZ_FNAME], s2[SZ_FNAME]
int	strncmp(), clgeti()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("s2", s2, SZ_FNAME)
	    call printf ("compare %s, %s: %d\n")
		call pargstr (s1)
		call pargstr (s2)
		call pargi (strncmp (s1, s2, clgeti("nchars")))
	    call flush (STDOUT)
	}
end


# MAT -- Test string match.

procedure t_mat()

char	s1[SZ_FNAME], pat[SZ_FNAME]
int	strmatch()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("pat", pat, SZ_FNAME)
	    call printf ("match %s, pat=%s: %d\n")
		call pargstr (s1)
		call pargstr (pat)
		call pargi (strmatch (s1, pat))
	    call flush (STDOUT)
	}
end


# SRCH -- Test string search.

procedure t_srch()

char	s1[SZ_FNAME], pat[SZ_FNAME]
int	strsearch()

begin
	repeat {
	    call clgstr ("s1", s1, SZ_FNAME)
	    call clgstr ("pat", pat, SZ_FNAME)
	    call printf ("search %s, pat=%s: %d\n")
		call pargstr (s1)
		call pargstr (pat)
		call pargi (strsearch (s1, pat))
	    call flush (STDOUT)
	}
end


# CTOWRD -- Test ctowrd.

procedure t_ctowrd()

char	buf1[SZ_LINE], buf2[SZ_LINE]
int	n, ip, ctowrd(), getline()

begin
	while (getline (STDIN, buf1) != EOF) {
	    ip = 1
	    repeat {
		buf2[1] = EOS
		n = ctowrd (buf1, ip, buf2, SZ_LINE)
		call printf ("n=%d, token=%s\n")
		    call pargi (n)
		    call pargstr (buf2)
	    } until (n <= 0)
	}
end
