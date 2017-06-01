/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License (the "License").
 * You may not use this file except in compliance with the License.
 *
 * You can obtain a copy of the license at usr/src/OPENSOLARIS.LICENSE
 * or http://www.opensolaris.org/os/licensing.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at usr/src/OPENSOLARIS.LICENSE.
 * If applicable, add the following below this CDDL HEADER, with the
 * fields enclosed by brackets "[]" replaced with your own identifying
 * information: Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 */
/*
 * Copyright 2008 Sun Microsystems, Inc.  All rights reserved.
 * Use is subject to license terms.
 */

/* Copyright (c) 1988 AT&T */
/* All Rights Reserved */

//#pragma ident	"%Z%%M%	%I%	%E% SMI"

#include "dextern.h"
#include <stdio.h>

char *os_getenv (char *envvar);

#define	IDENTIFIER 257

#define	MARK 258
#define	TERM 259
#define	LEFT 260
#define	RIGHT 261
#define	BINARY 262
#define	PREC 263
#define	LCURLY 264
#define	C_IDENTIFIER 265	/* name followed by colon */
#define	NUMBER 266
#define	START 267
#define	TYPEDEF 268
#define	TYPENAME 269
#define	UNION 270
#define	ENDFILE 0
#define	LHS_TEXT_LEN		80	/* length of lhstext */
#define	RHS_TEXT_LEN		640	/* length of rhstext */
	/* communication variables between various I/O routines */

#define	v_FLAG	0x01
#define	d_FLAG	0x02
#define	DEFAULT_PREFIX	"y"

char *infile;			/* input file name              */
static int numbval;		/* value of an input number     */
static int toksize = NAMESIZE;
static char *tokname;		/* input token name             */
char *parser;			/* location of common parser    */

static void finact (void);
static char *cstash (char *);
static void defout (void);
static void cpyunion (void);
static void cpycode (void);
static void cpyact (int);
static void lhsfill (char *);
static void rhsfill (char *);
static void lrprnt (void);
#ifdef XYACC_DEBUG
static void beg_debug (void);
static void end_toks (void);
static void end_debug (void);
#endif
static void exp_tokname (void);
static void exp_prod (void);
static void exp_ntok (void);
static void exp_nonterm (void);
static int defin (int, char *);
static int gettok (void);
static int chfind (int, char *);
static int skipcom (void);
static int findchtok (int);
#ifdef PREFIX_DEFINE
static void put_prefix_define (char *);
#endif


/* storage of names */

/*
 * initial block to place token and
 * nonterminal names are stored
 * points to initial block - more space
 * is allocated as needed.
 */
static char cnamesblk0[CNAMSZ];
static char *cnames = cnamesblk0;

/* place where next name is to be put in */
static char *cnamp = cnamesblk0;

/* number of defined symbols output */
static int ndefout = 3;

	/* storage of types */
static int defunion = 0;	/* union of types defined? */
static int ntypes = 0;		/* number of types defined */
static char *typeset[NTYPES];	/* pointers to type tags */

	/* symbol tables for tokens and nonterminals */

int ntokens = 0;
int ntoksz = NTERMS;
TOKSYMB *tokset;
int *toklev;

int nnonter = -1;
NTSYMB *nontrst;
int nnontersz = NNONTERM;

static int start;		/* start symbol */

	/* assigned token type values */
static int extval = 0;

	/* input and output file descriptors */

FILE *finput;			/* yacc input file */
FILE *faction;			/* file for saving actions */
FILE *fdefine;			/* file for # defines */
FILE *ftable;			/* y.tab.x file */
FILE *ftemp;			/* tempfile to pass 2 */
FILE *fudecl;			/* file for user declarations   */
FILE *fsppout;			/* SPP y.tab.x output file      */
FILE *fdebug;			/* where the strings for debugging are stored */
FILE *foutput;			/* y.output file */

	/* output string */

static char *lhstext;
static char *rhstext;

	/* storage for grammar rules */

int *mem0;			/* production storage */
int *mem;
int *tracemem;
extern int *optimmem;
int new_memsize = MEMSIZE;
int nprod = 1;			/* number of productions */
int nprodsz = NPROD;

int **prdptr;
int *levprd;
char *had_act;

/* flag for generating the # line's default is yes */
int gen_lines = 1;
int act_lines = 0;

/* flag for whether to include runtime debugging */
static int gen_testing = 0;

/* flag for version stamping--default turned off */
static char *v_stmp = "n";

int nmbchars = 0;		/* number of mb literals in mbchars */
MBCLIT *mbchars = (MBCLIT *) 0;	/* array of mb literals */
int nmbcharsz = 0;		/* allocated space for mbchars */

void
setup (argc, argv)
     int argc;
     char *argv[];
{
    int ii, i, j, lev, t, ty;
    /* ty is the sequencial number of token name in tokset */
    int c;
    int *p;
    char *cp;
    char actname[8];
    unsigned int options = 0;
    char *file_prefix = DEFAULT_PREFIX;
    char *sym_prefix = "";
#define	F_NAME_LENGTH	128
    char fname[F_NAME_LENGTH + 1];

    foutput = NULL;
    fdefine = NULL;
    i = 1;

    parser = strdup (os_getenv ("iraf"));
    parser = realloc (parser, strlen (parser) + strlen ("lib/yaccpar.x") + 1);
    strcat(parser, "lib/yaccpar.x");

    tokname = (char *) malloc (sizeof (char) * toksize);
    tokset = (TOKSYMB *) malloc (sizeof (TOKSYMB) * ntoksz);
    toklev = (int *) malloc (sizeof (int) * ntoksz);
    nontrst = (NTSYMB *) malloc (sizeof (NTSYMB) * nnontersz);
    mem0 = (int *) malloc (sizeof (int) * new_memsize);
    prdptr = (int **) malloc (sizeof (int *) * (nprodsz + 2));
    levprd = (int *) malloc (sizeof (int) * (nprodsz + 2));
    had_act = (char *) calloc ((nprodsz + 2), sizeof (char));
    lhstext = (char *) calloc (1, sizeof (char) * LHS_TEXT_LEN);
    rhstext = (char *) calloc (1, sizeof (char) * RHS_TEXT_LEN);
    aryfil (toklev, ntoksz, 0);
    aryfil (levprd, nprodsz, 0);
    for (ii = 0; ii < ntoksz; ++ii)
	tokset[ii].value = 0;
    for (ii = 0; ii < nnontersz; ++ii)
	nontrst[ii].tvalue = 0;
    aryfil (mem0, new_memsize, 0);
    mem = mem0;
    tracemem = mem0;

    while ((c = getopt (argc, argv, "vVdltp:Q:Y:P:b:")) != EOF)
	switch (c) {
	case 'v':
	    options |= v_FLAG;
	    break;
	case 'V':
	    (void) fprintf (stderr, "yacc: NOAO/IRAF v1.0\n");
	    break;
	case 'Q':
	    v_stmp = optarg;
	    if (*v_stmp != 'y' && *v_stmp != 'n')
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate -Q and [y/n].
 */
		error ("yacc: -Q should be followed by [y/n]");
	    break;
	case 'd':
	    options |= d_FLAG;
	    break;
	case 'l':
	    gen_lines = 0;	/* don't gen #lines */
	    break;
	case 't':
	    gen_testing = 1;	/* set YYDEBUG on */
	    break;
	case 'Y':
	    cp = (char *) malloc (strlen (optarg) + sizeof ("/yaccpar") + 1);
	    cp = strcpy (cp, optarg);
	    parser = strcat (cp, "/yaccpar");
	    break;
	case 'P':
	    parser = optarg;
	    break;
	case 'p':
	    if (strcmp (optarg, "yy") != 0)
		sym_prefix = optarg;
	    else
		sym_prefix = "";
	    break;
	case 'b':
	    file_prefix = optarg;
	    break;
	case '?':
	default:
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	This is a usage message. The translate should be
 *	consistent with man page translation.
 */
	    (void) fprintf (stderr,
			    "Usage: yacc [-vVdltY] [-Q(y/n)] [-b file_prefix] [-p sym_prefix]"
			    " [-P parser] file\n");
	    exit (1);
	}
    /*
     * Open y.output if -v is specified
     */
    if (options & v_FLAG) {
	(void) strncpy (fname,
			file_prefix, F_NAME_LENGTH - strlen (".output"));
	(void) strcat (fname, ".output");
	foutput = fopen (fname, "w");
	if (foutput == NULL)
	    error ("cannot open y.output");
    }

    /*
     * Open y.tab.h if -d is specified
     */
    if (options & d_FLAG) {
	(void) strncpy (fname,
			file_prefix, F_NAME_LENGTH - strlen (".tab.h"));
	(void) strcat (fname, ".tab.h");
	fdefine = fopen (fname, "w");
	if (fdefine == NULL)
	    error ("cannot open y.tab.h");
    }

    fdebug = fopen (DEBUGNAME, "w");
    if (fdebug == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate yacc.debug.
 */
	error ("cannot open yacc.debug");
    /*
     * Open ytab.x
     (void) strncpy(fname, file_prefix, F_NAME_LENGTH-strlen(".tab.x"));
     (void) strcat(fname, ".tab.x");
     ftable = fopen(fname, "w");
     if (ftable == NULL)
     error("cannot open %s", fname);
     */


    fsppout = fopen (OFILE, "w");
    if (fsppout == NULL)
	error ("cannot create output file");
    ftable = fopen (TABFILE, "w");
    if (ftable == NULL)
	error ("cannot create table file");
    fudecl = fopen (UDFILE, "w");
    if (fudecl == NULL)
	error ("cannot create user declarations file");


    ftemp = fopen (TEMPNAME, "w");
    faction = fopen (ACTNAME, "w");
    if (ftemp == NULL || faction == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	The message means: "Could not open a temporary file."
 */
	error ("cannot open temp file");

    if ((finput = fopen (infile = argv[optind], "r")) == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
	error ("cannot open input file");

    lineno = 1;
    cnamp = cnames;
    (void) defin (0, "$end");
    extval = 0400;
    (void) defin (0, "error");
    (void) defin (1, "$accept");
    mem = mem0;
    lev = 0;
    ty = 0;
    i = 0;
#ifdef XYACC_DEBUG
    beg_debug();  /* initialize fdebug file */
#endif

    /*
     * sorry -- no yacc parser here.....
     *      we must bootstrap somehow...
     */

    t = gettok ();
    if (*v_stmp == 'y')
	(void) fprintf (ftable, "#ident\t\"yacc: NOAO/IRAF v1.0\"\n");
    for (; t != MARK && t != ENDFILE;) {
	int tok_in_line;
	switch (t) {

	case ';':
	    t = gettok ();
	    break;

	case START:
	    if ((t = gettok ()) != IDENTIFIER) {
		error ("bad %%start construction");
	    }
	    start = chfind (1, tokname);
	    t = gettok ();
	    continue;

	case TYPEDEF:
	    tok_in_line = 0;
	    if ((t = gettok ()) != TYPENAME)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate %%type.
 */
		error ("bad syntax in %%type");
	    ty = numbval;
	    for (;;) {
		t = gettok ();
		switch (t) {

		case IDENTIFIER:
		    /*
		     * The following lines are idented to left.
		     */
		    tok_in_line = 1;
		    if ((t = chfind (1, tokname)) < NTBASE) {
			j = TYPE (toklev[t]);
			if (j != 0 && j != ty) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
			    error
				("type redeclaration of token %s",
				 tokset[t].name);
			} else
			    SETTYPE (toklev[t], ty);
		    } else {
			j = nontrst[t - NTBASE].tvalue;
			if (j != 0 && j != ty) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Check how nonterminal is translated in translated
 *	yacc man page or yacc user's document.
 */
			    error
				("type redeclaration of nonterminal %s",
				 nontrst[t - NTBASE].name);
			} else
			    nontrst[t - NTBASE].tvalue = ty;
		    }
		    /* FALLTHRU */
		    /*
		     * End Indentation
		     */
		case ',':
		    continue;

		case ';':
		    t = gettok ();
		    break;
		default:
		    break;
		}
		if (!tok_in_line)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
		    error ("missing tokens or illegal tokens");
		break;
	    }
	    continue;

	case UNION:
	    /* copy the union declaration to the output */
	    cpyunion ();
	    defunion = 1;
	    t = gettok ();
	    continue;

	case LEFT:
	case BINARY:
	case RIGHT:
	    i++;
	    /* FALLTHRU */
	case TERM:
	    tok_in_line = 0;

	    /* nonzero means new prec. and assoc. */
	    lev = (t - TERM) | 04;
	    ty = 0;

	    /* get identifiers so defined */

	    t = gettok ();
	    if (t == TYPENAME) {	/* there is a type defined */
		ty = numbval;
		t = gettok ();
	    }

	    for (;;) {
		switch (t) {

		case ',':
		    t = gettok ();
		    continue;

		case ';':
		    break;

		case IDENTIFIER:
		    tok_in_line = 1;
		    j = chfind (0, tokname);
		    if (j > NTBASE) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
			error ("%s is not a token.", tokname);
		    }
		    if (lev & ~04) {
			if (ASSOC (toklev[j]) & ~04)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
			    error
				("redeclaration of precedence of %s",
				 tokname);
			SETASC (toklev[j], lev);
			SETPLEV (toklev[j], i);
		    } else {
			if (ASSOC (toklev[j]))
			    (void) warning (1,
					    "redeclaration of precedence of %s.",
					    tokname);
			SETASC (toklev[j], lev);
		    }
		    if (ty) {
			if (TYPE (toklev[j]))
			    error (
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
				      "redeclaration of type of %s", tokname);
			SETTYPE (toklev[j], ty);
		    }
		    if ((t = gettok ()) == NUMBER) {
			tokset[j].value = numbval;
			if (j < ndefout && j > 2) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
			    error
				("type number of %s should be defined earlier",
				 tokset[j].name);
			}
			if (numbval >= -YYFLAG1) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
			    error
				("token numbers must be less than %d",
				 -YYFLAG1);
			}
			t = gettok ();
		    }
		    continue;

		}
		if (!tok_in_line)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
		    error ("missing tokens or illegal tokens");
		break;
	    }
	    continue;

	case LCURLY:
	    defout ();
	    cpycode ();
	    t = gettok ();
	    continue;

	default:
	    error ("syntax error");

	}

    }

    if (t == ENDFILE) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate %%%%.
 */
	error ("unexpected EOF before %%%%");
    }

    /* t is MARK */

    defout ();
#ifdef XYACC_DEBUG
    end_toks();   /* all tokens dumped - get ready for reductions */
#endif

    fprintf (fsppout, "define\tyyclearin\tyychar = -1\n");
    fprintf (fsppout, "define\tyyerrok\t\tyyerrflag = 0\n");
    fprintf (fsppout,
	     "define\tYYMOVE\t\tcall amovi (Memi[$1], Memi[$2], YYOPLEN)\n");

    prdptr[0] = mem;
    /* added production */
    *mem++ = NTBASE;

    /* if start is 0, we will overwrite with the lhs of the first rule */
    *mem++ = start;
    *mem++ = 1;
    *mem++ = 0;
    prdptr[1] = mem;

    while ((t = gettok ()) == LCURLY)
	cpycode ();

    if (t != C_IDENTIFIER)
	error ("bad syntax on first rule");

    if (!start)
	prdptr[0][1] = chfind (1, tokname);

    /* read rules */

    while (t != MARK && t != ENDFILE) {

	/* process a rule */

	if (t == '|') {
	    rhsfill ((char *) 0);	/* restart fill of rhs */
	    *mem = *prdptr[nprod - 1];
	    if (++mem >= &tracemem[new_memsize])
		exp_mem (1);
	} else if (t == C_IDENTIFIER) {
	    *mem = chfind (1, tokname);
	    if (*mem < NTBASE)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Check how nonterminal is translated.
 */
		error ("illegal nonterminal in grammar rule");
	    if (++mem >= &tracemem[new_memsize])
		exp_mem (1);
	    lhsfill (tokname);	/* new rule: restart strings */
	} else
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
	    error ("illegal rule: missing semicolon or | ?");

	/* read rule body */


	t = gettok ();
      more_rule:
	while (t == IDENTIFIER) {
	    *mem = chfind (1, tokname);
	    if (*mem < NTBASE)
		levprd[nprod] = toklev[*mem] & ~04;
	    if (++mem >= &tracemem[new_memsize])
		exp_mem (1);
	    rhsfill (tokname);	/* add to rhs string */
	    t = gettok ();
	}

	if (t == PREC) {
	    if (gettok () != IDENTIFIER)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate %%prec.
 */
		error ("illegal %%prec syntax");
	    j = chfind (2, tokname);
	    if (j >= NTBASE)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate %%prec.
 */
		error ("nonterminal %s illegal after %%prec",
		       nontrst[j - NTBASE].name);
	    levprd[nprod] = toklev[j] & ~04;
	    t = gettok ();
	}

	if (t == '=') {
	    had_act[nprod] = 1;
	    levprd[nprod] |= ACTFLAG;
	    (void) fprintf (faction, "\ncase %d:", nprod);
	    cpyact (mem - prdptr[nprod] - 1);
	    /* !SPP         (void) fprintf(faction, " break;"); */

	    if ((t = gettok ()) == IDENTIFIER) {
		/* action within rule... */

#ifdef XYACC_DEBUG
		lrprnt();             /* dump lhs, rhs */
#endif
		(void) sprintf (actname, "$$%d", nprod);
		/*
		 * make it nonterminal
		 */
		j = chfind (1, actname);

		/*
		 * the current rule will become rule
		 * number nprod+1 move the contents down,
		 * and make room for the null
		 */

		if (mem + 2 >= &tracemem[new_memsize])
		    exp_mem (1);
		for (p = mem; p >= prdptr[nprod]; --p)
		    p[2] = *p;
		mem += 2;

		/* enter null production for action */

		p = prdptr[nprod];

		*p++ = j;
		*p++ = -nprod;

		/* update the production information */

		levprd[nprod + 1] = levprd[nprod] & ~ACTFLAG;
		levprd[nprod] = ACTFLAG;

		if (++nprod >= nprodsz)
		    exp_prod ();
		prdptr[nprod] = p;

		/*
		 * make the action appear in
		 * the original rule
		 */
		*mem++ = j;
		if (mem >= &tracemem[new_memsize])
		    exp_mem (1);
		/* get some more of the rule */
		goto more_rule;
	    }
	}
	while (t == ';')
	    t = gettok ();
	*mem++ = -nprod;
	if (mem >= &tracemem[new_memsize])
	    exp_mem (1);

	/* check that default action is reasonable */

	if (ntypes && !(levprd[nprod] & ACTFLAG) &&
	    nontrst[*prdptr[nprod] - NTBASE].tvalue) {
	    /* no explicit action, LHS has value */
	    int tempty;
	    tempty = prdptr[nprod][1];
	    if (tempty < 0)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	LHS means Left Hand Side. It does not need to be translated.
 */
		error ("must return a value, since LHS has a type");
	    else if (tempty >= NTBASE)
		tempty = nontrst[tempty - NTBASE].tvalue;
	    else
		tempty = TYPE (toklev[tempty]);
	    if (tempty != nontrst[*prdptr[nprod] - NTBASE].tvalue) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Check how action is transltated in yacc man page or documents.
 */
		error ("default action causes potential type clash");
	    }
	}

	if (++nprod >= nprodsz)
	    exp_prod ();
	prdptr[nprod] = mem;
	levprd[nprod] = 0;
    }
    /* end of all rules */

#ifdef XYACC_DEBUG
    end_debug();          /* finish fdebug file's input */
#endif
    finact ();
    if (t == MARK) {
	/*
	   if (gen_lines)
	   (void) fprintf(fsppout, "\n# a line %d \"%s\"\n",
	   lineno, infile);
	 */
	while ((c = getc (finput)) != EOF)
	    (void) putc (c, fsppout);
    }
    (void) fclose (finput);
}

static void
finact ()
{
    /* finish action routine */
    (void) fclose (faction);
    (void) fprintf (fsppout, "define\tYYERRCODE\t%d\n", tokset[2].value);
}

static char *
cstash (s)
     register char *s;
{
    char *temp;
    static int used = 0;
    static int used_save = 0;
    static int exp_cname = CNAMSZ;
    int len = strlen (s);

    /*
     * 2/29/88 -
     * Don't need to expand the table, just allocate new space.
     */
    used_save = used;
    while (len >= (exp_cname - used_save)) {
	exp_cname += CNAMSZ;
	if (!used)
	    free ((char *) cnames);
	if ((cnames = (char *) malloc (sizeof (char) * exp_cname)) == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
	    error ("cannot expand string dump");
	cnamp = cnames;
	used = 0;
    }

    temp = cnamp;
    do {
	*cnamp++ = *s;
    }
    while (*s++);
    used += cnamp - temp;
    return (temp);
}

static int
defin (int t, char *s)
{
    /* define s to be a terminal if t=0 or a nonterminal if t=1 */

    int val;

    val = 0;
    if (t) {
	if (++nnonter >= nnontersz)
	    exp_nonterm ();
	nontrst[nnonter].name = cstash (s);
	return (NTBASE + nnonter);
    }
    /* must be a token */
    if (++ntokens >= ntoksz)
	exp_ntok ();
    tokset[ntokens].name = cstash (s);

    /* establish value for token */

    if (s[0] == ' ' && s[2] == 0) {	/* single character literal */
	val = findchtok (s[1]);
    } else if (s[0] == ' ' && s[1] == '\\') {	/* escape sequence */
	if (s[3] == 0) {	/* single character escape sequence */
	    switch (s[2]) {
		/* character which is escaped */
	    case 'a':
		(void) warning (1,
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to warning() function.
 *	Do not trasnlate ANSI C, \\a.
 */
				"\\a is ANSI C \"alert\" character");
#if __STDC__ - 1 == 0
		val = '\a';
		break;
#else
		val = '\007';
		break;
#endif
	    case 'v':
		val = '\v';
		break;
	    case 'n':
		val = '\n';
		break;
	    case 'r':
		val = '\r';
		break;
	    case 'b':
		val = '\b';
		break;
	    case 't':
		val = '\t';
		break;
	    case 'f':
		val = '\f';
		break;
	    case '\'':
		val = '\'';
		break;
	    case '"':
		val = '"';
		break;
	    case '?':
		val = '?';
		break;
	    case '\\':
		val = '\\';
		break;
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
	    default:
		error ("invalid escape");
	    }
	} else if (s[2] <= '7' && s[2] >= '0') {	/* \nnn sequence */
	    int i = 3;
	    val = s[2] - '0';
	    while (isdigit (s[i]) && i <= 4) {
		if (s[i] >= '0' && s[i] <= '7')
		    val = val * 8 + s[i] - '0';
		else
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 */
		    error ("illegal octal number");
		i++;
	    }
	    if (s[i] != 0)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate \\nnn.
 */
		error ("illegal \\nnn construction");
	    if (val > 255)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate
 *		\\nnn, \\xnnnnnnnn.
 */
		error
		    ("\\nnn exceed \\377; use \\xnnnnnnnn for char value of multibyte char");
	    if (val == 0 && i >= 4)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate \\000.
 */
		error ("'\\000' is illegal");
	} else if (s[2] == 'x') {	/* hexadecimal \xnnn sequence */
	    int i = 3;
	    val = 0;
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to warning() function.
 *	Do not translate \\x, ANSI C.
 */
	    (void) warning (1, "\\x is ANSI C hex escape");
	    if (isxdigit (s[i]))
		while (isxdigit (s[i])) {
		    int tmpval;
		    if (isdigit (s[i]))
			tmpval = s[i] - '0';
		    else if (s[i] >= 'a')
			tmpval = s[i] - 'a' + 10;
		    else
			tmpval = s[i] - 'A' + 10;
		    val = 16 * val + tmpval;
		    i++;
	    } else
		error ("illegal hexadecimal number");
	    if (s[i] != 0)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate \\xnn.
 */
		error ("illegal \\xnn construction");
#define	LWCHAR_MAX	0x7fffffff
	    if ((unsigned) val > LWCHAR_MAX)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate \\xnnnnnnnn and %#x.
 */
		error (" \\xnnnnnnnn exceed %#x", LWCHAR_MAX);
	    if (val == 0)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate \\x00.
 */
		error ("'\\x00' is illegal");
	    val = findchtok (val);
	} else
	    error ("invalid escape");
    } else {
	val = extval++;
    }
    tokset[ntokens].value = val;
    toklev[ntokens] = 0;
    return (ntokens);
}

static void
defout ()
{
    /* write out the defines (at the end of the declaration section) */

    register int i, c;
    register char *cp;

    for (i = ndefout; i <= ntokens; ++i) {

	cp = tokset[i].name;
	if (*cp == ' ') {	/* literals */
	    (void) fprintf (fdebug, WSFMT ("\t\"%s\",\t%d,\n"),
			    tokset[i].name + 1, tokset[i].value);
	    continue;		/* was cp++ */
	}

	for (; (c = *cp) != 0; ++cp) {
	    if (islower (c) || isupper (c) || isdigit (c) || c == '_')
		/* EMPTY */ ;
	    else
		goto nodef;
	}

	(void) fprintf (fdebug,
			WSFMT ("\t\"%s\",\t%d,\n"), tokset[i].name,
			tokset[i].value);
	(void) fprintf (fsppout, WSFMT ("define\t%s\t\t%d\n"),
			tokset[i].name, tokset[i].value);
	if (fdefine != NULL)
	    (void) fprintf (fdefine,
			    WSFMT ("define\t%s\t\t%d\n"), tokset[i].name,
			    tokset[i].value);

      nodef:;
    }
    ndefout = ntokens + 1;
}

static int
gettok ()
{
    int i, base;
    static int peekline;	/* number of '\n' seen in lookahead */
    int c, match, reserve;
  begin:
    reserve = 0;
    lineno += peekline;
    peekline = 0;
    c = getc (finput);
    /*
     * while (c == ' ' || c == '\n' || c == '\t' || c == '\f') {
     */
    while (isspace (c)) {
	if (c == '\n')
	    ++lineno;
	c = getc (finput);
    }
    if (c == '#') {		/* skip comment */
	lineno += skipcom ();
	goto begin;
    }

    switch (c) {

    case EOF:
	return (ENDFILE);
    case '{':
	(void) ungetc (c, finput);
	return ('=');		/* action ... */
    case '<':			/* get, and look up, a type name (union member name) */
	i = 0;
	while ((c = getc (finput)) != '>' && c != EOF && c != '\n') {
	    tokname[i] = c;
	    if (++i >= toksize)
		exp_tokname ();
	}
	if (c != '>')
	    error ("unterminated < ... > clause");
	tokname[i] = 0;
	if (i == 0)
	    error ("missing type name in < ... > clause");
	for (i = 1; i <= ntypes; ++i) {
	    if (!strcmp (typeset[i], tokname)) {
		numbval = i;
		return (TYPENAME);
	    }
	}
	typeset[numbval = ++ntypes] = cstash (tokname);
	return (TYPENAME);

    case '"':
    case '\'':
	match = c;
	tokname[0] = ' ';
	i = 1;
	for (;;) {
	    c = getc (finput);
	    if (c == '\n' || c == EOF)
		error ("illegal or missing ' or \"");
	    if (c == '\\') {
		c = getc (finput);
		tokname[i] = '\\';
		if (++i >= toksize)
		    exp_tokname ();
	    } else if (c == match)
		break;
	    tokname[i] = c;
	    if (++i >= toksize)
		exp_tokname ();
	}
	break;

    case '%':
    case '\\':

	switch (c = getc (finput)) {

	case '0':
	    return (TERM);
	case '<':
	    return (LEFT);
	case '2':
	    return (BINARY);
	case '>':
	    return (RIGHT);
	case '%':
	case '\\':
	    return (MARK);
	case '=':
	    return (PREC);
	case '{':
	    return (LCURLY);
	default:
	    reserve = 1;
	}

    default:

	if (isdigit (c)) {	/* number */
	    numbval = c - '0';
	    base = (c == '0') ? 8 : 10;
	    for (c = getc (finput); isdigit (c); c = getc (finput)) {
		numbval = numbval * base + c - '0';
	    }
	    (void) ungetc (c, finput);
	    return (NUMBER);
	} else if (islower (c) || isupper (c) ||
		   c == '_' || c == '.' || c == '$') {
	    i = 0;
	    while (islower (c) || isupper (c) ||
		   isdigit (c) || c == '_' || c == '.' || c == '$') {
		tokname[i] = c;
		if (reserve && isupper (c))
		    tokname[i] = tolower (c);
		if (++i >= toksize)
		    exp_tokname ();
		c = getc (finput);
	    }
	} else
	    return (c);

	(void) ungetc (c, finput);
    }

    tokname[i] = 0;

    if (reserve) {		/* find a reserved word */
	if (!strcmp (tokname, "term"))
	    return (TERM);
	if (!strcmp (tokname, "token"))
	    return (TERM);
	if (!strcmp (tokname, "left"))
	    return (LEFT);
	if (!strcmp (tokname, "nonassoc"))
	    return (BINARY);
	if (!strcmp (tokname, "binary"))
	    return (BINARY);
	if (!strcmp (tokname, "right"))
	    return (RIGHT);
	if (!strcmp (tokname, "prec"))
	    return (PREC);
	if (!strcmp (tokname, "start"))
	    return (START);
	if (!strcmp (tokname, "type"))
	    return (TYPEDEF);
	if (!strcmp (tokname, "union"))
	    return (UNION);
	error ("invalid escape, or illegal reserved word: %s", tokname);
    }

    /* look ahead to distinguish IDENTIFIER from C_IDENTIFIER */

    c = getc (finput);
    /*
     * while (c == ' ' || c == '\t' || c == '\n' || c == '\f' || c == '/')
     * {
     */
    while (isspace (c) || c == '/') {
	if (c == '\n') {
	    ++peekline;
	} else if (c == '#') {	/* look for comments */
	    peekline += skipcom ();
	}
	c = getc (finput);
    }
    if (c == ':')
	return (C_IDENTIFIER);
    (void) ungetc (c, finput);
    return (IDENTIFIER);
}

static int
fdtype (int t)
{
    /* determine the type of a symbol */
    int v;
    if (t >= NTBASE)
	v = nontrst[t - NTBASE].tvalue;
    else
	v = TYPE (toklev[t]);
    if (v <= 0)
	error ("must specify type for %s",
	       (t >= NTBASE) ? nontrst[t - NTBASE].name : tokset[t].name);
    return (v);
}

static int
chfind (int t, char *s)
{
    int i;

    if (s[0] == ' ')
	t = 0;
    TLOOP (i) {
	if (!strcmp (s, tokset[i].name)) {
	    return (i);
	}
    }
    NTLOOP (i) {
	if (!strcmp (s, nontrst[i].name)) {
	    return (i + NTBASE);
	}
    }
    /* cannot find name */
    if (t > 1)
	error ("%s should have been defined earlier", s);
    return (defin (t, s));
}

static void
cpyunion ()
{
    /*
     * copy the union declaration to the output,
     * and the define file if present
     */
    int level, c;
    if (gen_lines)
	(void) fprintf (fsppout, "\n# line %d \"%s\"\n", lineno, infile);
    (void) fprintf (fsppout, "typedef union\n");
    if (fdefine)
	(void) fprintf (fdefine, "\ntypedef union\n");
    (void) fprintf (fsppout, "#ifdef __cplusplus\n\tYYSTYPE\n#endif\n");
    if (fdefine)
	(void) fprintf (fdefine, "#ifdef __cplusplus\n\tYYSTYPE\n#endif\n");

    level = 0;
    for (;;) {
	if ((c = getc (finput)) == EOF)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	EOF - End Of File.
 *	Do not translate %%union.
 */
	    error ("EOF encountered while processing %%union");
	(void) putc (c, fsppout);
	if (fdefine)
	    (void) putc (c, fdefine);

	switch (c) {

	case '\n':
	    ++lineno;
	    break;

	case '{':
	    ++level;
	    break;

	case '}':
	    --level;
	    if (level == 0) {	/* we are finished copying */
		(void) fprintf (fsppout, " YYSTYPE;\n");
		if (fdefine)
		    (void) fprintf (fdefine,
				    " YYSTYPE;\nextern YYSTYPE yylval;\n");
		return;
	    }
	}
    }
}

static void
cpycode ()
{
    /* copies code between \{ and \} */
    int c;
    FILE *out;


    c = getc (finput);
    if (c == '\n') {
	c = getc (finput);
	lineno++;
    }

    /* The %{ .. %} section is divided up into a global and a local region.
     * The global region is first, so set the out file to fsppout (write
     * directly into SPP output file).  The start of the local declarations
     * for the parser is marked by %L.  When this is seen, direct output
     * into the temp file fudecl, which is later inserted into the
     * declarations section of yyparse.
     */
    out = fsppout;

    if (gen_lines)
	(void) fprintf (out, "\n# line %d \"%s\"\n", lineno, infile);
    for (; c >= 0; c = getc (finput)) {
	if (c == '\\') {
	    if ((c = getc (finput)) == '}')
		return;
	    else
		putc ('\\', out);
	}
	if (c == '%') {
	    if ((c = getc (finput)) == '}') {
		return;
	    } else if (c == 'L') {
		out = fudecl;
		continue;
	    } else
		putc ('%', out);
	}
	putc (c, out);
	if (c == '\n')
	    ++lineno;
    }

    error ("eof before %%}");
}

static int
skipcom ()
{
    register int ch;

    /* skip over SPP comments */
    while ((ch = getc (finput)) != '\n')
	if (ch == EOF)
	    error ("EOF inside comment");

    return (1);
}


static void
cpyact (int offset)
{
    /* copy C action to the next ; or closing } */
    int brac, c, match, j, s, tok, argument;
    char id_name[NAMESIZE + 1];
    int id_idx = 0;

    if (gen_lines) {
	(void) fprintf (faction, "\n# line %d \"%s\"\n", lineno, infile);
	act_lines++;
    }
    brac = 0;
    id_name[0] = 0;
  loop:
    c = getc (finput);
  swt:
    switch (c) {
    case ';':
	if (brac == 0) {
	    (void) putc (c, faction);
	    return;
	}
	goto lcopy;
    case '{':
	brac++;
	goto lcopy;
    case '$':
	s = 1;
	tok = -1;
	argument = 1;
	while ((c = getc (finput)) == ' ' || c == '\t')
	    /* NULL */ ;
	if (c == '<') {		/* type description */
	    (void) ungetc (c, finput);
	    if (gettok () != TYPENAME)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate $<ident>
 */
		error ("bad syntax on $<ident> clause");
	    tok = numbval;
	    c = getc (finput);
	}
	if (c == '$') {
	    (void) fprintf (faction, "yyval");
	    if (ntypes) {	/* put out the proper tag... */
		if (tok < 0)
		    tok = fdtype (*prdptr[nprod]);
		(void) fprintf (faction, WSFMT (".%s"), typeset[tok]);
	    }
	    goto loop;
	}
	if (c == '-') {
	    s = -s;
	    c = getc (finput);
	}
	if (isdigit (c)) {
	    j = 0;
	    while (isdigit (c)) {
		j = j * 10 + c - '0';
		c = getc (finput);
	    }
	    j = j * s - offset;
	    if (j > 0) {
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate $%d.
 */
		error ("Illegal use of $%d", j + offset);
	    }

	    switch (-j) {
	    case 0:
		fprintf (faction, "yypvt");
		break;
	    case 1:
		fprintf (faction, "yypvt-YYOPLEN");
		break;
	    default:
		fprintf (faction, "yypvt-%d*YYOPLEN", -j);
	    }


	    if (ntypes) {	/* put out the proper tag */
		if (j + offset <= 0 && tok < 0)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate $%d.
 */
		    error ("must specify type of $%d", j + offset);
		if (tok < 0)
		    tok = fdtype (prdptr[nprod][j + offset]);
		(void) fprintf (faction, WSFMT (".%s"), typeset[tok]);
	    }
	    goto swt;
	}
	(void) putc ('$', faction);
	if (s < 0)
	    (void) putc ('-', faction);
	goto swt;
    case '}':
	if (--brac)
	    goto lcopy;
	(void) putc (c, faction);
	return;
    case '/':			/* look for comments */
	(void) putc (c, faction);
	c = getc (finput);
	if (c != '*')
	    goto swt;
	/* it really is a comment */
	(void) putc (c, faction);
	c = getc (finput);
	while (c != EOF) {
	    while (c == '*') {
		(void) putc (c, faction);
		if ((c = getc (finput)) == '/')
		    goto lcopy;
	    }
	    (void) putc (c, faction);
	    if (c == '\n')
		++lineno;
	    c = getc (finput);
	}
	error ("EOF inside comment");
	/* FALLTHRU */
    case '\'':			/* character constant */
    case '"':			/* character string */
	match = c;
	(void) putc (c, faction);
	while ((c = getc (finput)) != EOF) {
	    if (c == '\\') {
		(void) putc (c, faction);
		c = getc (finput);
		if (c == '\n')
		    ++lineno;
	    } else if (c == match)
		goto lcopy;
	    else if (c == '\n')
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	This error message is issued when
 *	quoted string has multiple lines.
 */
		error ("newline in string or char. const.");
	    (void) putc (c, faction);
	}
	error ("EOF in string or character constant");
	/* FALLTHRU */
    case EOF:
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Check how 'action' is translated in yacc mapage/document.
 */
	error ("action does not terminate");
	/* FALLTHRU */
    case '\n':
	++lineno;
	goto lcopy;
    }
  lcopy:
    (void) putc (c, faction);
    /*
     * Save the possible identifier name.
     * Used to print out a warning message.
     */
    if (id_idx >= NAMESIZE) {
	/*
	 * Error. Silently ignore.
	 */
	/* EMPTY */ ;
    }
    /*
     * If c has a possibility to be a
     * part of identifier, save it.
     */
    else if (isalnum (c) || c == '_') {
	id_name[id_idx++] = c;
	id_name[id_idx] = 0;
    } else {
	id_idx = 0;
	id_name[id_idx] = 0;
    }
    goto loop;
}

static void
lhsfill (s)			/* new rule, dump old (if exists), restart strings */
     char *s;
{
    static int lhs_len = LHS_TEXT_LEN;
    int s_lhs = strlen (s);
    if (s_lhs >= lhs_len) {
	lhs_len = s_lhs + 2;
	lhstext = (char *)
	    realloc ((char *) lhstext, sizeof (char) * lhs_len);
	if (lhstext == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	LHS -- Left Hand Side.
 */
	    error ("couldn't expanded LHS length");
    }
    rhsfill ((char *) 0);
    (void) strcpy (lhstext, s);	/* don't worry about too long of a name */
}

static void
rhsfill (s)
     char *s;			/* either name or 0 */
{
    static char *loc;		/* next free location in rhstext */
    static int rhs_len = RHS_TEXT_LEN;
    static int used = 0;
    int s_rhs = (s == NULL ? 0 : strlen (s));
    register char *p;

    if (!s) {			/* print out and erase old text */
	if (*lhstext)		/* there was an old rule - dump it */
	    lrprnt ();
	(loc = rhstext)[0] = 0;
	return;
    }
    /* add to stuff in rhstext */
    p = s;

    used = loc - rhstext;
    if ((s_rhs + 3) >= (rhs_len - used)) {
	static char *textbase;
	textbase = rhstext;
	rhs_len += s_rhs + RHS_TEXT_LEN;
	rhstext = (char *)
	    realloc ((char *) rhstext, sizeof (char) * rhs_len);
	if (rhstext == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	RHS -- Right Hand Side.
 */
	    error ("couldn't expanded RHS length");
	loc = loc - textbase + rhstext;
    }

    *loc++ = ' ';
    if (*s == ' ') {		/* special quoted symbol */
	*loc++ = '\'';		/* add first quote */
	p++;
    }
    while ((*loc = *p++)) {
	if (loc++ > &rhstext[RHS_TEXT_LEN] - 3)
	    break;
    }

    if (*s == ' ')
	*loc++ = '\'';
    *loc = 0;			/* terminate the string */
}

static void
lrprnt ()
{				/* print out the left and right hand sides */
    char *rhs;
    char *m_rhs = NULL;

    if (!*rhstext)		/* empty rhs - print usual comment */
	rhs = " /* empty */";
    else {
	int idx1;		/* tmp idx used to find if there are d_quotes */
	int idx2;		/* tmp idx used to generate escaped string */
	char *p;
	/*
	 * Check if there are any double quote in RHS.
	 */
	for (idx1 = 0; rhstext[idx1] != 0; idx1++) {
	    if (rhstext[idx1] == '"') {
		/*
		 * A double quote is found.
		 */
		idx2 = strlen (rhstext) * 2;
		p = m_rhs = (char *)
		    malloc ((idx2 + 1) * sizeof (char));
		if (m_rhs == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	RHS - Right Hand Side.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
		    error ("Couldn't allocate memory for RHS.");
		/*
		 * Copy string
		 */
		for (idx2 = 0; rhstext[idx2] != 0; idx2++) {
		    /*
		     * Check if this quote is escaped or not
		     */
		    if (rhstext[idx2] == '"') {
			int tmp_l = idx2 - 1;
			int cnt = 0;
			while (tmp_l >= 0 && rhstext[tmp_l] == '\\') {
			    cnt++;
			    tmp_l--;
			}
			/*
			 * If quote is not escaped,
			 * then escape it.
			 */
			if (cnt % 2 == 0)
			    *p++ = '\\';
		    }
		    *p++ = rhstext[idx2];
		}
		*p = 0;
		/*
		 * Break from the loop
		 */
		break;
	    }
	}
	if (m_rhs == NULL)
	    rhs = rhstext;
	else
	    rhs = m_rhs;
    }
    (void) fprintf (fdebug, WSFMT ("\t\"%s :%s\",\n"), lhstext, rhs);
    if (m_rhs)
	free (m_rhs);
}


#ifdef XYACC_DEBUG 

static void
beg_debug ()
{				/* dump initial sequence for fdebug file */
    (void) fprintf (fdebug, "typedef struct\n");
    (void) fprintf (fdebug, "#ifdef __cplusplus\n\tyytoktype\n");
    (void) fprintf (fdebug, "#endif\n{\n");
    (void) fprintf (fdebug, "#ifdef __cplusplus\nconst\n#endif\n");
    (void) fprintf (fdebug, "char *t_name; int t_val; } yytoktype;\n");
    (void) fprintf (fdebug,
		    "#ifndef YYDEBUG\n#\tdefine YYDEBUG\t%d", gen_testing);
    (void) fprintf (fdebug, "\t/*%sallow debugging */\n#endif\n\n",
		    gen_testing ? " " : " don't ");
    (void) fprintf (fdebug, "#if YYDEBUG\n\nyytoktype yytoks[] =\n{\n");
}


static void
end_toks ()
{				/* finish yytoks array, get ready for yyred's strings */
    (void) fprintf (fdebug, "\t\"-unknown-\",\t-1\t/* ends search */\n");
    (void) fprintf (fdebug, "};\n\n");
    (void) fprintf (fdebug, "#ifdef __cplusplus\nconst\n#endif\n");
    (void) fprintf (fdebug, "char * yyreds[] =\n{\n");
    (void) fprintf (fdebug, "\t\"-no such reduction-\",\n");
}


static void
end_debug ()
{				/* finish yyred array, close file */
    lrprnt ();			/* dump last lhs, rhs */
    (void) fprintf (fdebug, "};\n#endif /* YYDEBUG */\n");
    (void) fclose (fdebug);
}

#endif


/*
 * 2/29/88 -
 * The normal length for token sizes is NAMESIZE - If a token is
 * seen that has a longer length, expand "tokname" by NAMESIZE.
 */
static void
exp_tokname ()
{
    toksize += NAMESIZE;
    tokname = (char *) realloc ((char *) tokname, sizeof (char) * toksize);
}


/*
 * 2/29/88 -
 *
 */
static void
exp_prod ()
{
    int i;
    nprodsz += NPROD;

    prdptr =
	(int **) realloc ((char *) prdptr, sizeof (int *) * (nprodsz + 2));
    levprd = (int *) realloc ((char *) levprd, sizeof (int) * (nprodsz + 2));
    had_act = (char *)
	realloc ((char *) had_act, sizeof (char) * (nprodsz + 2));
    for (i = nprodsz - NPROD; i < nprodsz + 2; ++i)
	had_act[i] = 0;

    if ((*prdptr == NULL) || (levprd == NULL) || (had_act == NULL))
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
	error ("couldn't expand productions");
}

/*
 * 2/29/88 -
 * Expand the number of terminals.  Initially there are NTERMS;
 * each time space runs out, the size is increased by NTERMS.
 * The total size, however, cannot exceed MAXTERMS because of
 * the way LOOKSETS(struct looksets) is set up.
 * Tables affected:
 *	tokset, toklev : increased to ntoksz
 *
 *	tables with initial dimensions of TEMPSIZE must be changed if
 *	(ntoksz + NNONTERM) >= TEMPSIZE : temp1[]
 */
static void
exp_ntok ()
{
    ntoksz += NTERMS;

    tokset = (TOKSYMB *) realloc ((char *) tokset, sizeof (TOKSYMB) * ntoksz);
    toklev = (int *) realloc ((char *) toklev, sizeof (int) * ntoksz);

    if ((tokset == NULL) || (toklev == NULL))
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate NTERMS.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
	error ("couldn't expand NTERMS");
}


static void
exp_nonterm ()
{
    nnontersz += NNONTERM;

    nontrst = (NTSYMB *)
	realloc ((char *) nontrst, sizeof (TOKSYMB) * nnontersz);
    if (nontrst == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *	Do not translate NTERMS.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
	error ("couldn't expand NNONTERM");
}

void
exp_mem (flag)
     int flag;
{
    int i;
    static int *membase;
    new_memsize += MEMSIZE;

    membase = tracemem;
    tracemem = (int *)
	realloc ((char *) tracemem, sizeof (int) * new_memsize);
    if (tracemem == NULL)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	This message is passed to error() function.
 *
 *	You may just translate this as:
 *	'Could not allocate internally used memory.'
 */
	error ("couldn't expand mem table");
    if (flag) {
	for (i = 0; i <= nprod; ++i)
	    prdptr[i] = prdptr[i] - membase + tracemem;
	mem = mem - membase + tracemem;
    } else {
	size += MEMSIZE;
	temp1 = (int *) realloc ((char *) temp1, sizeof (int) * size);
	optimmem = optimmem - membase + tracemem;
    }
}

static int
findchtok (chlit)
     int chlit;
/*
 * findchtok(chlit) returns the token number for a character literal
 * chlit that is "bigger" than 255 -- the max char value that the
 * original yacc was build for.  This yacc treate them as though
 * an ordinary token.
 */
{
    int i;

    if (chlit < 0xff)
	return (chlit);		/* single-byte char */
    for (i = 0; i < nmbchars; ++i) {
	if (mbchars->character == chlit)
	    return (mbchars->tvalue);
    }

    /* Not found.  Register it! */
    if (++nmbchars > nmbcharsz) {	/* Make sure there's enough space */
	nmbcharsz += NMBCHARSZ;
	mbchars = (MBCLIT *)
	    realloc ((char *) mbchars, sizeof (MBCLIT) * nmbcharsz);
	if (mbchars == NULL)
	    error ("too many character literals");
    }
    mbchars[nmbchars - 1].character = chlit;
    return (mbchars[nmbchars - 1].tvalue = extval++);
    /* Return the newly assigned token. */
}

/*
 * When -p is specified, symbol prefix for
 *	yy{parse, lex, error}(),
 *	yy{lval, val, char, debug, errflag, nerrs}
 * are defined to the specified name.
 */
#ifdef PREFIX_DEFINE

static void
put_prefix_define (char *pre)
{
    char *syms[] = {
	/* Functions */
	"parse",
	"lex",
	"error",
	/* Variables */
	"lval",
	"val",
	"char",
	"debug",
	"errflag",
	"nerrs",
	NULL
    };
    int i;

    for (i = 0; syms[i]; i++)
	(void) fprintf (fsppout, "define\tyy%s\t%s%s\n",
			syms[i], pre, syms[i]);
}

#endif


