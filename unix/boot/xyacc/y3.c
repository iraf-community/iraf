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

static void go2gen (int);
static void precftn (int, int, int);
static void wract (int);
static void wrstate (int);
static void wdef (char *, int);
static void wrmbchars (void);
	/* important local variables */
static int lastred;		/* number of the last reduction of a state */
int *defact;
extern int *toklev;
extern int cwp;

int exca[NSTATES * 2];		/* buffer states for printing with warray  */
int nexca;


	/* I/O descriptors */

extern FILE *finput;		/* input file                   */
extern FILE *faction;		/* file for saving actions      */
extern FILE *fdefine;		/* file for #defines            */
extern FILE *fudecl;		/* file for user declarations   */
extern FILE *ftable;		/* parser tables file           */
extern FILE *fsppout;		/* SPP output file              */
extern FILE *ftemp;		/* tempfile to pass 2           */
extern FILE *foutput;		/* y.output file                */




/* print the output for the states */
void
output (void)
{
    int i, k, c;
    register WSET *u, *v;

    /*
       (void) fprintf(fsppout, "static YYCONST yytabelem yyexca[] ={\n");
     */

    SLOOP (i) {			/* output the stuff for state i */
	nolook = !(tystate[i] == MUSTLOOKAHEAD);
	closure (i);
	/* output actions */
	nolook = 1;
	aryfil (temp1, ntoksz + nnontersz + 1, 0);
	WSLOOP (wsets, u) {
	    c = *(u->pitem);
	    if (c > 1 && c < NTBASE && temp1[c] == 0) {
		WSLOOP (u, v) {
		    if (c == *(v->pitem))
			putitem (v->pitem + 1, (LOOKSETS *) 0);
		}
		temp1[c] = state (c);
	    } else if (c > NTBASE && temp1[(c -= NTBASE) + ntokens] == 0) {
		temp1[c + ntokens] = amem[indgo[i] + c];
	    }
	}
	if (i == 1)
	    temp1[1] = ACCEPTCODE;
	/* now, we have the shifts; look at the reductions */
	lastred = 0;
	WSLOOP (wsets, u) {
	    c = *(u->pitem);
	    if (c <= 0) {	/* reduction */
		lastred = -c;
		TLOOP (k) {
		    if (BIT (u->ws.lset, k)) {
			if (temp1[k] == 0)
			    temp1[k] = c;
			else if (temp1[k] < 0) {
			    /*
			     * reduce/reduce
			     * conflict
			     */
			    /* BEGIN CSTYLED */
			    if (foutput != NULL)
				(void) fprintf (foutput,
						WSFMT
						("\n%d: reduce/reduce conflict"
						 " (red'ns %d and %d ) on %s"),
						i, -temp1[k], lastred,
						symnam (k));
			    if (-temp1[k] > lastred)
				temp1[k] = -lastred;
			    ++zzrrconf;
			    /* END CSTYLED */
			} else
			    /*
			     * potentia
			     * shift/reduce
			     * conflict.
			     */
			    precftn (lastred, k, i);
		    }
		}
	    }
	}
	wract (i);
    }

    /*
       (void) fprintf(fsppout, "\t};\n");
     */
    warray ("yyexca", exca, nexca);
    wdef ("YYNPROD", nprod);
    if (nmbchars > 0) {
	wrmbchars ();
    }
}

static int pkdebug = 0;
int
apack (int *p, int n)
{
    /* pack state i from temp1 into amem */
    int off;
    int *pp, *qq;
    int *q, *rr;
    int diff;

    /*
     * we don't need to worry about checking because we
     * we will only look up entries known to be there...
     */

    /* eliminate leading and trailing 0's */

    q = p + n;
    for (pp = p, off = 0; *pp == 0 && pp <= q; ++pp, --off)
	/* NULL */ ;
    if (pp > q)
	return (0);		/* no actions */
    p = pp;

    /* now, find a place for the elements from p to q, inclusive */
    /* for( rr=amem; rr<=r; ++rr,++off ){ *//* try rr */
    rr = amem;
    for (;; ++rr, ++off) {
	while (rr >= &amem[new_actsize - 1])
	    exp_act (&rr);
	qq = rr;
	for (pp = p; pp <= q; ++pp, ++qq) {
	    if (*pp) {
		diff = qq - rr;
		while (qq >= &amem[new_actsize - 1]) {
		    exp_act (&rr);
		    qq = diff + rr;
		}
		if (*pp != *qq && *qq != 0)
		    goto nextk;
	    }
	}

	/* we have found an acceptable k */

	if (pkdebug && foutput != NULL)
	    (void) fprintf (foutput,
			    "off = %d, k = %" PRIdPTR "\n", off, rr - amem);

	qq = rr;
	for (pp = p; pp <= q; ++pp, ++qq) {
	    if (*pp) {
		diff = qq - rr;
		while (qq >= &amem[new_actsize - 1]) {
		    exp_act (&rr);
		    qq = diff + rr;
		}
		if (qq > memp)
		    memp = qq;
		*qq = *pp;
	    }
	}
	if (pkdebug && foutput != NULL) {
	    for (pp = amem; pp <= memp; pp += 10) {
		(void) fprintf (foutput, "\t");
		for (qq = pp; qq <= pp + 9; ++qq)
		    (void) fprintf (foutput, "%d ", *qq);
		(void) fprintf (foutput, "\n");
	    }
	}
	return (off);
      nextk:;
    }
    /* error("no space in action table" ); */
    /* NOTREACHED */
}

void
go2out (void)
{
    /* output the gotos for the nontermninals */
    int i, j, k, best, count, cbest, times;

    (void) fprintf (ftemp, "$\n");	/* mark begining of gotos */

    for (i = 1; i <= nnonter; ++i) {
	go2gen (i);
	/* find the best one to make default */
	best = -1;
	times = 0;
	for (j = 0; j < nstate; ++j) {	/* is j the most frequent */
	    if (tystate[j] == 0)
		continue;
	    if (tystate[j] == best)
		continue;
	    /* is tystate[j] the most frequent */
	    count = 0;
	    cbest = tystate[j];
	    for (k = j; k < nstate; ++k)
		if (tystate[k] == cbest)
		    ++count;
	    if (count > times) {
		best = cbest;
		times = count;
	    }
	}

	/* best is now the default entry */
	zzgobest += (times - 1);
	for (j = 0; j < nstate; ++j) {
	    if (tystate[j] != 0 && tystate[j] != best) {
		(void) fprintf (ftemp, "%d,%d,", j, tystate[j]);
		zzgoent += 1;
	    }
	}

	/* now, the default */

	zzgoent += 1;
	(void) fprintf (ftemp, "%d\n", best);

    }
}

static int g2debug = 0;
static void
go2gen (int c)
{
    /* output the gotos for nonterminal c */
    int i, work, cc;
    ITEM *p, *q;

    /* first, find nonterminals with gotos on c */
    aryfil (temp1, nnonter + 1, 0);
    temp1[c] = 1;

    work = 1;
    while (work) {
	work = 0;
	PLOOP (0, i) {
	    if ((cc = prdptr[i][1] - NTBASE) >= 0) {
		/* cc is a nonterminal */
		if (temp1[cc] != 0) {
		    /*
		     * cc has a goto on c
		     * thus, the left side of
		     * production i does too.
		     */
		    cc = *prdptr[i] - NTBASE;
		    if (temp1[cc] == 0) {
			work = 1;
			temp1[cc] = 1;
		    }
		}
	    }
	}
    }

    /* now, we have temp1[c] = 1 if a goto on c in closure of cc */

    if (g2debug && foutput != NULL) {
	(void) fprintf (foutput, WSFMT ("%s: gotos on "), nontrst[c].name);
	NTLOOP (i) if (temp1[i])
	    (void) fprintf (foutput, WSFMT ("%s "), nontrst[i].name);
	(void) fprintf (foutput, "\n");
    }

    /* now, go through and put gotos into tystate */
    aryfil (tystate, nstate, 0);
    SLOOP (i) {
	ITMLOOP (i, p, q) {
	    if ((cc = *p->pitem) >= NTBASE) {
		if (temp1[cc -= NTBASE]) {
		    /* goto on c is possible */
		    tystate[i] = amem[indgo[i] + c];
		    break;
		}
	    }
	}
    }
}

/* decide a shift/reduce conflict by precedence.  */
static void
precftn (int r, int t, int s)
{

    /*
     * r is a rule number, t a token number
     * the conflict is in state s
     * temp1[t] is changed to reflect the action
     */

    int lp, lt, action;

    lp = levprd[r];
    lt = toklev[t];
    if (PLEVEL (lt) == 0 || PLEVEL (lp) == 0) {
	/* conflict */
	if (foutput != NULL)
	    (void) fprintf (foutput,
			    WSFMT ("\n%d: shift/reduce conflict"
				   " (shift %d, red'n %d) on %s"),
			    s, temp1[t], r, symnam (t));
	++zzsrconf;
	return;
    }
    if (PLEVEL (lt) == PLEVEL (lp))
	action = ASSOC (lt) & ~04;
    else if (PLEVEL (lt) > PLEVEL (lp))
	action = RASC;		/* shift */
    else
	action = LASC;		/* reduce */

    switch (action) {
    case BASC:			/* error action */
	temp1[t] = ERRCODE;
	return;
    case LASC:			/* reduce */
	temp1[t] = -r;
	return;
    }
}


/* WRACT -- Output the state I.  Modified to save state array in exca
 * for later printing by warray.
 */
static void
wract (int i)
{
    /* output state i */
    /* temp1 has the actions, lastred the default */
    int p, p0, p1;
    int ntimes, tred, count, j;
    int flag;

    /* find the best choice for lastred */

    lastred = 0;
    ntimes = 0;
    TLOOP (j) {
	if (temp1[j] >= 0)
	    continue;
	if (temp1[j] + lastred == 0)
	    continue;
	/* count the number of appearances of temp1[j] */
	count = 0;
	tred = -temp1[j];
	levprd[tred] |= REDFLAG;
	TLOOP (p) {
	    if (temp1[p] + tred == 0)
		++count;
	}
	if (count > ntimes) {
	    lastred = tred;
	    ntimes = count;
	}
    }

    /*
     * for error recovery, arrange that, if there is a shift on the
     * error recovery token, `error', that the default be the error action
     if (temp1[2] > 0)
     */
    if (temp1[1] > 0)
	lastred = 0;

    /* clear out entries in temp1 which equal lastred */
    TLOOP (p) {
	if (temp1[p] + lastred == 0)
	    temp1[p] = 0;
    }

    wrstate (i);
    defact[i] = lastred;

    flag = 0;
    TLOOP (p0) {
	if ((p1 = temp1[p0]) != 0) {
	    if (p1 < 0) {
		p1 = -p1;
		goto exc;
	    } else if (p1 == ACCEPTCODE) {
		p1 = -1;
		goto exc;
	    } else if (p1 == ERRCODE) {
		p1 = 0;
		goto exc;
	      exc:
		if (flag++ == 0) {
		    exca[nexca++] = -1;
		    exca[nexca++] = i;
		}
		exca[nexca++] = tokset[p0].value;
		exca[nexca++] = p1;
		++zzexcp;
		if (nexca >= NSTATES * 2) {
		    error ("state table overflow");
		}
	    } else {
		(void) fprintf (ftemp, "%d,%d,", tokset[p0].value, p1);
		++zzacent;
	    }
	}
    }
    if (flag) {
	defact[i] = -2;
	exca[nexca++] = -2;
	exca[nexca++] = lastred;
    }
    (void) fprintf (ftemp, "\n");
}

static void
wrstate (int i)
{
    /* writes state i */
    int j0, j1;
    register ITEM *pp, *qq;
    register WSET *u;

    if (foutput == NULL)
	return;
    (void) fprintf (foutput, "\nstate %d\n", i);
    ITMLOOP (i, pp, qq) {
	(void) fprintf (foutput, WSFMT ("\t%s\n"), writem (pp->pitem));
    }
    if (tystate[i] == MUSTLOOKAHEAD) {
	/* print out empty productions in closure */
	WSLOOP (wsets + (pstate[i + 1] - pstate[i]), u) {
	    if (*(u->pitem) < 0)
		(void) fprintf (foutput, WSFMT ("\t%s\n"), writem (u->pitem));
	}
    }

    /* check for state equal to another */
    TLOOP (j0) if ((j1 = temp1[j0]) != 0) {
	(void) fprintf (foutput, WSFMT ("\n\t%s  "), symnam (j0));
	if (j1 > 0) {		/* shift, error, or accept */
	    if (j1 == ACCEPTCODE)
		(void) fprintf (foutput, "accept");
	    else if (j1 == ERRCODE)
		(void) fprintf (foutput, "error");
	    else
		(void) fprintf (foutput, "shift %d", j1);
	} else
	    (void) fprintf (foutput, "reduce %d", -j1);
    }

    /* output the final production */
    if (lastred)
	(void) fprintf (foutput, "\n\t.  reduce %d\n\n", lastred);
    else
	(void) fprintf (foutput, "\n\t.  error\n\n");

    /* now, output nonterminal actions */
    j1 = ntokens;
    for (j0 = 1; j0 <= nnonter; ++j0) {
	if (temp1[++j1])
	    (void) fprintf (foutput,
			    WSFMT ("\t%s  goto %d\n"),
			    symnam (j0 + NTBASE), temp1[j1]);
    }
}

static void
wdef (char *s, int n)
{
    /* output a definition of s to the value n */
    (void) fprintf (fsppout, WSFMT ("define\t%s\t\t%d\n"), s, n);
}

# define NDP_PERLINE    8

void
warray (char *s, int *v, int n)
{
    register int i, j;

    fprintf (ftable, "short\t%s[%d]\n", s, n);

    for (j = 0; j < n; j += NDP_PERLINE) {
	fprintf (ftable, "data\t(%s(i),i=%3d,%3d)\t/",
		 s, j + 1, (j + NDP_PERLINE < n) ? j + NDP_PERLINE : n);

	for (i = j; i < j + NDP_PERLINE && i < n; i++) {
	    if ((i == j + NDP_PERLINE - 1) || i >= n - 1)
		fprintf (ftable, "%4d/\n", v[i]);
	    else
		fprintf (ftable, "%4d,", v[i]);
	}
    }
}

void
hideprod (void)
{
    /*
     * in order to free up the mem and amem arrays for the optimizer,
     * and still be able to output yyr1, etc., after the sizes of
     * the action array is known, we hide the nonterminals
     * derived by productions in levprd.
     */

    int i, j;

    j = 0;
    levprd[0] = 0;
    PLOOP (1, i) {
	if (!(levprd[i] & REDFLAG)) {
	    ++j;
	    if (foutput != NULL) {
		(void) fprintf (foutput,
				WSFMT ("Rule not reduced:   %s\n"),
				writem (prdptr[i]));
	    }
	}
	levprd[i] = *prdptr[i] - NTBASE;
    }
    if (j)
/*
 * TRANSLATION_NOTE  -- This is a message from yacc.
 *	Check how 'reduced' is translated in yacc man page/document.
 */
	(void) fprintf (stderr, "%d rules never reduced\n", j);
}


static int
cmpmbchars (MBCLIT *p, MBCLIT *q)
{
    /* Compare two MBLITs. */
    return ((p->character) - (q->character));
}

static void
wrmbchars (void)
{
    int i;

    return wdef ("YYNMBCHARS", nmbchars);
    qsort (mbchars, nmbchars, sizeof (*mbchars),
	   (int (*)(const void *, const void *)) cmpmbchars);
    (void) fprintf (ftable,
		    "static struct{\n\tchar character;"
		    "\n\tint tvalue;\n}yymbchars[YYNMBCHARS]={\n");
    for (i = 0; i < nmbchars; ++i) {
	(void) fprintf (ftable, "\t{%#x,%d}",
			(int) mbchars[i].character, mbchars[i].tvalue);
	if (i < nmbchars - 1) {
	    /* Not the last. */
	    (void) fprintf (ftable, ",\n");
	}
    }
    (void) fprintf (ftable, "\n};\n");
}
