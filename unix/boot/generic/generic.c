/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#define	import_spp
#include <iraf.h>

/*
 * GENERIC -- This filter takes a file containing a generic operator as input
 * and generates as output either a set of files, one for each of the data
 * types in the generic family, or a single file wherein the generic section
 * has been duplicated for each case.
 */

#define	input		lex_input
#define	unput		lex_unput
extern	char *yytext;
extern	int  yyleng;
extern	FILE *yyin;
extern	FILE *yyout;

#define	MAXFILES	512
#define	MAXNEST		50
#define	OK		0
#define	SZ_FORSTK	20

/* $FOR contstruct descriptor.
 */
struct _for {
	char	f_prevtype;		/* type before $for	*/
	char	f_types[20];		/* "csilrdx"		*/
	char	*f_curtype;		/* pointer into f_types	*/
	long	f_fpos;			/* seek offset of $FOR	*/
};

struct	_for forstk[SZ_FORSTK];
int	forlev;
char	*type_string;
char	xtype_string[SZ_FNAME+1];
char	type_char;
int	pass_output = 1;
int	clobber = NO;

extern int   yylex (void);
extern int   lex_input (void);
extern void  lex_unput (int ch);
extern long lex_tell (void);
extern void lex_seek (long fpos);


char *make_typed_filename (char *template, char type_char);
void  set_type_string (char ch);
void  copy_line (void);
void  copy_string (void);
void  copy_comment (void);
void  make_float (char type_ch);
void  output_indef (char ch);
void  output_upper (char *s);
void  pass_through (void);
void  do_for (void);
void  do_endfor (void);
void  do_if (void);
void  do_else (void);
void  do_endif (void);

int   evaluate_expr (void);
int   parse_relational (int *size1, int *size2, int *op);

int   relop (void);
int   gsize (char ch);
char  nextch (void);
char  gch (void);
void  uch (char ch);

void  output (char ch);
void  outstr (char *s);




/**
 *  GENERIC: e.g., generic [-k] [-t csilrdx] file
 */
int main (int argc, char *argv[])
{
	char	*files[MAXFILES], *s, **p, *ip;
	char	fname[SZ_FNAME], *extension;
	char	*types = "i", *t;
	char	*prefix = "";
	char	genfname[SZ_FNAME+1];
	char	template[SZ_FNAME+1];
	char	input_file[SZ_FNAME+1];
	char	*index(), *rindex();
	int	n, nfiles;
	FILE	*fp;

	genfname[0] = EOS;
	nfiles = 0;

	for (p = &argv[1];  *p != NULL;  p++) {
	    s = *p;
	    if (s[0] == '-') {
		switch (s[1]) {
		case 'k':
		    clobber = YES;
		    break;
		case 't':
		    if (*(p+1) != NULL)
			types = *++p;
		    break;
		case 'o':
		    if (*(p+1) != NULL)
			strcpy (genfname, *++p);
		    break;
		case 'p':
		    if (*(p+1) != NULL)
			prefix = *++p;
		    break;
		}
	    } else {
		files[nfiles++] = s;
		if (genfname[0] != EOS && nfiles > 1) {
		    fprintf (stderr,
			"Cannot process multiple files with '-o' flag\n");
		    exit (OSOK+1);
		}
	    }
	}
 
	for (n=0;  n < nfiles;  n++) {
	    strcpy (input_file, files[n]);
	    yyin = fopen (input_file, "r");
	    if (yyin == NULL) {
		fprintf (stderr, "Cannot open input file '%s'\n", input_file);
		continue;
	    }

	    /* Set pointer to the filename extension string.  If the file name
	     * has an extension, lop it off by overwriting the '.' with EOS.
	     * The first character of the extension of a generic file is
	     * normally a 'g', e.g., ".gx" or ".gc", but we want to generate
	     * a ".x" or ".c" file, so lop off any leading g in the extension.
	     */
	    if ((extension = rindex (input_file, '.')) != NULL) {
		*extension++ = EOS;
		if (*extension == 'g')
		    extension++;
	    } else
		extension = "";

	    for (t=types;  *t != EOS;  t++) {
		/* Make output file name */
		strcpy (fname, prefix);

		/* Expand a template of the form "chars$tchars" into the root
		 * name of the new file, replacing the $t by the type char.
		 * If using input filename as the root, add "$t"; otherwise,
		 * check whether or not the generic filename string has a
		 * "$t" in it, and add one at end if it does not.
		 */
		if (genfname[0] == EOS) {
		    strcpy (template, input_file);
		    strcat (template, "$t");

		} else {
		    strcpy (template, genfname);

		    for (ip=index(genfname,'$');  ip != NULL;
			ip = index(ip,'$')) {

			if (*(ip+1) == '$')
			    ip += 2;
			else if (*(ip+1) == 't')
			    break;
		    }

		    if (ip == NULL && strlen(types) > 1)
			strcat (ip, "$t");
		}

		if (genfname[0] == EOS || strlen (types) > 1)
		    strcat (fname, make_typed_filename (template, *t));
		else
		    strcat (fname, template);

		/* If the user supplied the output filename template, we
		 * assume that it already contains an extension.
		 */
		if (genfname[0] == EOS) {
		    strcat (fname, ".");
		    strcat (fname, extension);
		}

		if (access(fname,0) == 0) {
		    if (clobber == NO) {
			fprintf (stderr, "File `%s' already exists\n", fname);
			continue;
		    } else
			unlink (fname);
		}
		if ((fp = fopen (fname, "w")) == NULL) {
		    fprintf (stderr, "Cannot open file `%s'\n", fname);
		    continue;
		}

		yyout = fp;
		set_type_string (*t);
		type_char = *t;
		forlev = -1;

		yylex();	/* do it */

		fclose (fp);
		lex_seek (0L);
	    }

	    fclose (yyin);
	}

	exit (OSOK);
}


/* MAKE_TYPED_FILENAME -- Make a copy of a filename string, substituting
 * the given type suffix character for the every sequence "$t" found in the
 * input string.  The output string is retained in an internal static buffer.
 * Any sequence "$$" is converted into a single "$".
 */
char *
make_typed_filename (char *template, char type_char)
{
	register char *ip, *op;
	char	ch;
	static	char fname[SZ_FNAME+1];

	if (isupper (type_char))
	    ch = tolower (type_char);
	else
	    ch = type_char;

	for (ip=template, op=fname;  *ip != EOS;  )
	    if (*ip == '$' && *(ip+1) == '$') {
		*op++ = '$';
		ip += 2;
	    } else if (*ip == '$' && *(ip+1) == 't') {
		*op++ = ch;
		ip += 2;
	    } else
		*op++ = *ip++;
	    
	return (fname);
}


/* SET_TYPE_STRING -- Given the type suffix character, set the external
 * array "type_string" to the name of the corresponding SPP datatype.
 */
void
set_type_string (char ch)
{
	char	*ip, *op;

	switch (ch) {
	case 'B':
	    type_string = "ubyte";		/* unsigned byte	*/
	    break;
	case 'U':
	    type_string = "ushort";
	    break;
	case 'b':
	    type_string = "bool";
	    break;
	case 'c':
	    type_string = "char";
	    break;
	case 's':
	    type_string = "short";
	    break;
	case 'i':
	    type_string = "int";
	    break;
	case 'l':
	    type_string = "long";
	    break;
	case 'r':
	    type_string = "real";
	    break;
	case 'd':
	    type_string = "double";
	    break;
	case 'x':
	    type_string = "complex";
	    break;
	case 'p':
	    type_string = "pointer";
	    break;
	default:
	    fprintf (stderr, "Unknown type suffix char `%c'\n", ch);
	}

	op = xtype_string;
	*op++ = 'X';
	for (ip=type_string;  *ip != EOS;  ip++)
	    *op++ = toupper (*ip);
	*op++ = EOS;
}


/* COPY_LINE -- Output whatever is in the yylex token buffer, followed by the
 * remainder of the line from which the token was extracted.
 */
void
copy_line (void)
{
	char	ch;

	outstr(yytext);
	while ((ch = input()) != '\n')
	    output(ch);
	unput(ch);
}


/* COPY_STRING -- Called when the opening quote of a string is seen in the
 * input.  Copy the opening quote followed by all input characters until the
 * end of string is seen.
 */
void
copy_string (void)
{
	char	ch;

	outstr(yytext);
	for (;;) {
	    switch (ch = input()) {
	    case '"':
		output(ch);
		return;
	    case '\\':
		output(ch);
		if ((ch = input()) != '\n')
		    output(ch);
		else
		    unput(ch);
		break;
	    case '\n':
		unput(ch);
		return;
	    default:
		output(ch);
	    }
	}
}


/* COPY_COMMENT -- Copy a C style comment to the output file.
 */
void
copy_comment (void)
{
	char	ch;
	int	flag = 0;

	outstr (yytext);

	while ((ch = input()) != EOF) {
	    output (ch);
	    switch (ch) {
	    case '*':
		flag = 1;
		break;
	    case '/':
		if (flag == 1)
		    return;
		else
		    flag = 0;
		break;
	    default:
		flag = 0;
		break;
	    }
	}
}


/* MAKE_FLOAT -- Called when a n$f is seen in the input to convert a numeric
 * constant to the form appropriate for the indicated datatype, e.g., "0",
 * "0.", "0.0D0", etc.
 */
void
make_float (char type_ch)
{
	char	*p;

	for (p=yytext;  *p != '$';  p++)
	    ;
	*p = EOS;

	if (type_ch == 'x') {
	    output ('(');
	    outstr (yytext);
	    outstr (".0,");
	    outstr (yytext);
	    outstr (".0)");
	} else {
	    outstr (yytext);
	    switch (type_ch) {
	    case 'r':
		outstr (".0");
		break;
	    case 'd':
		outstr (".0D0");
		break;
	    }
	}
}


/* OUTPUT_INDEF -- Output the INDEF string for the indicated datatype.
 */
void
output_indef (char ch)	/* output INDEF, INDEFS, INDEFL, etc. */
{
	outstr(yytext);

	switch (ch) {
	case 's':
	    output ('S');
	    break;
	case 'i':
	    output ('I');
	    break;
	case 'l':
	    output ('L');
	    break;
	case 'r':
	    output ('R');
	    break;
	case 'd':
	    output ('D');
	    break;
	case 'x':
	    output ('X');
	    break;
	}
}


/* OUTPUT_UPPER -- Output the name of the current datatype (INT, REAL, etc.)
 * in upper case.
 */
void
output_upper (char *s)
{
	char	ch, *p;

	outstr(s);
	for (p=type_string;  (ch = *p) != EOS;  p++)
	    output(toupper(ch));
}


/* PASS_THROUGH -- Used to pass text on to the output without modification.
 * The text is delimited as "$/ (text) /" in the input file.  The delimited
 * section may enclose newlines.
 */
void
pass_through (void)
{
	char	ch;

	while ((ch = input()) != '/')
	    output(ch);
}


/* DO_FOR -- Process a "$FOR (types)" statement.  The sequence of statements
 * bracketed by $for ... $endfor will be processed and output (to a single
 * output stream) for each datatype named in the for predicate.
 */
void
do_for (void)
{
	register char	*op;
	register int	ch;
	register struct _for *fp;
	char	types[20];

	if (++forlev + 1 >= SZ_FORSTK) {
	    fprintf (stderr, "$for statements nested too deeply\n");
	    exit (OSOK+1);
	}

	/* Extract list of types.
	 */
	while ((ch = input()) != '(')
	    if (ch == EOF || ch == '\n') {
		fprintf (stderr, "$for must have () delimited list of types\n");
		strcpy (types, "i");
		goto init_;
	    }

	for (op=types;  (ch = input()) != ')';  op++)
	    if (ch == EOF || ch == '\n') {
		fprintf (stderr, "missing right paren in $for statement\n");
		break;
	    } else
		*op = ch;

	*op = EOS;
	if (op == types) {
	    fprintf (stderr, "null typelist in $for statement\n");
	    strcpy (types, "i");
	}

init_:
	fp = &forstk[forlev];
	fp->f_prevtype = type_char;
	strcpy (fp->f_types, types);
	fp->f_curtype = fp->f_types;
	fp->f_fpos = lex_tell();

	type_char = *(fp->f_curtype)++;
	set_type_string (type_char);
}


/* DO_ENDFOR -- Called to process a $ENDFOR.  Set the next datatype and seek
 * back to the line following the matching $FOR statement.  When the type list
 * is exhausted pop the $for stack and continue normal processing.
 */
void
do_endfor (void)
{
	register struct _for *fp;

	if (forlev < 0) {
	    fprintf (stderr, "$endfor with no matching $for\n");
	    return;
	}

	fp = &forstk[forlev];
	if ((type_char = *(fp->f_curtype)++) != EOS) {
	    set_type_string (type_char);
	    lex_seek (fp->f_fpos);
	} else {
	    type_char = fp->f_prevtype;
	    set_type_string (type_char);
	    --forlev;
	}
}


/*
 * Conditional Compilation
 * -------------------------
 */

#define	TRUE	1
#define	FALSE	0
#define	EQ	0
#define	NE	1
#define	LE	2
#define	LT	3
#define	GE	4
#define	GT	5

char	expr_buf[80], *expr;
int	level = 0;

struct	if_stack {
	int	oldstate;
	int	active;
} stk[MAXNEST];


/* DO_IF -- Process a $IF statement.  Evaluate the predicate and push a
 * pass or stop output flag on the if stack.
 */
void
do_if (void)
{
	char	ch;
	int	expr_value;
	struct	if_stack *p;

	level += 1;
	p = &stk[level];
	p->oldstate = pass_output;
	p->active = (pass_output == TRUE);

	if ((expr_value = evaluate_expr()) == ERR)
	    expr_value = FALSE;

	if ((ch = input()) != '\n')
	    unput(ch);

	if (p->active == FALSE)
	    return;
	else if (expr_value == FALSE)
	    pass_output = FALSE;
}
	

/* DO_ELSE -- Process a $ELSE statement.  Toggle the pass/stop output flag
 * on top of the if stack.
 */
void
do_else (void)
{
	char	ch;

	if (level == 0)
	    fprintf (stderr, "Unmatched $else statement\n");
	else if (stk[level].active)		/* toggle pass_output */
	    pass_output = (pass_output == FALSE);

	if ((ch = input()) != '\n')
	    unput(ch);
}


/* DO_ENDIF -- Process a $ENDIF statement.  Pop the if stack.
 */
void
do_endif (void)		/* $endif statement */
{
	char	ch;

	if (level == 0)
	    fprintf (stderr, "Too many $endif statements\n");
	else
	    pass_output = stk[level--].oldstate;

	if ((ch = input()) != '\n')
	    unput(ch);
}


/* EVALUATE_EXPR -- Kludge to evaluate boolean expressions in $if statements.
 * Two kinds of expressions are permitted: (datatype relop chars), or
 * (sizeof(char) relop sizeof(char)), where relop = (==, !=, <= etc.).
 *
 * Examples:		$if (datatype != dx)
 *			    (code to be compiled if type not d or x)
 *
 *			$if (sizeof(i) <= sizeof(r))
 *			    (code to be compiled if size int <= real)
 */
int
evaluate_expr (void)
{
	char	ch=0, *p, *index(); 
	int	lpar, size1, size2, op;


	/* Advance to start of expression (discard '(') */
	if (nextch() != '(')
	    goto err;
	else
	    input();

	/* Extract expression string into buffer */
	expr = expr_buf;
	nextch();

	for (p=expr_buf, lpar=1;  lpar > 0 && (*p = input()) != EOF;  p++)
	    switch (ch = *p) {
	    case '(':
		lpar++;
		break;
	    case ')':
		if (--lpar == 0)
		    *p = EOS;
		break;
	    case '\n':
		goto err;
	    }
		
	/* Is current type in set or not in set */
	if (strncmp (expr,"datatype",8) == 0) {
	    expr += 8;
	    switch (relop()) {
	    case EQ:
		return (index(expr,type_char) != NULL);
	    case NE:
		return (index(expr,type_char) == NULL);
	    default:
		goto err;
	    }

	/* Compare sizes of two data types */
	} else if (strncmp(expr,"sizeof",6) == 0) {
	    if (parse_relational (&size1, &size2, &op) == ERR) {
		ch = 0;
		goto err;
	    }
	    switch (op) {
	    case EQ:
		return (size1 == size2);
	    case NE:
		return (size1 != size2);
	    case LE:
		return (size1 <= size2);
	    case LT:
		return (size1 <  size2);
	    case GE:
		return (size1 >= size2);
	    case GT:
		return (size1 >  size2);
	    }

	/* only "type" and "sizeof" are implemented */
	} else {
err:	    fprintf (stderr, "Syntax error in $if statement\n");
	    if (ch != '\n') {
		/* skip rest of line */
		while ((ch = input()) != '\n')
		    ;
		unput(ch);
	    }
	}

	return (ERR);
}


/* PARSE_RELATIONAL -- Parse "sizeof(t1) relop sizeof(t2)"  (via brute force...) */
int
parse_relational (int *size1, int *size2, int *op)
{
	expr += 6;				/* ... (t1) */

	if (gch() != '(')
	    return (ERR);
	if ((*size1 = gsize(gch())) == ERR)
	    return (ERR);
	if (gch() != ')')
	    return (ERR);			/* relop */
	if ((*op = relop()) == ERR)
	    return (ERR);

	uch (gch());				/* skip whitespace */

	if (strncmp(expr,"sizeof",6) != 0)	/* sizeof(t2) */
	    return (ERR);

	expr += 6;

	if (gch() != '(')
	    return (ERR);
	if ((*size2 = gsize(gch())) == ERR)
	    return (ERR);
	if (gch() != ')')
	    return (ERR);

	return (OK);
}


/* RELOP -- Return a code for the next relational operator token in the input
 * stream.
 */
int
relop (void)
{
	char	ch;


	switch (gch()) {
	case '!':
	    if (gch() == '=')
		return (NE);
	    return (ERR);
	case '=':
	    if (gch() == '=')
		return (EQ);
	    return (ERR);
	case '<':
	    if ((ch = gch()) == '=')
		return (LE);
	    uch(ch);
	    return (LT);
	case '>':
	    if ((ch = gch()) == '=')
		return (GE);
	    uch(ch);
	    return (GT);
	default:
	    return (ERR);
	}
}
		    

/* GSIZE -- Return the size of a datatype given its character code.
 */
int
gsize (char ch)
{
	switch (ch) {
	case 'B':
	    return (sizeof(XUBYTE));
	case 'U':
	    return (sizeof(XUSHORT));
	case 't':
	    return (gsize(type_char));
	case 'c':
	    return (sizeof(XCHAR));
	case 's':
	    return (sizeof(XSHORT));
	case 'i':
	    return (sizeof(XINT));
	case 'l':
	    return (sizeof(XLONG));
	case 'r':
	    return (sizeof(XREAL));
	case 'd':
	    return (sizeof(XDOUBLE));
	case 'x':
	    return (sizeof(XCOMPLEX));
	case 'p':
	    return (sizeof(XPOINTER));
	default:
	    return (ERR);
	}
}


/* NEXTCH -- Advance to next non-whitespace character.
 */
char 
nextch (void)
{
	char	ch;

	for (ch=input();  ch == ' ' || ch == '\t';  ch=input())
	    ;
	unput (ch);
	return (ch);
}


/* GCH -- Get next nonwhite char from expression buffer.
 */
char
gch (void)
{
	while (*expr == ' ' || *expr == '\t')
	    expr++;

	if (*expr != EOS)
	    return (*expr++);
	else
	    return (EOS);
}


/* UCH -- Put char back into expression buffer.
 */
void
uch (char ch)
{
	*--expr = ch;
}


/* OUTPUT -- Write a single character to the output file, if output is
 * currently enabled (else throw it away).
 */
void
output (char ch)
{
	if (pass_output)
	    putc (ch, yyout);
}


/* OUTSTR -- Output a string.
 */
void
outstr (char *s)
{
	if (pass_output)
	    fputs (s, yyout);
}
