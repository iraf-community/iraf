/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):yy_getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin = {stdin}, *yyout = {stdout};
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;
# define YYNEWLINE 10
int 
lex_yylex (void){
int nstr; extern int yyprevious;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
	/* groups of blanks and tabs, while significant as delimiters,
		 * are otherwise ignored.
		 */ ;
break;
case 2:
{	/* trailing ',' implies continuation */
			return (',');
		}
break;
case 3:
{	/* trailing '\' completely absorbed */
		}
break;
case 4:
{
			/* Host os command escape.  Remove everything up through
			 * '!'.  Let clsystem decide what to do with null cmd.
			 * Must precede the "!" YOP_NOT spec in this file.
			 */
			register char *cp;
			for (cp = yytext;  *cp++ != '!';  )
			    ;
			yylval = addconst (cp, OT_STRING);
			return (Y_OSESC);
		}
break;
case 5:
	return (Y_ALLPIPE);
break;
case 6:
	return (Y_APPEND);
break;
case 7:
	return (Y_ALLAPPEND);
break;
case 8:
	return (Y_ALLREDIR);
break;
case 9:
{
			yylval = addconst (yytext, OT_STRING);
			return (Y_GSREDIR);
		}
break;
case 10:
	return (YOP_LE);
break;
case 11:
	return (YOP_GE);
break;
case 12:
	return (YOP_EQ);
break;
case 13:
	return (YOP_NE);
break;
case 14:
	return (YOP_POW);
break;
case 15:
	return (YOP_OR);
break;
case 16:
	return (YOP_AND);
break;
case 17:
	return (YOP_NOT);
break;
case 18:
	return (YOP_AOADD);
break;
case 19:
	return (YOP_AOSUB);
break;
case 20:
	return (YOP_AOMUL);
break;
case 21:
	return (YOP_AODIV);
break;
case 22:
	return (YOP_AOCAT);
break;
case 23:
	return (YOP_CONCAT);
break;
case 24:
	{ if (dobrace) {
			dobrace = NO;
			return (*yytext);
		  } else {
			dobrace = YES;
			unput (*yytext);
			return (';');
		  }
		}
break;
case 25:
	return (*yytext);
break;
case 26:
	return (*yytext);
break;
case 27:
	return (crackident (yytext));
break;
case 28:
	return (crackident (yytext));
break;
case 29:
	{	extern bracelevel;
			if (bracelevel) {
	    eprintf ("ERROR: background not allowed within statement block\n");
			    return ('#');
			} else {
			    yyleng = 0;
			    while ((yytext[yyleng]=input()) != '\n')
				yyleng++;
			    yytext[yyleng] = '\0';
			    bkg_init (yytext);
			    return (Y_NEWLINE);
			}
		}
break;
case 30:
{
			/* crackident() sets yylval and returns token value.
			 */
			return (crackident (yytext));
		}
break;
case 31:
{
			/* must precede OT_REAL as integers also match there */
			yylval = addconst (yytext, OT_INT);
			return (Y_CONSTANT);
		}
break;
case 32:
{
			yylval = addconst (yytext, OT_REAL);
		 	return (Y_CONSTANT);
		}
break;
case 33:
{
			/* sexagesimal format */
			yylval = addconst (yytext, OT_REAL);
			return (Y_CONSTANT);
		}
break;
case 34:
{	/* Quoted string.  call traverse() to read the
			 * string into yytext.
			 */
			traverse (*yytext);
			yylval = addconst (yytext, OT_STRING);
			return (Y_CONSTANT);
		}
break;
case 35:
	return (Y_NEWLINE);
break;
case 36:
{	/* Ignore a comment. */
			while (input() != '\n')
			    ;
			unput ('\n');
		}
break;
case 37:
	return (*yytext);
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of lex_yylex */

#include "errs.h"

/* See gram.c for the various support functions, such as addconst()
 * and crackident().  Traverse is included here since it directly
 * references input, unput, yytext, etc.
 */

/* TRAVERSE -- Called by the lexical analyzer when a quoted string has
 * been recognized.  Characters are input and deposited in yytext (the
 * lexical analyzer token buffer) until the trailing quote is seen.
 * Strings may not span lines unless the newline is delimited.  The
 * recognized escape sequences are converted upon input; all others are
 * left alone, presumably to later be converted by other code.
 * Quotes may be included in the string by escaping them, or by means of
 * the double quote convention.
 */
int 
traverse (int delim)
{
	register char *op, *cp, ch;
	static	char *esc_ch  = "ntfr\\\"'";
	static	char *esc_val = "\n\t\f\r\\\"\'";
	char	*index();

	for (op=yytext;  (*op = input()) != EOF;  op++) {
	    if (*op == delim) {
		if ((*op = input()) == EOF)
		    break;
		if (*op == delim)
		    continue;	/* double quote convention; keep one */
		else {
		    unput (*op);
		    break;			/* normal exit	*/
		}

	    } else if (*op == '\n') {		/* error recovery exit	*/
		*op = '\0';
		cl_error (E_UERR, "Newline while processing string");
		break;

	    } else if (*op == '\\') {
		if ((*op = input()) == EOF) {
		    break;
		} else if (*op == '\n') {
		    --op;			/* explicit continuation */
		    while ((ch = input()) && isspace(ch) || ch == '#') {
			if (ch == '#')
			    while ((ch = input()) && ch != '\n')
				;
		    }
		    unput (ch);
		    continue;
		} else if ((cp = index (esc_ch, *op)) != NULL) {
		    *op = esc_val[cp-esc_ch];
		} else if (isdigit (*op)) {	/* '\0DD' octal constant   */
		    *op -= '0';
		    while (isdigit (ch = input()))
			*op = (*op * 8) + (ch - '0');
		    unput (ch);
		} else {
		    ch = *op;			/* unknown escape sequence, */
		    *op++ = '\\';		/* leave it alone.	    */
		    *op = ch;
		}
	    }
	}

	*op = '\0';
	yyleng = (op - yytext);
}
int yyvstop[] = {
0,

37,
0,

1,
37,
0,

35,
0,

17,
37,
0,

34,
37,
0,

36,
37,
0,

30,
37,
0,

29,
37,
0,

37,
0,

37,
0,

37,
0,

37,
0,

37,
0,

26,
37,
0,

31,
32,
37,
0,

37,
0,

37,
0,

37,
0,

27,
37,
0,

37,
0,

25,
37,
0,

37,
0,

24,
37,
0,

1,
37,
0,

4,
17,
37,
0,

1,
0,

13,
0,

30,
0,

16,
0,

14,
0,

20,
0,

18,
0,

2,
0,

19,
0,

32,
0,

23,
0,

21,
0,

32,
0,

31,
32,
0,

31,
0,

31,
0,

10,
0,

12,
0,

8,
0,

11,
0,

6,
0,

9,
0,

28,
0,

3,
0,

5,
0,

15,
0,

1,
0,

4,
0,

4,
13,
0,

22,
0,

33,
0,

32,
0,

7,
0,

32,
0,

33,
0,

33,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,4,	1,5,	
61,0,	0,0,	0,0,	4,28,	
0,0,	0,0,	0,0,	13,35,	
13,36,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	22,55,	22,56,	0,0,	
26,59,	0,0,	1,6,	1,7,	
1,8,	1,9,	4,28,	1,10,	
1,7,	10,31,	13,35,	1,11,	
1,12,	1,13,	1,14,	1,15,	
1,16,	1,17,	2,26,	11,32,	
22,55,	0,0,	24,57,	26,59,	
26,60,	0,0,	0,0,	16,39,	
64,69,	1,18,	1,19,	1,20,	
1,21,	6,29,	1,9,	1,9,	
12,34,	1,9,	11,33,	14,37,	
1,9,	16,40,	2,27,	2,7,	
2,8,	2,9,	18,48,	2,10,	
2,7,	19,49,	21,54,	2,11,	
39,63,	2,13,	2,14,	2,15,	
2,16,	1,9,	38,62,	38,62,	
0,0,	1,22,	0,0,	1,23,	
1,9,	20,50,	0,0,	0,0,	
0,0,	2,18,	2,19,	2,20,	
2,21,	15,38,	15,38,	15,38,	
15,38,	15,38,	15,38,	15,38,	
15,38,	15,38,	15,38,	53,53,	
0,0,	53,53,	0,0,	0,0,	
20,51,	20,52,	38,62,	38,62,	
53,53,	1,24,	1,25,	0,0,	
0,0,	0,0,	20,53,	0,0,	
20,53,	2,22,	0,0,	2,23,	
2,9,	0,0,	9,30,	20,53,	
24,58,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
9,30,	0,0,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
0,0,	0,0,	0,0,	0,0,	
0,0,	2,24,	2,25,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	0,0,	0,0,	0,0,	
0,0,	9,30,	0,0,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	9,30,	9,30,	9,30,	
9,30,	17,41,	0,0,	17,42,	
17,42,	17,42,	17,42,	17,42,	
17,42,	17,42,	17,42,	17,42,	
17,42,	17,43,	0,0,	0,0,	
0,0,	0,0,	27,60,	0,0,	
17,44,	17,45,	17,44,	17,46,	
17,46,	17,44,	27,60,	27,0,	
41,41,	41,41,	41,41,	41,41,	
41,41,	41,41,	41,41,	41,41,	
41,41,	41,41,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,47,	
52,67,	0,0,	0,0,	0,0,	
41,62,	41,62,	0,0,	0,0,	
17,44,	17,45,	17,44,	17,46,	
17,46,	17,44,	0,0,	0,0,	
27,60,	0,0,	0,0,	0,0,	
0,0,	27,60,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	17,47,	
0,0,	52,53,	27,61,	52,53,	
41,62,	41,62,	27,60,	27,60,	
0,0,	27,60,	52,53,	0,0,	
27,60,	43,64,	43,64,	43,64,	
43,64,	43,64,	43,64,	43,64,	
43,64,	43,64,	43,64,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	27,60,	44,44,	44,44,	
44,44,	44,44,	44,44,	44,44,	
44,44,	44,44,	44,44,	44,44,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	44,44,	
44,44,	44,44,	44,44,	44,44,	
44,44,	46,65,	0,0,	46,65,	
0,0,	0,0,	46,66,	46,66,	
46,66,	46,66,	46,66,	46,66,	
46,66,	46,66,	46,66,	46,66,	
60,60,	0,0,	44,47,	0,0,	
0,0,	0,0,	0,0,	0,0,	
60,60,	60,0,	0,0,	44,44,	
44,44,	44,44,	44,44,	44,44,	
44,44,	62,65,	0,0,	62,65,	
0,0,	0,0,	62,68,	62,68,	
62,68,	62,68,	62,68,	62,68,	
62,68,	62,68,	62,68,	62,68,	
0,0,	0,0,	44,47,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	60,60,	0,0,	
0,0,	0,0,	0,0,	60,60,	
65,68,	65,68,	65,68,	65,68,	
65,68,	65,68,	65,68,	65,68,	
65,68,	65,68,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
60,60,	60,60,	0,0,	60,60,	
0,0,	0,0,	60,60,	66,66,	
66,66,	66,66,	66,66,	66,66,	
66,66,	66,66,	66,66,	66,66,	
66,66,	0,0,	0,0,	0,0,	
0,0,	0,0,	69,70,	60,60,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	69,69,	69,69,	
69,69,	69,69,	70,70,	70,70,	
70,70,	70,70,	70,70,	70,70,	
70,70,	70,70,	70,70,	70,70,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+-1,	0,		0,	
yycrank+-41,	yysvec+1,	0,	
yycrank+0,	0,		yyvstop+1,
yycrank+6,	0,		yyvstop+3,
yycrank+0,	0,		yyvstop+6,
yycrank+4,	0,		yyvstop+8,
yycrank+0,	0,		yyvstop+11,
yycrank+0,	0,		yyvstop+14,
yycrank+102,	0,		yyvstop+17,
yycrank+3,	0,		yyvstop+20,
yycrank+9,	0,		yyvstop+23,
yycrank+7,	0,		yyvstop+25,
yycrank+10,	0,		yyvstop+27,
yycrank+10,	0,		yyvstop+29,
yycrank+57,	0,		yyvstop+31,
yycrank+12,	0,		yyvstop+33,
yycrank+179,	0,		yyvstop+36,
yycrank+17,	0,		yyvstop+40,
yycrank+20,	0,		yyvstop+42,
yycrank+59,	0,		yyvstop+44,
yycrank+19,	0,		yyvstop+46,
yycrank+20,	0,		yyvstop+49,
yycrank+0,	0,		yyvstop+51,
yycrank+16,	0,		yyvstop+54,
yycrank+0,	0,		yyvstop+56,
yycrank+23,	0,		yyvstop+59,
yycrank+-241,	0,		yyvstop+62,
yycrank+0,	yysvec+4,	yyvstop+66,
yycrank+0,	0,		yyvstop+68,
yycrank+0,	yysvec+9,	yyvstop+70,
yycrank+0,	0,		yyvstop+72,
yycrank+0,	0,		yyvstop+74,
yycrank+0,	0,		yyvstop+76,
yycrank+0,	0,		yyvstop+78,
yycrank+0,	yysvec+13,	0,	
yycrank+0,	0,		yyvstop+80,
yycrank+0,	0,		yyvstop+82,
yycrank+22,	yysvec+15,	yyvstop+84,
yycrank+23,	0,		yyvstop+86,
yycrank+0,	0,		yyvstop+88,
yycrank+204,	0,		yyvstop+90,
yycrank+0,	yysvec+17,	yyvstop+92,
yycrank+265,	0,		0,	
yycrank+282,	0,		0,	
yycrank+0,	yysvec+44,	yyvstop+95,
yycrank+310,	yysvec+44,	0,	
yycrank+0,	0,		yyvstop+97,
yycrank+0,	0,		yyvstop+99,
yycrank+0,	0,		yyvstop+101,
yycrank+0,	0,		yyvstop+103,
yycrank+0,	0,		yyvstop+105,
yycrank+230,	0,		yyvstop+107,
yycrank+44,	0,		yyvstop+109,
yycrank+0,	0,		yyvstop+111,
yycrank+0,	yysvec+22,	0,	
yycrank+0,	0,		yyvstop+113,
yycrank+0,	0,		yyvstop+115,
yycrank+0,	0,		yyvstop+117,
yycrank+0,	yysvec+26,	yyvstop+119,
yycrank+-367,	0,		yyvstop+121,
yycrank+-2,	yysvec+60,	yyvstop+123,
yycrank+342,	0,		0,	
yycrank+0,	0,		yyvstop+126,
yycrank+2,	yysvec+43,	yyvstop+128,
yycrank+368,	0,		0,	
yycrank+391,	yysvec+44,	yyvstop+130,
yycrank+0,	0,		yyvstop+132,
yycrank+0,	yysvec+65,	yyvstop+134,
yycrank+408,	0,		yyvstop+136,
yycrank+418,	0,		yyvstop+138,
0,	0,	0};
struct yywork *yytop = yycrank+475;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,012 ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,'+' ,01  ,'+' ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'B' ,'A' ,'D' ,'D' ,'A' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,'G' ,
'X' ,'G' ,'G' ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] = {
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
#ifndef lint
static	char ncform_sccsid[] = "@(#)ncform 1.6 88/02/08 SMI"; /* from S5R2 1.2 */
#endif

int yylineno =1;
# define YYU(x) x
# define NLSTATE yyprevious=YYNEWLINE
char yytext[YYLMAX];
struct yysvf *yylstate [YYLMAX], **yylsp, **yyolsp;
char yysbuf[YYLMAX];
char *yysptr = yysbuf;
int *yyfnd;
extern struct yysvf *yyestate;
int yyprevious = YYNEWLINE;
int 
yylook (void){
	register struct yysvf *yystate, **lsp;
	register struct yywork *yyt;
	struct yysvf *yyz;
	int yych, yyfirst;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
	yyfirst=1;
	if (!yymorfg)
		yylastch = yytext;
	else {
		yymorfg=0;
		yylastch = yytext+yyleng;
		}
	for(;;){
		lsp = yylstate;
		yyestate = yystate = yybgin;
		if (yyprevious==YYNEWLINE) yystate++;
		for (;;){
# ifdef LEXDEBUG
			if(debug)fprintf(yyout,"state %d\n",yystate-yysvec-1);
# endif
			yyt = yystate->yystoff;
			if(yyt == yycrank && !yyfirst){  /* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
			yyfirst=0;
		tryagain:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"char ");
				allprint(yych);
				putchar('\n');
				}
# endif
			yyr = yyt;
			if ( (int)yyt > (int)yycrank){
				yyt = yyr + yych;
				if (yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
# ifdef YYOPTIM
			else if((int)yyt < (int)yycrank) {		/* r < yycrank */
				yyt = yyr = yycrank+(yycrank-yyt);
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"compressed state\n");
# endif
				yyt = yyt + yych;
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transitions */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				yyt = yyr + YYU(yymatch[yych]);
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"try fall back character ");
					allprint(YYU(yymatch[yych]));
					putchar('\n');
					}
# endif
				if(yyt <= yytop && yyt->verify+yysvec == yystate){
					if(yyt->advance+yysvec == YYLERR)	/* error transition */
						{unput(*--yylastch);break;}
					*lsp++ = yystate = yyt->advance+yysvec;
					goto contin;
					}
				}
			if ((yystate = yystate->yyother) && (yyt= yystate->yystoff) != yycrank){
# ifdef LEXDEBUG
				if(debug)fprintf(yyout,"fall back to state %d\n",yystate-yysvec-1);
# endif
				goto tryagain;
				}
# endif
			else
				{unput(*--yylastch);break;}
		contin:
# ifdef LEXDEBUG
			if(debug){
				fprintf(yyout,"state %d char ",yystate-yysvec-1);
				allprint(yych);
				putchar('\n');
				}
# endif
			;
			}
# ifdef LEXDEBUG
		if(debug){
			fprintf(yyout,"stopped at %d with ",*(lsp-1)-yysvec-1);
			allprint(yych);
			putchar('\n');
			}
# endif
		while (lsp-- > yylstate){
			*yylastch-- = 0;
			if (*lsp != 0 && (yyfnd= (*lsp)->yystops) && *yyfnd > 0){
				yyolsp = lsp;
				if(yyextra[*yyfnd]){		/* must backup */
					while(yyback((*lsp)->yystops,-*yyfnd) != 1 && lsp > yylstate){
						lsp--;
						unput(*yylastch--);
						}
					}
				yyprevious = YYU(*yylastch);
				yylsp = lsp;
				yyleng = yylastch-yytext+1;
				yytext[yyleng] = 0;
# ifdef LEXDEBUG
				if(debug){
					fprintf(yyout,"\nmatch ");
					sprint(yytext);
					fprintf(yyout," action %d\n",*yyfnd);
					}
# endif
				return(*yyfnd++);
				}
			unput(*yylastch);
			}
		if (yytext[0] == 0  /* && feof(yyin) */)
			{
			yysptr=yysbuf;
			return(0);
			}
		yyprevious = yytext[0] = input();
		if (yyprevious>0)
			output(yyprevious);
		yylastch=yytext;
# ifdef LEXDEBUG
		if(debug)putchar('\n');
# endif
		}
	}
int 
yyback (int *p, int m)
{
if (p==0) return(0);
while (*p)
	{
	if (*p++ == m)
		return(1);
	}
return(0);
}
	/* the following are only used in the lex library */
int 
yyinput (void){
	return(input());
	}
int 
yyoutput (int c) {
	output(c);
	}
int 
yyunput (int c) {
	unput(c);
	}
