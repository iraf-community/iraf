# include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX BUFSIZ
# define output(c) putc(c,yyout)
# define input() (((yytchar=yysptr>yysbuf?U(*--yysptr):k_getc(yyin))==10?(yylineno++,yytchar):yytchar)==EOF?0:yytchar)
# define unput(c) {yytchar= (c);if(yytchar=='\n')yylineno--;*yysptr++=yytchar;}
# define yymore() (yymorfg=1)
# define ECHO fprintf(yyout, "%s",yytext)
# define REJECT { nstr = yyreject(); goto yyfussy;}
int yyleng; extern char yytext[];
int yymorfg;
extern char *yysptr, yysbuf[];
int yytchar;
FILE *yyin, *yyout;
extern int yylineno;
struct yysvf { 
	struct yywork *yystoff;
	struct yysvf *yyother;
	int *yystops;};
struct yysvf *yyestate;
extern struct yysvf yysvec[], *yybgin;

#include <ctype.h>

/*
 * GENERIC -- This filter takes a file containing a generic operator as input
 * and generates as output either a set of files, one for each of the data
 * types in the generic family, or a single file wherein the generic section
 * has been duplicated for each case.
 */

#undef	output
extern	char	*type_string;
extern	char	xtype_string[];
extern	char	type_char;

# define YYNEWLINE 10
yylex(){
int nstr; extern int yyprevious;
if (yyin==NULL) yyin = stdin;
if (yyout==NULL) yyout = stdout;
while((nstr = yylook()) >= 0)
yyfussy: switch(nstr){
case 0:
if(yywrap()) return(0); break;
case 1:
			outstr (type_string);
break;
case 2:
			outstr (xtype_string);
break;
case 3:
			output_indef (type_char);
break;
case 4:
	ECHO;
break;
case 5:
		output_upper ("SZ_");
break;
case 6:
		output_upper ("TY_");
break;
case 7:
			outstr ("PIXEL");
break;
case 8:
			outstr ("INDEF");
break;
case 9:
	{
					yytext[strlen(yytext)-5] = '\0';
					output_upper (yytext);
				}
break;
case 10:
			{	if (isupper (type_char))
					    output (tolower (type_char));
					else
					    output (type_char);
				}
break;
case 11:
			{	if (islower (type_char))
					    output (toupper (type_char));
					else
					    output (type_char);
				}
break;
case 12:
			pass_through();
break;
case 13:
	make_float (type_char);
break;
case 14:
		do_if();
break;
case 15:
		do_else();
break;
case 16:
		do_endif();
break;
case 17:
		do_for();
break;
case 18:
		do_endfor();
break;
case 19:
		do_if();
break;
case 20:
		do_else();
break;
case 21:
		do_endif();
break;
case 22:
		do_for();
break;
case 23:
		do_endfor();
break;
case 24:
			output ('$');
break;
case 25:
			copy_comment();
break;
case 26:
			copy_string();
break;
case 27:
			ECHO;
break;
case 28:
			ECHO;
break;
case 29:
		ECHO;
break;
case 30:
		ECHO;
break;
case 31:
			copy_line();
break;
case 32:
			copy_line();
break;
case -1:
break;
default:
fprintf(yyout,"bad switch yylook %d",nstr);
} return(0); }
/* end of yylex */


/* LEX_INPUT -- Make input() callable as a function from the .c code.
 */
lex_input()
{
	return (input());
}


/* LEX_UNPUT -- Make unput() callable as a function from the .c code.
 */
lex_unput (ch)
int	ch;
{
	unput (ch);
}
int yyvstop[] = {
0,

26,
0,

31,
0,

31,
0,

32,
0,

24,
0,

12,
0,

11,
0,

10,
0,

25,
0,

19,
0,

14,
0,

13,
0,

27,
0,

22,
0,

17,
0,

20,
0,

15,
0,

3,
0,

1,
0,

28,
0,

21,
0,

8,
0,

7,
0,

16,
0,

9,
0,

4,
0,

2,
9,
0,

29,
0,

23,
0,

18,
0,

5,
9,
0,

6,
9,
0,

30,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] = {
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
3,3,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,3,	0,0,	1,4,	
1,5,	1,6,	2,15,	3,3,	
2,16,	0,0,	0,0,	3,17,	
7,29,	0,0,	0,0,	0,0,	
1,7,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,9,	1,9,	
1,9,	1,9,	1,9,	1,9,	
1,9,	1,9,	1,10,	1,9,	
1,9,	1,9,	1,9,	1,9,	
1,9,	1,11,	1,9,	1,9,	
1,12,	1,13,	1,9,	1,9,	
1,9,	1,14,	1,9,	1,9,	
6,18,	8,30,	10,32,	11,33,	
12,34,	13,35,	14,36,	20,40,	
21,42,	20,41,	23,45,	6,19,	
27,49,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	15,37,	
17,20,	17,21,	26,48,	15,38,	
17,39,	25,46,	22,43,	25,47,	
30,50,	6,20,	6,21,	31,51,	
32,52,	6,22,	22,44,	33,53,	
34,54,	35,55,	36,56,	37,57,	
6,23,	37,58,	38,59,	39,43,	
6,24,	40,61,	41,62,	42,63,	
44,64,	45,65,	38,60,	46,66,	
17,25,	17,26,	47,67,	48,68,	
17,27,	51,69,	52,70,	53,71,	
30,50,	6,25,	6,26,	54,72,	
55,73,	6,27,	56,74,	57,75,	
58,76,	60,77,	61,78,	62,79,	
64,81,	65,82,	62,80,	66,83,	
6,28,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	9,9,	
9,31,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	9,9,	
9,9,	9,9,	9,9,	67,84,	
69,86,	70,87,	67,85,	9,9,	
71,88,	72,89,	73,90,	74,91,	
75,92,	76,93,	77,94,	79,95,	
80,96,	81,97,	82,98,	84,99,	
85,100,	86,101,	87,102,	89,103,	
90,104,	91,105,	93,106,	87,102,	
94,107,	95,108,	87,102,	99,109,	
103,110,	104,111,	107,112,	110,113,	
87,102,	87,102,	111,114,	112,115,	
0,0,	0,0,	87,102,	0,0,	
0,0};
struct yysvf yysvec[] = {
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+3,	yysvec+1,	0,	
yycrank+7,	0,		0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+56,	0,		0,	
yycrank+2,	0,		0,	
yycrank+57,	0,		0,	
yycrank+108,	0,		0,	
yycrank+16,	yysvec+9,	0,	
yycrank+22,	yysvec+9,	0,	
yycrank+6,	yysvec+9,	0,	
yycrank+8,	yysvec+9,	0,	
yycrank+18,	yysvec+9,	0,	
yycrank+14,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+7,
yycrank+47,	0,		0,	
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+11,
yycrank+23,	0,		0,	
yycrank+21,	0,		0,	
yycrank+52,	0,		0,	
yycrank+29,	0,		0,	
yycrank+0,	0,		yyvstop+13,
yycrank+13,	0,		0,	
yycrank+7,	0,		0,	
yycrank+2,	0,		0,	
yycrank+0,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+17,
yycrank+54,	0,		0,	
yycrank+54,	yysvec+9,	0,	
yycrank+60,	yysvec+9,	0,	
yycrank+43,	yysvec+9,	0,	
yycrank+37,	yysvec+9,	0,	
yycrank+38,	yysvec+9,	0,	
yycrank+61,	yysvec+9,	0,	
yycrank+27,	0,		0,	
yycrank+36,	0,		0,	
yycrank+69,	0,		0,	
yycrank+58,	0,		0,	
yycrank+74,	0,		0,	
yycrank+61,	0,		0,	
yycrank+0,	0,		yyvstop+19,
yycrank+76,	0,		0,	
yycrank+57,	0,		0,	
yycrank+32,	0,		0,	
yycrank+50,	0,		0,	
yycrank+37,	0,		0,	
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+65,	yysvec+9,	0,	
yycrank+85,	yysvec+9,	0,	
yycrank+86,	yysvec+9,	0,	
yycrank+79,	yysvec+9,	0,	
yycrank+80,	yysvec+9,	0,	
yycrank+74,	yysvec+9,	0,	
yycrank+48,	0,		0,	
yycrank+64,	0,		0,	
yycrank+0,	0,		yyvstop+25,
yycrank+66,	0,		0,	
yycrank+97,	0,		0,	
yycrank+97,	0,		0,	
yycrank+0,	0,		yyvstop+27,
yycrank+99,	0,		0,	
yycrank+100,	0,		0,	
yycrank+70,	0,		0,	
yycrank+97,	0,		0,	
yycrank+0,	0,		yyvstop+29,
yycrank+131,	yysvec+9,	0,	
yycrank+131,	yysvec+9,	0,	
yycrank+128,	yysvec+9,	0,	
yycrank+132,	yysvec+9,	0,	
yycrank+133,	yysvec+9,	0,	
yycrank+138,	yysvec+9,	0,	
yycrank+107,	0,		0,	
yycrank+104,	0,		0,	
yycrank+102,	0,		0,	
yycrank+0,	0,		yyvstop+31,
yycrank+132,	0,		0,	
yycrank+142,	0,		0,	
yycrank+143,	0,		0,	
yycrank+138,	0,		0,	
yycrank+0,	0,		yyvstop+33,
yycrank+104,	0,		0,	
yycrank+114,	0,		0,	
yycrank+141,	yysvec+9,	0,	
yycrank+150,	yysvec+9,	yyvstop+35,
yycrank+0,	yysvec+9,	yyvstop+37,
yycrank+131,	yysvec+9,	0,	
yycrank+132,	yysvec+9,	0,	
yycrank+145,	yysvec+9,	0,	
yycrank+0,	0,		yyvstop+39,
yycrank+120,	0,		0,	
yycrank+107,	0,		0,	
yycrank+143,	0,		0,	
yycrank+0,	0,		yyvstop+41,
yycrank+0,	0,		yyvstop+43,
yycrank+0,	0,		yyvstop+45,
yycrank+113,	0,		0,	
yycrank+0,	0,		yyvstop+47,
yycrank+0,	yysvec+9,	yyvstop+49,
yycrank+0,	yysvec+9,	yyvstop+51,
yycrank+159,	yysvec+9,	0,	
yycrank+160,	yysvec+9,	0,	
yycrank+0,	yysvec+9,	yyvstop+53,
yycrank+0,	0,		yyvstop+56,
yycrank+130,	0,		0,	
yycrank+0,	0,		yyvstop+58,
yycrank+0,	0,		yyvstop+60,
yycrank+155,	yysvec+9,	0,	
yycrank+158,	yysvec+9,	0,	
yycrank+134,	0,		0,	
yycrank+0,	yysvec+9,	yyvstop+62,
yycrank+0,	yysvec+9,	yyvstop+65,
yycrank+0,	0,		yyvstop+68,
0,	0,	0};
struct yywork *yytop = yycrank+238;
struct yysvf *yybgin = yysvec+1;
char yymatch[] = {
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,'A' ,
'A' ,'A' ,'A' ,01  ,01  ,01  ,01  ,'_' ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
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
yylook(){
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
yyback(p, m)
	int *p;
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
yyinput(){
	return(input());
	}
yyoutput(c)
  int c; {
	output(c);
	}
yyunput(c)
   int c; {
	unput(c);
	}
