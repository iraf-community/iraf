# include <stdio.h>
# define U(x) x
# define NLSTATE yyprevious=YYNEWLINE
# define BEGIN yybgin = yysvec + 1 +
# define INITIAL 0
# define YYLERR yysvec
# define YYSTATE (yyestate-yysvec-1)
# define YYOPTIM 1
# define YYLMAX 200
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
			{	if (isupper (type_char))
					    output (tolower (type_char));
					else
					    output (type_char);
				}
break;
case 10:
			{	if (islower (type_char))
					    output (toupper (type_char));
					else
					    output (type_char);
				}
break;
case 11:
			pass_through();
break;
case 12:
	make_float (type_char);
break;
case 13:
		do_if();
break;
case 14:
		do_else();
break;
case 15:
		do_endif();
break;
case 16:
		do_for();
break;
case 17:
		do_endfor();
break;
case 18:
		do_if();
break;
case 19:
		do_else();
break;
case 20:
		do_endif();
break;
case 21:
		do_for();
break;
case 22:
		do_endfor();
break;
case 23:
			output ('$');
break;
case 24:
			copy_comment();
break;
case 25:
			copy_string();
break;
case 26:
			ECHO;
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
			copy_line();
break;
case 31:
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
int yyvstop[] ={
0,

25,
0,

30,
0,

30,
0,

31,
0,

23,
0,

11,
0,

10,
0,

9,
0,

24,
0,

18,
0,

13,
0,

12,
0,

26,
0,

21,
0,

16,
0,

19,
0,

14,
0,

3,
0,

1,
0,

27,
0,

20,
0,

8,
0,

7,
0,

15,
0,

4,
0,

2,
0,

28,
0,

22,
0,

17,
0,

5,
0,

6,
0,

29,
0,
0};
# define YYTYPE char
struct yywork { YYTYPE verify, advance; } yycrank[] ={
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	1,3,	0,0,	
0,0,	0,0,	0,0,	0,0,	
3,3,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	1,3,	0,0,	1,4,	
1,5,	1,6,	2,14,	3,3,	
2,15,	0,0,	0,0,	3,16,	
7,28,	0,0,	0,0,	0,0,	
1,7,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	1,8,	
1,8,	1,8,	1,8,	6,17,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	0,0,	0,0,	
0,0,	0,0,	6,18,	30,49,	
0,0,	0,0,	1,9,	10,31,	
22,43,	34,53,	37,41,	21,41,	
9,30,	1,10,	13,34,	20,40,	
1,11,	1,12,	19,38,	21,42,	
19,39,	1,13,	29,48,	12,33,	
6,19,	6,20,	11,32,	31,50,	
6,21,	8,29,	16,19,	16,20,	
32,51,	33,52,	16,37,	6,22,	
14,35,	26,47,	38,58,	6,23,	
14,36,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	8,8,	
8,8,	8,8,	8,8,	24,44,	
25,46,	24,45,	29,48,	39,59,	
6,24,	6,25,	40,60,	35,54,	
6,26,	35,55,	16,24,	16,25,	
36,56,	42,61,	16,26,	43,62,	
44,63,	45,64,	46,65,	6,27,	
36,57,	49,66,	50,67,	51,68,	
52,69,	53,70,	54,71,	55,72,	
57,73,	58,74,	59,75,	61,77,	
62,78,	59,76,	63,79,	64,80,	
66,82,	67,83,	64,81,	68,84,	
69,85,	70,86,	71,87,	72,88,	
73,89,	75,90,	76,91,	77,92,	
78,93,	80,94,	81,95,	82,96,	
84,97,	85,98,	86,99,	88,100,	
82,96,	89,101,	90,102,	82,96,	
94,103,	97,104,	98,105,	101,106,	
104,107,	82,96,	82,96,	105,108,	
106,109,	0,0,	0,0,	82,96,	
0,0};
struct yysvf yysvec[] ={
0,	0,	0,
yycrank+1,	0,		0,	
yycrank+3,	yysvec+1,	0,	
yycrank+7,	0,		0,	
yycrank+0,	0,		yyvstop+1,
yycrank+0,	0,		yyvstop+3,
yycrank+23,	0,		0,	
yycrank+2,	0,		0,	
yycrank+61,	0,		0,	
yycrank+2,	0,		0,	
yycrank+2,	0,		0,	
yycrank+4,	0,		0,	
yycrank+2,	0,		0,	
yycrank+2,	0,		0,	
yycrank+3,	0,		yyvstop+5,
yycrank+0,	0,		yyvstop+7,
yycrank+29,	0,		0,	
yycrank+0,	0,		yyvstop+9,
yycrank+0,	0,		yyvstop+11,
yycrank+10,	0,		0,	
yycrank+4,	0,		0,	
yycrank+9,	0,		0,	
yycrank+3,	0,		0,	
yycrank+0,	0,		yyvstop+13,
yycrank+11,	0,		0,	
yycrank+9,	0,		0,	
yycrank+3,	0,		0,	
yycrank+0,	0,		yyvstop+15,
yycrank+0,	0,		yyvstop+17,
yycrank+20,	0,		0,	
yycrank+3,	0,		0,	
yycrank+7,	0,		0,	
yycrank+5,	0,		0,	
yycrank+6,	0,		0,	
yycrank+4,	0,		0,	
yycrank+19,	0,		0,	
yycrank+30,	0,		0,	
yycrank+8,	0,		0,	
yycrank+23,	0,		0,	
yycrank+55,	0,		0,	
yycrank+44,	0,		0,	
yycrank+0,	0,		yyvstop+19,
yycrank+65,	0,		0,	
yycrank+47,	0,		0,	
yycrank+21,	0,		0,	
yycrank+37,	0,		0,	
yycrank+24,	0,		0,	
yycrank+0,	0,		yyvstop+21,
yycrank+0,	0,		yyvstop+23,
yycrank+72,	0,		0,	
yycrank+73,	0,		0,	
yycrank+63,	0,		0,	
yycrank+64,	0,		0,	
yycrank+57,	0,		0,	
yycrank+31,	0,		0,	
yycrank+47,	0,		0,	
yycrank+0,	0,		yyvstop+25,
yycrank+49,	0,		0,	
yycrank+80,	0,		0,	
yycrank+80,	0,		0,	
yycrank+0,	0,		yyvstop+27,
yycrank+82,	0,		0,	
yycrank+83,	0,		0,	
yycrank+53,	0,		0,	
yycrank+53,	0,		0,	
yycrank+0,	0,		yyvstop+29,
yycrank+86,	0,		0,	
yycrank+81,	0,		0,	
yycrank+86,	0,		0,	
yycrank+87,	0,		0,	
yycrank+92,	0,		0,	
yycrank+61,	0,		0,	
yycrank+58,	0,		0,	
yycrank+56,	0,		0,	
yycrank+0,	0,		yyvstop+31,
yycrank+86,	0,		0,	
yycrank+96,	0,		0,	
yycrank+97,	0,		0,	
yycrank+92,	0,		0,	
yycrank+0,	0,		yyvstop+33,
yycrank+58,	0,		0,	
yycrank+68,	0,		0,	
yycrank+103,	0,		yyvstop+35,
yycrank+0,	0,		yyvstop+37,
yycrank+84,	0,		0,	
yycrank+85,	0,		0,	
yycrank+98,	0,		0,	
yycrank+0,	0,		yyvstop+39,
yycrank+73,	0,		0,	
yycrank+60,	0,		0,	
yycrank+96,	0,		0,	
yycrank+0,	0,		yyvstop+41,
yycrank+0,	0,		yyvstop+43,
yycrank+0,	0,		yyvstop+45,
yycrank+66,	0,		0,	
yycrank+0,	0,		yyvstop+47,
yycrank+0,	0,		yyvstop+49,
yycrank+112,	0,		0,	
yycrank+113,	0,		0,	
yycrank+0,	0,		yyvstop+51,
yycrank+0,	0,		yyvstop+53,
yycrank+83,	0,		0,	
yycrank+0,	0,		yyvstop+55,
yycrank+0,	0,		yyvstop+57,
yycrank+108,	0,		0,	
yycrank+111,	0,		0,	
yycrank+87,	0,		0,	
yycrank+0,	0,		yyvstop+59,
yycrank+0,	0,		yyvstop+61,
yycrank+0,	0,		yyvstop+63,
0,	0,	0};
struct yywork *yytop = yycrank+191;
struct yysvf *yybgin = yysvec+1;
char yymatch[] ={
00  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,011 ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
011 ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,'0' ,
'0' ,'0' ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
01  ,01  ,01  ,01  ,01  ,01  ,01  ,01  ,
0};
char yyextra[] ={
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0,0,0,0,0,0,0,0,
0};
/*	ncform	4.1	83/08/11	*/

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
	int yych;
	struct yywork *yyr;
# ifdef LEXDEBUG
	int debug;
# endif
	char *yylastch;
	/* start off machines */
# ifdef LEXDEBUG
	debug = 0;
# endif
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
			if(yyt == yycrank){		/* may not be any transitions */
				yyz = yystate->yyother;
				if(yyz == 0)break;
				if(yyz->yystoff == yycrank)break;
				}
			*yylastch++ = yych = input();
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
