/int nstr; extern int yyprevious;/a\
if (yyin==NULL) yyin = stdin;\
if (yyout==NULL) yyout = stdout;
/{stdin}/c\
FILE *yyin, *yyout;
s/"stdio.h"/<stdio.h>/
s/YYLMAX 200/YYLMAX 8192/
s/static int input/int input/g
s/static void yyunput/void yyunput/g
