/int nstr; extern int yyprevious;/a\
if (yyin==NULL) yyin = stdin;\
if (yyout==NULL) yyout = stdout;
/{stdin}/c\
FILE *yyin, *yyout;
s/"stdio.h"/<stdio.h>/
s/getc/k_getc/
