$! Fix the lexyy.c file  (see lex.sed)   [VMS]
$!
$ open/write fp lex_fix.com
$ write fp "$ edit/edt/nocommand lexyy.c"
$ write fp "sub/getc(yyin)/yy_getc(yyin)/w"
$ write fp "sub/yylex/lex_yylex/w"
$ write fp "sub/YYLMAX 200/YYLMAX 2048/w"
$ write fp "exit"
$ write fp "$ exit"
$ close fp
$ @lex_fix.com
$ delete lex_fix.com;*
