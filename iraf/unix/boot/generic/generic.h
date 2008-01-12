#ifndef _GENERIC_H
#define _GENERIC_H

#include <stdio.h>

/* lexyy.c */
extern char yytext[];
extern int  yyleng;
extern FILE *yyin;
extern FILE *yyout;
extern int yylex( void );
extern int lex_input( void );
extern void lex_unput ( int );
/* chario.c */
extern int k_fseek ( FILE *, long, int );
extern int k_fclose ( FILE * );
extern FILE *k_fopen ( const char *, const char * );
extern long k_ftell ( FILE * );
extern int k_getc ( FILE * );
/* generic.c */
extern const char *type_string;
extern char xtype_string[];
extern char type_char;
extern void outstr ( const char * );
extern void output_indef ( char );
extern void output_upper ( const char * );
extern void pass_through( void );
extern void output ( char );
extern void make_float ( char );
extern void do_if ( void );
extern void do_else ( void );
extern void do_endif ( void );
extern void do_for ( void );
extern void do_endfor ( void );
extern void copy_comment ( void );
extern void copy_string ( void );
extern void copy_line ( void );
/* yywrap.c */
extern int yywrap ( void );

#endif	/* _GENERIC_H */
