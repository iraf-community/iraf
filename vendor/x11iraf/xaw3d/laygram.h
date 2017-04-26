#ifndef BISON_Y_TAB_H
# define BISON_Y_TAB_H

#ifndef YYSTYPE
typedef union {
    int		    ival;
    XrmQuark	    qval;
    BoxPtr	    bval;
    BoxParamsPtr    pval;
    GlueRec	    gval;
    LayoutDirection lval;
    ExprPtr	    eval;
    Operator	    oval;
} LayYYstype;
# define YYSTYPE LayYYstype
# define YYSTYPE_IS_TRIVIAL 1
#endif
# define	OC	257
# define	CC	258
# define	OA	259
# define	CA	260
# define	OP	261
# define	CP	262
# define	NAME	263
# define	NUMBER	264
# define	INFINITY	265
# define	VERTICAL	266
# define	HORIZONTAL	267
# define	EQUAL	268
# define	DOLLAR	269
# define	PLUS	270
# define	MINUS	271
# define	TIMES	272
# define	DIVIDE	273
# define	PERCENTOF	274
# define	PERCENT	275
# define	WIDTH	276
# define	HEIGHT	277
# define	UMINUS	278
# define	UPLUS	279


extern YYSTYPE LayYYlval;

#endif /* not BISON_Y_TAB_H */
