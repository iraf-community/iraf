/* A Bison parser, made by GNU Bison 2.3.  */

/* Skeleton interface for Bison's Yacc-like parsers in C

   Copyright (C) 1984, 1989, 1990, 2000, 2001, 2002, 2003, 2004, 2005, 2006
   Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     Y_SCAN = 258,
     Y_SCANF = 259,
     Y_FSCAN = 260,
     Y_FSCANF = 261,
     Y_OSESC = 262,
     Y_APPEND = 263,
     Y_ALLAPPEND = 264,
     Y_ALLREDIR = 265,
     Y_GSREDIR = 266,
     Y_ALLPIPE = 267,
     D_D = 268,
     D_PEEK = 269,
     Y_NEWLINE = 270,
     Y_CONSTANT = 271,
     Y_IDENT = 272,
     Y_WHILE = 273,
     Y_IF = 274,
     Y_ELSE = 275,
     Y_FOR = 276,
     Y_BREAK = 277,
     Y_NEXT = 278,
     Y_SWITCH = 279,
     Y_CASE = 280,
     Y_DEFAULT = 281,
     Y_RETURN = 282,
     Y_GOTO = 283,
     Y_PROCEDURE = 284,
     Y_BEGIN = 285,
     Y_END = 286,
     Y_BOOL = 287,
     Y_INT = 288,
     Y_REAL = 289,
     Y_STRING = 290,
     Y_FILE = 291,
     Y_STRUCT = 292,
     Y_GCUR = 293,
     Y_IMCUR = 294,
     Y_UKEY = 295,
     Y_PSET = 296,
     YOP_AOCAT = 297,
     YOP_AODIV = 298,
     YOP_AOMUL = 299,
     YOP_AOSUB = 300,
     YOP_AOADD = 301,
     YOP_OR = 302,
     YOP_AND = 303,
     YOP_NE = 304,
     YOP_EQ = 305,
     YOP_GE = 306,
     YOP_LE = 307,
     YOP_CONCAT = 308,
     UMINUS = 309,
     YOP_NOT = 310,
     YOP_POW = 311
   };
#endif
/* Tokens.  */
#define Y_SCAN 258
#define Y_SCANF 259
#define Y_FSCAN 260
#define Y_FSCANF 261
#define Y_OSESC 262
#define Y_APPEND 263
#define Y_ALLAPPEND 264
#define Y_ALLREDIR 265
#define Y_GSREDIR 266
#define Y_ALLPIPE 267
#define D_D 268
#define D_PEEK 269
#define Y_NEWLINE 270
#define Y_CONSTANT 271
#define Y_IDENT 272
#define Y_WHILE 273
#define Y_IF 274
#define Y_ELSE 275
#define Y_FOR 276
#define Y_BREAK 277
#define Y_NEXT 278
#define Y_SWITCH 279
#define Y_CASE 280
#define Y_DEFAULT 281
#define Y_RETURN 282
#define Y_GOTO 283
#define Y_PROCEDURE 284
#define Y_BEGIN 285
#define Y_END 286
#define Y_BOOL 287
#define Y_INT 288
#define Y_REAL 289
#define Y_STRING 290
#define Y_FILE 291
#define Y_STRUCT 292
#define Y_GCUR 293
#define Y_IMCUR 294
#define Y_UKEY 295
#define Y_PSET 296
#define YOP_AOCAT 297
#define YOP_AODIV 298
#define YOP_AOMUL 299
#define YOP_AOSUB 300
#define YOP_AOADD 301
#define YOP_OR 302
#define YOP_AND 303
#define YOP_NE 304
#define YOP_EQ 305
#define YOP_GE 306
#define YOP_LE 307
#define YOP_CONCAT 308
#define UMINUS 309
#define YOP_NOT 310
#define YOP_POW 311




#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
# define YYSTYPE_IS_TRIVIAL 1
#endif

extern YYSTYPE yylval;

