



define	ARB		999999999
define	ERR		-1
define	EOF		-2
define	BOF		-3
define	EOT		-4
define	BOFL		BOF
define	EOFL		EOF
define	EOS		0
define	NO		0
define	YES		1
define	OK		0
define	NULL		0


define	READ_ONLY	1		
define	READ_WRITE	2
define	WRITE_ONLY	3
define	APPEND		4
define	NEW_FILE	5
define	TEMP_FILE	6
define	NEW_COPY	7
define	NEW_IMAGE	5		
define	NEW_STRUCT	5
define	NEW_TAPE	5
define	TEXT_FILE	11		
define	BINARY_FILE	12
define	DIRECTORY_FILE	13
define	STATIC_FILE	14
define	SPOOL_FILE	(-2)
define	RANDOM		1		
define	SEQUENTIAL	2
define	CLIN		1		
define	CLOUT		2		
define	STDIN		3		
define	STDOUT		4
define	STDERR		5
define	STDGRAPH	6
define	STDIMAGE	7
define	STDPLOT		8



define	SZ_BOOL		2		
define	SZ_CHAR		1
define	SZ_SHORT	1
define	SZ_INT		2
define	SZ_LONG		2
define	SZ_REAL		2
define	SZ_DOUBLE	4
define	SZ_COMPLEX	4
define	SZ_POINTER	2
define	SZ_STRUCT	2
define	SZ_USHORT	1
define	SZ_FNAME	255		
define	SZ_PATHNAME	511		
define	SZ_LINE		1023		
define	SZ_COMMAND	2047		

define  SZ_MII_SHORT    1		
define  SZ_MII_LONG     2
define  SZ_MII_REAL     2
define  SZ_MII_DOUBLE   4
define  SZ_MII_INT      SZ_MII_LONG

define	SZ_INT32	2		
define	SZ_LONG32	2
define	SZ_STRUCT32	2

define	TY_BOOL		1		
define	TY_CHAR		2
define	TY_SHORT	3
define	TY_INT		4
define	TY_LONG		5
define	TY_REAL		6
define	TY_DOUBLE	7
define	TY_COMPLEX	8
define	TY_POINTER	9
define	TY_STRUCT	10		
define	TY_USHORT	11		
define	TY_UBYTE	12		


define	INDEFS		(-32767)
define	INDEFL		(-2147483647)
define	INDEFI		INDEFL
define	INDEFR		1.6e38
define	INDEFD		1.6d308
define	INDEFX		(INDEF,INDEF)
define	INDEF		INDEFR

define	IS_INDEFS	(($1)==INDEFS)
define	IS_INDEFL	(($1)==INDEFL)
define	IS_INDEFI	(($1)==INDEFI)
define	IS_INDEFR	(($1)==INDEFR)
define	IS_INDEFD	(($1)==INDEFD)
define	IS_INDEFX	(real($1)==INDEFR)
define	IS_INDEF	(($1)==INDEFR)


define	P2C	((($1)-1)*2+1)
define	P2S	((($1)-1)*2+1)
define	P2L	($1)
define	P2R	($1)
define	P2D	((($1)-1)/2+1)
define	P2X	((($1)-1)/2+1)

define	P2P	($1)			












define	access	xfaccs
define	calloc	xcallc
define	close	xfcloe
define	delete	xfdele
define	error	xerror
define	flush	xffluh
define	getc	xfgetc
define	getchar	xfgetr
define	malloc	xmallc
define	mfree	xmfree
define	mktemp	xmktep
define	note	xfnote
define	open	xfopen
define	poll	xfpoll
define	printf	xprinf
define	putc	xfputc
define	putchar	xfputr
define	qsort	xqsort
define	read	xfread
define	realloc	xrealc
define	seek	xfseek
define	sizeof	xsizef
define	strcat	xstrct
define	strcmp	xstrcp
define	strcpy	xstrcy
define	strlen	xstrln
define	ungetc	xfungc
define	write	xfwrie
define  fatal   xfatal
define  fchdir  xfchdr
define  fscan   xfscan
define  getopt  xgtopt
define  getpid  xgtpid
define  getuid  xgtuid
define  rename  xfrnam
define  reset   xreset
define  scan    xxscan






define	IS_UPPER	($1>=65&$1<=90)
define	IS_LOWER	($1>=97&$1<=122)
define	IS_DIGIT	($1>=48&$1<=57)
define	IS_PRINT	($1>=32&$1<127)
define	IS_CNTRL	($1>0&$1<32)
define	IS_ASCII	($1>0&$1<=127)
define	IS_ALPHA	(IS_UPPER($1)|IS_LOWER($1))
define	IS_ALNUM	(IS_ALPHA($1)|IS_DIGIT($1))
define	IS_WHITE	($1==32|$1==9)
define	TO_UPPER	($1+65-97)
define	TO_LOWER	($1+97-65)
define	TO_INTEG	($1-48)
define	TO_DIGIT	($1+48)

#!# 2
















 
x$subr t_hello ()

x$short	ST0001(14)
save
x$int	iyy
data	(ST0001(iyy),iyy= 1, 8)	/104,101,108,108,111, 44, 32,119/
data	(ST0001(iyy),iyy= 9,14)	/111,114,108,100, 10, 0/
begin
#!# 10

	call printf (ST0001)
end


