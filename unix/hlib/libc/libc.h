/*
 * LIBC.H -- Definitions which should be included by all C source files which
 * use the IRAF runtime C library.
 */

#define	XCHAR		short
#define	XINT		int
#define	SZ_DEFIOBUF	1024
#define	FIO_MAXFD	128

#define	FIOCOM		fiocom_		/* [MACHDEP] */
#define	MEMCOM		mem_
#define	XERPSH		xerpsh_
#define	XERPOP		xerpop_
#define	c_main		cmain_

/* Error handling.
 */
#define	iferr(stmt)	{XERPSH();stmt;}if(XERPOP())

/* SPP/C pointer conversions.
 */
extern	char		MEMCOM[];
#define	Memc		(((XCHAR *)MEMCOM)-1)
#define	Memi		(((XINT *)MEMCOM)-1)
#define	Memcptr(addr)	((XCHAR *)(addr) - Memc + 1)
#define	Memiptr(addr)	((XINT *)(addr) - Memi + 1)

/* External names.
 */
#ifndef NOLIBCNAMES

#define	getenv		envget
#define	sys_nerr	u_sysnerr
#define	sys_errlist	u_syserrlist

#define	atof		u_atof
#define	atoi		u_atoi
#define	atol		u_atol
#define	calloc		u_calloc
#define	envget		u_envget
#define	eprintf		u_eprintf
#define	fclose		u_fclose
#define	fdopen		u_fdopen
#define	fflush		u_fflush
#define	fgetc		u_fgetc
#define	fgets		u_fgets
#define	fopen		u_fopen
#define	fprintf		u_fprintf
#define	fputc		u_fputc
#define	fputs		u_fputs
#define	fread		u_fread
#define	free		u_free
#define	freopen		u_freopen
#define	fscanf		u_fscanf
#define	fseek		u_fseek
#define	ftell		u_ftell
#define	fwrite		u_fwrite
#define	gets		u_gets
#define	getw		u_getw
#define	index		u_index
#define	isatty		u_isatty
#define	malloc		u_malloc
#define	mktemp		u_mktemp
#define	perror		u_perror
#define	printf		u_printf
#define	puts		u_puts
#define	putw		u_putw
#define	qsort		u_qsort
#define	realloc		u_realloc
#define	rewind		u_rewind
#define	rindex		u_rindex
#define	scanf		u_scanf
#define	setbuf		u_setbuf
#define	setbuffer	u_setfbf		/* collision	*/
#define	setlinebuf	u_setlinebuf
#define	sprintf		u_sprintf
#define	sscanf		u_sscanf
#define	strcat		u_strcat
#define	strchr		u_index
#define	strcmp		u_strcmp
#define	strcpy		u_strcpy
#define	strlen		u_strlen
#define	strncat		u_strnt			/* collision	*/
#define	strncmp		u_strnp			/* collision	*/
#define	strncpy		u_strny			/* collision	*/
#define	strrchr		u_rindex
#define	system		u_system
#define	ungetc		u_ungetc

/* C_SPP names not unique in the first seven characters.
 */
#define	c_envgetb	c_envgb
#define	c_envgeti	c_envgi
#define	c_envgets	c_envgs
#define	c_ttyclear	c_ttycr
#define	c_ttyclearln	c_ttycn
#define	c_ttygetb	c_ttygb
#define	c_ttygeti	c_ttygi
#define	c_ttygetr	c_ttygr
#define	c_ttygets	c_ttygs
#define	c_ttyputline	c_ttype
#define	c_ttyputs	c_ttyps
#define c_ungetc	c_ungec
#define c_ungetstr	c_unges

#endif

#define	D_libc
