/*
 * U_STDIO.H -- C defines used to emulate the UNIX standard i/o package upon
 * IRAF file i/o.  These definitions are portable, but highly dependent on
 * the guts of IRAF FIO.
 */

#ifndef D_stdio
#define	D_stdio

#include <iraf/libc.h>
#include <iraf/stdarg.h>
#include <iraf/spp.h>

#define	FIOCOM		fiocom_		/* [MACHDEP] */

#define	SZ_DEFIOBUF	1024
#define	FIO_MAXFD	4096
#define	IRAF_FILE	struct _iobuf

#define	IRAF_BUFSIZ	SZ_DEFIOBUF
#define	_NFILE		FIO_MAXFD

/* Filler space is defined here to reduce clutter in the struct def below.
 */
#define	_F1		_filler1[_NFILE-1]
#define	_F2		_filler2[_NFILE-1]
#define	_F3		_filler3[_NFILE-1]
#define	_F4		_filler4[_NFILE-1]
#define	_F5		_filler5[_NFILE-1]
#define	_F6		_filler6[_NFILE-1]
#define	_F7		_filler7[_NFILE-1]

/* The _iobuf structure is the C version of the FIO common /fiocom/, which
 * contains all the FIO buffer and i/o pointers.  Each structure field is
 * maintained in the common as an array of length _NFILE, hence in terms of
 * C the structures are interleaved.  The file pointers are indices into
 * the array Memc, an array of XCHAR.
 */
struct _iobuf {
	XLONG	_boffset, _F1;		/* XCHAR file offset of buffer	*/
	XINT	_bufptr,  _F2;		/* buffer pointer		*/
	XINT	_buftop,  _F3;		/* pointer to top of buffer + 1	*/
	XINT	_iop,     _F4;		/* pointer to next XCHAR	*/
	XINT	_itop,    _F5;		/* call filbuf when _iop >=	*/
	XINT	_otop,    _F6;		/* call flsbuf when _iop >=	*/
	XINT	_fiodes,  _F7;		/* FIO file descriptor		*/
	XINT	_fflags;		/* bit flags			*/
};

extern	XLONG FIOCOM[];			/* the FIO common		*/

#define	_FFLUSHNL	01		/* flush buffer on newline	*/
#define	_FREAD		02		/* read perm on file		*/
#define	_FWRITE		04		/* read perm on file		*/
#define	_FEOF		010		/* file positioned to EOF	*/
#define	_FERR		020		/* i/o error on file		*/
#define _FKEEP		040		/* keep file open at exit	*/
#define	_FFLUSH		0100		/* write to device on newline	*/
#define	_FRAW		0200		/* raw input mode		*/
#define	_FNDELAY	0400		/* nonblocking i/o		*/
#define	_FPUSH		01000		/* data is pushed back		*/
#define	_FIPC		02000		/* file is an IPC channel	*/

/* Convert FILE pointers to and from FIO integer file descriptors.
 */
#define	FDTOFP(fd)	((IRAF_FILE *)(&FIOCOM[(fd)-1]))
#define	FPTOFD(fp)	((XLONG *)(fp) - FIOCOM + 1)

#define	eprintf		u_eprintf

/* ../../../sys/libc/???.c */
/* cfilbuf.c */
extern int c_filbuf ( IRAF_FILE * );
/* cflsbuf.c */
extern int c_flsbuf ( unsigned int, IRAF_FILE * );
/* printf.c */
extern int u_doprnt ( const char *, va_list *, IRAF_FILE * );

#ifdef USE_STDARG
extern int eprintf ( const char *format, ... );
#else
extern int eprintf ();
#endif

/* Symbols defined in the standard libc. 
 */
#ifndef NOLIBCNAMES
#define	D_stdio_libcnames

#ifdef	D_kernel
#error "Do not include iraf/stdio.h without NOLIBCNAMES when using iraf/kernel.h."
#endif

#define	FILE		IRAF_FILE
#define	BUFSIZ		IRAF_BUFSIZ

# ifndef EOF
#define	EOF		(-1)
# endif

#define	stdin		(FDTOFP(3))
#define	stdout		(FDTOFP(4))
#define	stderr		(FDTOFP(5))

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
#define	freopen		u_freopen
#define	fscanf		u_fscanf
#define	fseek		u_fseek
#define	ftell		u_ftell
#define	fwrite		u_fwrite
#define	gets		u_gets
#define	getw		u_getw
#define	perror		u_perror
#define	printf		u_printf
#define	puts		u_puts
#define	putw		u_putw
#define	rewind		u_rewind
#define	scanf		u_scanf
#define	setbuf		u_setbuf
#define	setbuffer	u_setfbf		/* collision	*/
#define	setlinebuf	u_setlinebuf
#define	snprintf	u_snprintf
#define	sprintf		u_sprintf
#define	sscanf		u_sscanf
#define	ungetc		u_ungetc

/* I/O macro defines.  I/O is assumed to be sequential, i.e., we do not check
 * for _iop < _bufptr.  This is consistent with UNIX usage.  The getc and putc
 * macros are quite efficient despite their complex appearance.
 */
#define	getchar()	fgetc(stdin)
#define	getc(fp) \
(((fp)->_iop >= (fp)->_itop) ? c_filbuf((fp)) : Memc[(fp)->_iop++] & 0377)

#define	putchar(ch)	fputc((ch),stdout)
#define	putc(ch,fp) \
(((fp)->_iop >= (fp)->_otop || ((ch) == '\n' && (fp)->_fflags&_FFLUSH)) ? \
c_flsbuf((unsigned int)(ch),(fp)) : ((int)(Memc[(fp)->_iop++] = (unsigned int)(ch))))

#define	fileno(fp)	(FPTOFD((fp)))
#define	feof(fp)	((fp)->_fflags & _FEOF)
#define	ferror(fp)	((fp)->_fflags & _FERR)
#define	clearerr(fp)	((fp)->_fflags &= ~_FERR)

extern int fclose ( FILE * );
extern FILE *fdopen ( int, const char * );
extern int fflush ( FILE * );
extern char *fgets ( char *, int, FILE * );
extern FILE *fopen ( const char *, const char * );
extern int fputc ( int , FILE * );
extern int fputs ( const char *, FILE * );
extern size_t fread ( void *, size_t, size_t, FILE * );
extern FILE *freopen ( const char *, const char *, FILE * );
extern int fseek ( FILE *, long, int );
extern long ftell ( FILE * );
extern int fgetc ( FILE * );
extern size_t fwrite ( const void *, size_t, size_t, FILE * );
extern char *gets ( char * );
extern int getw ( FILE * );
extern void perror ( const char * );
extern int puts ( const char * );
extern int putw ( int, FILE * );
extern void rewind ( FILE * );
extern int setbuf ( FILE *, char * );
extern int setbuffer ( FILE *, char *, size_t );
extern int setlinebuf ( FILE * );
extern int ungetc ( int, FILE * );

#ifdef USE_STDARG
extern int printf ( const char *, ... );
extern int fprintf ( FILE *, const char *, ... );
extern int scanf ( const char *, ... );
extern int fscanf ( FILE *, const char *, ... );
extern int sscanf ( const char *, const char *, ... );
extern int snprintf ( char *, size_t, const char *, ... );
extern int sprintf ( char *, const char *, ... );
#else
extern int printf ();
extern int fprintf ();
extern int scanf ();
extern int fscanf ();
extern int sscanf ();
extern int snprintf ();
extern int sprintf ();
#endif

#endif	/* NOLIBCNAMES */

#endif
