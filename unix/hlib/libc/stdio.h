/*
 * U_STDIO.H -- C defines used to emulate the UNIX standard i/o package upon
 * IRAF file i/o.  These definitions are portable, but highly dependent on
 * the guts of IRAF FIO.
 */

#ifndef D_libc
#ifndef import_libc
#include "libc.h"
#endif
#endif

#define	BUFSIZ		SZ_DEFIOBUF
#define	_NFILE		FIO_MAXFD
#define	FILE		struct _iobuf

# ifndef NULL
#define	NULL		0
# endif
# ifndef EOF
#define	EOF		(-1)
# endif

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

extern	XINT FIOCOM[];			/* the FIO common		*/

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
#define	FDTOFP(fd)	((FILE *)(&FIOCOM[(fd)-1]))
#define	FPTOFD(fp)	((XINT)((XINT *)(fp) - FIOCOM + 1))

#define	stdin		(FDTOFP(3))
#define	stdout		(FDTOFP(4))
#define	stderr		(FDTOFP(5))


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
c_flsbuf((unsigned)(ch),(fp)) : ((int)(Memc[(fp)->_iop++] = (unsigned)(ch))))

#define	fileno(fp)	(FPTOFD((fp)))
#define	feof(fp)	((fp)->_fflags & _FEOF)
#define	ferror(fp)	((fp)->_fflags & _FERR)
#define	clearerr(fp)	((fp)->_fflags &= ~_FERR)


FILE	*fopen();
FILE	*fdopen();
char	*fgets();
char	*gets();

#define	D_stdio
