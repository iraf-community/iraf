/*
 * FSET.H -- FSET/FSTATUS parameters (r = read_only, * = internal to FIO).
 * Some of these parameters provide access to the guts of the i/o system and
 * should not be used by packages outside FIO, to avoid a dependence on the
 * inner workings of FIO.  Parameters affecting the file buffer number, types,
 * or sizes are read-only after the first i/o to the file.
 */

#define	F_ADVICE	1	/*  advice on type of access (rand,seq,def) */
#define	F_ASYNC		2	/*  enable asynchronous i/o [y/n] */
#define	F_BLKSIZE	3	/*r device block size, chars */
#define	F_BUFPTR	4	/** install externally created file buffer */
#define	F_BUFSIZE	5	/*  file buffer size, chars */
#define	F_BUFTOP	6	/** set pointer to top of buffer */
#define	F_BUFTYPE	7	/*  file buffer type (F_LOCAL or F_GLOBAL) */
#define	F_CANCEL	8	/*  cancel buffered data */
#define	F_CHANNEL	9	/*r channel number */
#define	F_CLOBBER	10	/*  is file clobber enabled [y/n] */
#define	F_DEVCODE	11	/** device driver: index in device table */
#define	F_DEVICE	12	/** entry point address device read/get () */
#define	F_EOF		13      /*r is file positioned at EOF [y/n] */
#define	F_FILENAME	14	/*r get file name (fstats) */
#define	F_FILESIZE	15	/*r get file size (fstatl) */
#define	F_FILEWAIT	16	/*  is file wait on open enabled [y/n] */
#define	F_FIODES	17	/** struct pointer to file descr. structure */
#define	F_FFIOMODE	18	/*r is i/o in progress on channel */
#define	F_FLUSHNL	19	/*  is flush on newline enabled [y/n] */
#define	F_KEEP		20	/*  keep file after task completion? */
#define	F_LASTREFFILE	21	/*r get FD of last referenced (active) file */
#define	F_MODE		22	/*r file access mode (ro,wo,rw) */
#define	F_NBUFS		23	/*  number of file buffers */
#define	F_NCHARS	24	/*r nchars last transfer */
#define	F_ONEVERSION	25	/*  keep only one version of a file */
#define	F_OPEN		26	/*r is file open */
#define	F_OPTBUFSIZE	27	/*r optimal buffer size for device (chars) */
#define	F_PBBSIZE	28	/*  push back buffer size, chars */
#define	F_RAW		29	/*  raw (vs "cooked") mode for text devices */
#define	F_READ		30	/*r does file have read access [y/n] */
#define	F_REDIR		31	/*r i/o is redirected */
#define	F_SZBBLK	32	/*r size(bytes) of last devblock read|written */
#define	F_TYPE		33	/*r file type (text, binary) */
#define	F_WRITE		34	/*r does file have write access [y/n] */
#define	F_MAXBUFSIZE	35	/*r maximum file buffer size */
#define	F_UNREAD	36	/*r number of unread chars in FIO buffer */
#define	F_VALIDATE	37	/*  validate FIO buffer contents (fseti) */
#define	F_CLOSEFD	38	/*  close host channel when inactive */
#define	F_FIRSTBUFOFF	39	/*  file offset of 1st FIO buffer (def=1) */
#define	F_SETREDRAW	40	/*w set/enable screen redraw code */

#define	F_LOCAL		1	/* allocate local file buffers */
#define	F_GLOBAL	2	/* take file buffers from global pool */
#define	F_GETPROT	2	/* is file protected? */
#define	F_NDELAY	0100	/* nonblocking i/o */
#define	F_FFIOINACT	0	/* no i/o in progress */
#define	F_FFIOREAD	1	/* read in progress */
#define	F_FFIOWRITE	2	/* write in progress */

#define	D_fset
