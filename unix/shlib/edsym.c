/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>

/*
 * EDSYM -- Edit the symbol table of a process or object module which uses
 * the IRAF shared library.
 *
 * Usage:	edsym file shimage [flags]
 *
 *	-d	debug: show symbol table being edited
 *	-k	keep: do not omit "uninteresting" symbols
 *	-n	noact: do not modify any files
 *	-t	delete symbols pointing into transfer vector
 *	-T	delete all shared image symbols, keeping only client symbols
 */

#	ifdef i386
#include <filehdr.h>
#include <aouthdr.h>
#include <nlist.h>

#define	AOUT
#include <syms.h>
#define	N_name		_n._n_name
#define	N_zeroes	_n._n_n._n_zeroes
#define	N_offset	_n._n_n._n_offset
#define	N_abs		-1
#define	HDROFF		0xd0
#define	OBJECT		0514

#	else
#include <a.out.h>

#define	HDROFF		0
#define	OBJECT		0407
#define SYMESZ		sizeof(struct nlist)
#	endif

#define	DEF_SBUFSIZE	32768
#define	INC_SBUFSIZE	8192
#define	MAXLEN		256
#define	VHDRSIZE	0x1c
#define	v_end		vec[4]
#define	IS_TVECT(a)	((a)>=(vshlib+VHDRSIZE)&&(a)<vshend)
#define	max(a,b)	(((a)>=(b))?(a):(b))

static	int debug = 0;			/* print symbol table */
static	int noact = 0;			/* do not modify any files */
static	int omit_tv = 0;		/* omit transfer vector symbols */
static	int omit_shsym = 0;		/* omit all shlib symbols */
static	int keep_sym = 0;		/* do not omit uninteresting symbols */

static	char *i_sbuf, *o_sbuf;
static	char *op, *otop;
static	int  o_sbufsize;
extern	char *malloc();
extern	char *realloc();


/* EDSYM -- Edit the symbol table of a process which uses the IRAF shared
 * library.  For each symbol found which points to a location in the shared
 * library transfer vector, add a V prefix to the symbol name, and add a
 * symbol with the old name pointing to the actual function in the shared
 * image.  This is desirable before runtime debugging of processes linked
 * with the shared library.
 */
main (argc, argv)
int	argc;
char	*argv[];
{
	unsigned vshlib, vshend, vbase, vsize;
	int	offset, nbytes;
#ifdef i386
	struct	filehdr fh, sh;
	struct	aouthdr ah;
#else
	struct	exec fh, sh;
#endif

	unsigned *vec;
	char	*fname, *shlib, *ip, *oop, *out;
	int	fd, nsym, nosym, arg;
	int	errcode = 0;
	char	tempfile[256];
	char	shpath[256];
	struct	nlist nl[3];
	FILE	*fp, *tp;
	int	version;

	if (argc < 3) {
	    fprintf (stderr, "Usage: edsym <file> <shlib> [-dkntT]\n");
	    exit (0);

	} else {
	    fname = argv[1];
	    shlib = argv[2];
	    for (arg=3;  arg < argc;  arg++) {
		if (argv[arg][0] != '-')
		    continue;
		for (ip=argv[arg]+1;  *ip;  ip++) {
		    switch (*ip) {
		    case 'd':		/* print symbols */
			debug++;
			break;
		    case 'k':		/* do not omit uninteresting symbols */
			keep_sym++;
			break;
		    case 'n':		/* do not edit object file */
			noact++;
			break;
		    case 't':		/* omit transfer vector symbols */
			omit_tv++;
			break;
		    case 'T':		/* omit all shlib symbols */
			omit_shsym++;
			break;
		    default:
			fprintf (stderr, "edsym: unknown switch -%c\n", *ip);
		    }
		}
	    }
		
	}

	/* Open the file to be edited. */
	if ((fp = fopen (fname, "r+")) == NULL) {
	    fprintf (stderr, "cannot open file %s\n", fname);
	    exit (1);
	} else {
	    /* Get the file header. */
#ifdef i386
	    if (fread (&fh, sizeof(fh), 1, fp) != 1) {
		errcode = 1;  goto readerr;
	    } else if (!ISCOFF(fh.f_magic) && fh.f_magic != OBJECT) {
		fprintf (stderr, "not a COFF format file\n");
		exit (2);
	    } else if (fh.f_symptr == 0 || fh.f_nsyms <= 0) {
		fprintf (stderr, "%s has been stripped\n", fname);
		exit (3);
	    }
#else
	    if (fread (&fh, sizeof(fh), 1, fp) != 1) {
		errcode = 1;  goto readerr;
	    } else if (N_BADMAG(fh) && fh.a_magic != OBJECT) {
		fprintf (stderr, "not a valid executable or object file\n");
		/* exit (2); */
	    } else if (fh.a_syms <= 0) {
		fprintf (stderr, "%s has been stripped\n", fname);
		exit (3);
	    }
#endif
	}

	/* Get the shared image version number.  This is stored in the
	 * first element of the ushlib vector in the file being edited.
	 */
#ifdef i386
	nl[0].n_name = "ushlib_";
	nl[1].n_name = NULL;
#else
	nl[0].n_un.n_name = "_ushlib_";
	nl[1].n_un.n_name = NULL;
#endif
	if (nlist (fname, nl) != 0) {
	    fprintf (stderr, "cannot read name list from %s\n", fname);
	    exit (4);
	}

#ifdef i386
	lseek (fileno(fp), (unsigned)nl[0].n_value - 0x1000, L_SET);
#else
	lseek (fileno(fp), (unsigned)nl[0].n_value - PAGSIZ, L_SET);
#endif
	if (read (fileno(fp), &version, sizeof(version)) != sizeof(version)) {
	    fprintf (stderr,
		"cannot read shared image version number from %s\n", fname);
	    exit (9);
	}

	/* Use the correct version of the shared image. */
	for (ip=shlib, out=oop=shpath;  *oop = *ip++;  oop++)
	    if (*oop == '/')
		out = oop + 1;
	if (strcmp (out, "S.e") == 0) {
	    sprintf (out, "S%d.e", version);
	    shlib = shpath;
	}

	if (debug)
	    printf ("use shared image %s\n", shlib);

	/* Get the location of the shared image transfer vector. */
#ifdef i386
	nl[0].n_name = "vshlib_";
	nl[1].n_name = "vshend_";
	nl[2].n_name = NULL;
#else
	nl[0].n_un.n_name = "_vshlib_";
	nl[1].n_un.n_name = "_vshend_";
	nl[2].n_un.n_name = NULL;
#endif
	if (nlist (shlib, nl) != 0) {
	    fprintf (stderr, "cannot read name list from %s\n", shlib);
	    exit (4);
	}

	/* Open the shared image. */
	if ((fd = open (shlib, O_RDONLY)) == -1) {
	    fprintf (stderr, "cannot open shared image %s\n", shlib);
	    exit (5);

	} else {
#ifdef i386
	    /* Get the file header. */
	    if (read (fd, &sh, sizeof(sh)) != sizeof(sh)) {
		errcode = 2;  goto readerr;
	    } else if (!ISCOFF(sh.f_magic)) {
		fprintf (stderr, "not a COFF format file\n");
		exit (6);
	    }

	    /* Get the system header. */
	    if (read (fd, &ah, sizeof(ah)) != sizeof(ah)) {
		errcode = 3;  goto readerr;
	    } else if (ah.magic != 0413) {
		fprintf (stderr, "bad magic %o in system header\n", ah.magic);
		exit (7);
	    }
#else
	    /* Get the file header. */
	    if (read (fd, &sh, sizeof(sh)) != sizeof(sh)) {
		errcode = 2;  goto readerr;
	    } else if (sh.a_magic != ZMAGIC) {
		fprintf (stderr, "not a page-aligned executable file\n");
		exit (6);
	    }
#endif
	    /* Read the transfer vector. */
	    vshlib = (unsigned) nl[0].n_value;
	    vshend = (unsigned) nl[1].n_value;
	    vsize  = vshend - vshlib;
#ifdef i386
	    vbase  = ah.text_start;
#else
	    vbase  = (sh.a_entry & ~0xffffff);
#endif
	    if (debug) {
		printf ("vshlib=%x, vshend=%x, vbase=%x, vsize=%x\n",
		    vshlib, vshend, vbase, vsize);
	    }

	    vec = (unsigned *) malloc (vsize);
	    if (vec == NULL) {
		fprintf (stderr, "out of memory\n");
		exit (8);
	    }

	    lseek (fd, vshlib - vbase + HDROFF, L_SET);
	    if (read (fd, vec, vsize) != vsize) {
		fprintf (stderr,
		    "cannot read transfer vector from %s\n", shlib);
		exit (9);
	    }

	    close (fd);
	}

	/* Now edit the symbol table of the object file.  To do this we must
	 * first read the string buffer into memory.  We then open a scratch
	 * file for symbol output.  Successive symbols are read from the input
	 * file.  For each symbol which points into the transfer vector area
	 * we output two symbols, one a copy of the input symbol with V
	 * prepended to the symbol name, the second a copy with the symbol
	 * value changed to point to the location of the actual procedure in
	 * the shared image.  Other symbols are merely copied to the scratch
	 * file.  When all symbols have been processed we overwrite the symbol
	 * table and string buffer of the file being edited with the contents
	 * of the scratch buffer (containing the new symbols) and the new
	 * string buffer, completing the editing operation.
	 */

	/* Read the string buffer. */
#ifdef i386
	if (fh.f_symptr != 0) {
	    offset = fh.f_symptr + (fh.f_nsyms * SYMESZ);
#else
	if ((offset = N_STROFF(fh)) != 0) {
#endif
	    /* The size of the string buffer in bytes is stored in the first
	     * four bytes of the buffer.
	     */
	    fseek (fp, offset, L_SET);
	    if (fread (&nbytes, sizeof(int), 1, fp) != 1) {
		errcode = 5;  goto readerr;
	    }

	    i_sbuf = malloc (max(nbytes,DEF_SBUFSIZE));
	    o_sbuf = malloc (o_sbufsize = max(nbytes,DEF_SBUFSIZE));
	    if (i_sbuf == NULL || o_sbuf == NULL) {
		fprintf (stderr, "out of memory\n");
		exit (8);
	    }

	    fseek (fp, offset, L_SET);
	    if (fread (i_sbuf, 1, nbytes, fp) != nbytes) {
		fprintf (stderr, "cannot read string buffer from %x\n", fname);
		errcode = 5;  goto readerr;
	    }
	    op = o_sbuf;
	    otop = o_sbuf + o_sbufsize;
	}

	/* Open the scratch file. */
	sprintf (tempfile, "%s.T", fname);
	if ((tp = fopen (tempfile, "w+")) == NULL) {
	    fprintf (stderr, "cannot create %s\n", tempfile);
	    exit (10);
	}

	/* Process the symbol table, writing out two symbols for every
	 * function symbol pointing into the transfer vector, and copying
	 * all other symbols unmodified.
	 */
	fseek (tp, 0L, L_SET);
#ifdef i386
	fseek (fp, fh.f_symptr, L_SET);
	for (nsym=fh.f_nsyms, nosym=0;  --nsym >= 0;  ) {
	    register int n, ch;
	    struct   syment sym, osym;
	    char     name[MAXLEN];
	    int      keep, naux;
	    unsigned v, *epa;

	    if (fread (&sym, SYMESZ, 1, fp) != 1) {
		errcode = 6;  goto readerr;
	    }
	    v = (unsigned)sym.n_value;
	    if (sym.N_zeroes) {
		strncpy (name, sym.N_name, 8);
		name[8] = '\0';
	    } else
		strncpy (name, i_sbuf+sym.N_offset, MAXLEN-1);

	    if (debug) {
		printf ("%20s %8x %3d %6o %3d %2d", name, sym.n_value,
		    sym.n_scnum, sym.n_type, sym.n_sclass, sym.n_numaux);
	    }

	    if ((sym.n_scnum != N_abs) || !IS_TVECT(v) || (name[0] == 'V')) {
#else
	fseek (fp, N_SYMOFF(fh), L_SET);
	for (nsym=(fh.a_syms/SYMESZ), nosym=0;  --nsym >= 0;  ) {
	    register int n, ch;
	    struct   nlist sym, osym;
	    char     name[MAXLEN];
	    int      keep, naux;
	    unsigned v, *epa;

	    if (fread (&sym, SYMESZ, 1, fp) != 1) {
		errcode = 6;  goto readerr;
	    }
	    v = (unsigned)sym.n_value;
	    if (sym.n_un.n_strx)
		strncpy (name, i_sbuf+sym.n_un.n_strx, MAXLEN-1);

	    if (debug) {
		printf ("%20s %3x %8x %6o",
		    name, sym.n_type, sym.n_value, sym.n_desc);
	    }

	    if (!(sym.n_type & N_ABS) || !IS_TVECT(v) || (name[1] == 'V')) {
#endif
		/* Omit "uninteresting" symbols. */
		ch = name[0];
		keep = (keep_sym || (
		    !(ch == 'L') &&
		    !(ch == 'v' && name[1] == '.') &&
		    !(ch == 'V' && strncmp(name,"VAR_SEG",7)==0) &&
		    !(ch == 'A' && strncmp(name,"ARR_SEG",7)==0)
		    /*
		&& !(n=strlen(name), (name[n-2] == '.' && name[n-1] == 'o'))
		     */
		    ));

		if (keep && !(omit_shsym && v >= vshlib && v <= v_end)) {
		    editname (&sym, NULL);
		    fwrite (&sym, SYMESZ, 1, tp);
		    nosym++;
		} else if (debug)
		    printf ("  deleted");

		if (debug)
		    printf ("\n");

	    } else {
		/* Output the V symbol pointing to the transfer vector.
		 */
		if (!omit_tv && !omit_shsym) {
		    osym = sym;
		    editname (&osym, "V");
		    fwrite (&osym, SYMESZ, 1, tp);
		    nosym++;
		}
		
		/* Disassemble the JMP instruction in the transfer vector to
		 * get the address of the referenced procedure in the shared
		 * library.  [extracted from os.zlocpr].
		 */
		epa = (unsigned *)((char *)vec + (v - vshlib));
#ifdef i386
		v = ((unsigned)v + *((unsigned *)((char *)epa + 1)) + 5);
#else
#ifdef mc68000
		v = *((unsigned *)((char *)epa + 2));
#else
#ifdef sparc
		v = (((*epa & 0x3fffff) << 10) | (*(epa+1) & 0x3ff));
#endif
#endif
#endif

		/* Output the same-name symbol, pointing to the function
		 * pointed to by the transfer vector.
		 */
		if (!omit_shsym) {
		    osym = sym;
		    osym.n_value = v;
		    editname (&osym, NULL);
		    fwrite (&osym, SYMESZ, 1, tp);
		    nosym++;

		    if (debug)
			printf ("  -> %x\n", v);

		} else if (debug)
		    printf ("  deleted\n");
	    }
#ifdef i386
	    for (naux = sym.n_numaux;  --naux >= 0;  --nsym) {
		if (fread (&sym, SYMESZ, 1, fp) != 1) {
		    errcode = 7;  goto readerr;
		}
		editname (&sym, NULL);
		fwrite (&sym, SYMESZ, 1, tp);
		nosym++;
	    }
#endif
	}

	if (!noact) {
	    /* Copy the new symbol table to the object file, replacing the old
	     * one.
	     */
	    if (nosym > 0) {
#ifdef i386
		struct   syment sym;
		fseek (fp, fh.f_symptr, L_SET);
#else
		struct   nlist sym;
		fseek (fp, N_SYMOFF(fh), L_SET);
#endif
		fseek (tp, 0L, L_SET);
		for (nsym=nosym;  --nsym >= 0;  ) {
		    if (fread (&sym, SYMESZ, 1, tp) != 1) {
			errcode = 8;  goto readerr;
		    }
		    fwrite (&sym, SYMESZ, 1, fp);
		}
	    }

	    /* Append the string buffer. */
	    if (op > o_sbuf) {
		*((int *)o_sbuf) = op - o_sbuf;
		fwrite (o_sbuf, 1, op - o_sbuf, fp);
		fflush (fp);
	    }

	    /* Truncate the file at the point just past the string buffer.
	     */
	    if (ftruncate (fileno(fp), ftell(fp)) != 0)
		fprintf (stderr, "Warning: cannot truncate %s\n", fname);

	    /* Update the file header. */
#ifdef i386
	    fh.f_nsyms = nosym;
#else
	    fh.a_syms = nosym * SYMESZ;
#endif
	    fseek (fp, 0L, L_SET);
	    fwrite (&fh, sizeof(fh), 1, fp);
	    fflush (fp);
	}

	/* All done. */
	free (vec);
	free (i_sbuf);
	free (o_sbuf);
	fclose (tp);
	fclose (fp);
	unlink (tempfile);
	exit (0);

readerr:
	fprintf (stderr, "read error %d on %s\n", errcode, fname);
	exit (11);
}


/* EDITNAME -- Replace the name 'name' of a symbol by 'Vname', copying the
 * edited name to the output string buffer, and editing the symbol structure
 * as necessary to point to the new name.  If called with a NULL or null string
 * prefix, this serves to copy out the symbol name.
 */
editname (sym, prefix)
#ifdef i386
register struct	syment *sym;
#else
register struct	nlist *sym;
#endif
char	*prefix;
{
	register char	*ip, *np;
	char	name[MAXLEN];
	int	used, n;

#ifdef i386
	/* Construct the new name. */
	n = 0;
	if (prefix)
	    for (ip=prefix;  (name[n] = *ip++);  n++)
		;

	if (sym->N_zeroes) {
	    strncpy (&name[n], sym->N_name, SYMNMLEN);
	    name[n+SYMNMLEN] = '\0';
	} else
	    strncpy (&name[n], i_sbuf + sym->N_offset, MAXLEN-n);

	/* If the new name is no longer than SYMNMLEN place it directly in
	 * the symstruct, else append it to the string buffer.
	 */
	if ((n = strlen(name)) <= SYMNMLEN)
	    strncpy (sym->N_name, name, SYMNMLEN);
	else {
	    sym->N_zeroes = 0;
	    sym->N_offset = putname(name);
	}
#else
	/* Construct the new name. */
	if (sym->n_un.n_strx) {
	    /* Copy any leading underscores. */
	    for (np = i_sbuf + sym->n_un.n_strx,  n=0;  *np == '_';  np++)
		name[n++] = '_';

	    /* Add the prefix string, if any. */
	    if (prefix)
		for (ip=prefix;  (name[n] = *ip++);  n++)
		    ;

	    /* Append the symbol name. */
	    strncpy (&name[n], np, MAXLEN-n);

	    /* Write the new symbol to the output symbol table. */
	    sym->n_un.n_strx = putname(name);
	}
#endif
}


/* PUTNAME -- Append a string to the output string buffer.
 */
putname (name)
char	*name;
{
	register char *ip, *oop;
	int	used, offset;

	if (op == o_sbuf)
	    op = o_sbuf + sizeof(int);

	if (op + strlen(name) >= otop) {
	    used = op - o_sbuf;
	    o_sbuf = realloc (o_sbuf, o_sbufsize += INC_SBUFSIZE);
	    op = o_sbuf + used;
	    otop = o_sbuf + o_sbufsize;
	}

	offset = op - o_sbuf;
	for (ip=name, oop=op;  *oop++ = *ip++;  )
	    ;
	op = oop;

	return (offset);
}
