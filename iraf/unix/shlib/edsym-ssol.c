/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/file.h>
#include <nlist.h>
#include <libelf.h>

/*
 * EDSYM -- Edit the symbol table of a process or object module which uses
 * the IRAF shared library.  (Sun Solaris version July94).
 *
 * Usage:	edsym file shimage [flags]
 *
 *	-d	debug: show symbol table being edited
 *	-k	keep: do not omit "uninteresting" symbols
 *	-n	noact: do not modify any files
 *	-t	delete symbols pointing into transfer vector
 *	-T	delete all shared image symbols, keeping only client symbols
 */


#define	VHDRSIZE	0x1c
#define	v_end		vec[4]
#define	IS_TVECT(a)	((a)>=(vshlib+VHDRSIZE)&&(a)<vshend)
#define	max(a,b)	(((a)>=(b))?(a):(b))

static	int debug = 0;			/* print symbol table */
static	int noact = 0;			/* do not modify any files */
static	int omit_tv = 0;		/* omit transfer vector symbols */
static	int omit_shsym = 0;		/* omit all shlib symbols */
static	int keep_sym = 0;		/* do not omit uninteresting symbols */

extern	char *malloc();


/* EDSYM -- Edit the symbol table of a process which uses the IRAF shared
 * library.  For each symbol found which points to a location in the shared
 * library transfer vector, add a V prefix to the symbol name, and add a
 * symbol with the old name pointing to the actual function in the shared
 * image.  This is desirable before runtime debugging of processes linked
 * with the shared library.
 */
main (argc, argv)
int argc;
char *argv[];
{
        register Elf32_Phdr *phdr;
        register Elf32_Ehdr *ehdr;
        register Elf32_Shdr *shdr;
	register Elf32_Sym *sym;
	unsigned vshlib, vshend, vsize;
	char *fname, *shlib, *buf, *ip, *op;
	int version, fd, fd_sh, nsyms, arg;
	char *out, shpath[256];
	int offset, nbytes;
	struct nlist nl[3];
	unsigned ushlib[8];
	unsigned *vec, *epa;
        Elf *elf, *elf_sh;
        Elf_Scn *scn;

	/* Process arguments.  This version of edsym does not support all of
	 * the following arguments.
	 */
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
	if ((fd = open (fname, 2)) < 0) {
	    fprintf (stderr, "edsym: cannot open file %s\n", fname);
	    exit (1);
	}

	elf_version (EV_CURRENT);
	elf = elf_begin (fd, ELF_C_READ, NULL);
	if (!elf) {
	    fprintf (stderr, "edsym: not an ELF format executable\n");
	    exit (2);
	}

	/* Read file header.  */
	ehdr = elf32_getehdr (elf);
	if (!ehdr) {
	    fprintf (stderr, "edsym: cannot read file header\n");
	    exit (3);
	}

	/* Read program header for text segment, which we assume to be the
	 * first loadable program segment.
	 */
	phdr = elf32_getphdr (elf);
	if (ehdr->e_phnum <= 0 || !phdr) {
	    printf ("cannot read program header\n");
	    exit (4);
	}

	/* Get the ushlib vector from the file being edited.
	 */
	nl[0].n_name = "ushlib_";
	nl[1].n_name = NULL;
	if (nlist (fname, nl) != 0) {
	    fprintf (stderr, "cannot read name list from %s\n", fname);
	    exit (4);
	}

	offset = (unsigned)nl[0].n_value - phdr->p_vaddr + phdr->p_offset;
	lseek (fd, offset, L_SET);
	if (read (fd, (char *)ushlib, sizeof(ushlib)) != sizeof(ushlib)) {
	    fprintf (stderr, "cannot read shared image %s\n", fname);
	    exit (9);
	}

	version = ushlib[0];
	vshlib = ushlib[2];
	vshend = ushlib[3];
	vsize = vshend - vshlib;

	/* Read transfer vector from shared image.
	 */

	/* Use the correct version of the shared image. */
	for (ip=shlib, out=op=shpath;  *op = *ip++;  op++)
	    if (*op == '/')
		out = op + 1;
	if (strcmp (out, "S.e") == 0) {
	    sprintf (out, "S%d.e", version);
	    shlib = shpath;
	}

	/* Open the file to be edited. */
	if ((fd_sh = open (shlib, 0)) < 0) {
	    fprintf (stderr, "edsym: cannot open file %s\n", shlib);
	    exit (1);
	}

	elf_sh = elf_begin (fd_sh, ELF_C_READ, NULL);
	if (!elf_sh) {
	    fprintf (stderr, "edsym: not an ELF format executable\n");
	    exit (2);
	}

	/* Read file header.  */
	ehdr = elf32_getehdr (elf_sh);
	if (!ehdr) {
	    fprintf (stderr, "edsym: cannot read file header\n");
	    exit (3);
	}

	/* Read program header for text segment, which we assume to be the
	 * first loadable program segment.
	 */
	phdr = elf32_getphdr (elf_sh);
	if (ehdr->e_phnum <= 0 || !phdr) {
	    printf ("cannot read program header\n");
	    exit (4);
	}

	if (!(vec = (unsigned *) malloc (vsize))) {
	    fprintf (stderr, "out of memory\n");
	    exit (10);
	}
	offset = vshlib - phdr->p_vaddr + phdr->p_offset;
	lseek (fd_sh, offset, L_SET);
	if (read (fd_sh, vec, vsize) != vsize) {
	    fprintf (stderr,
		"cannot read transfer vector from %s\n", shlib);
	    exit (11);
	}

	/* Locate symbol table section in file to be edited. */
	scn = NULL;
	while (scn = elf_nextscn(elf,scn)) {
	    shdr = elf32_getshdr (scn);
	    if (shdr->sh_type == SHT_SYMTAB)
		break;
	}
	if (!scn) {
	    fprintf (stderr, "edsym: file %s has no symbol table\n", fname);
	    exit (12);
	}

	/* Read symbol table into memory. */
	nbytes = shdr->sh_size;
	nsyms = nbytes / sizeof(Elf32_Sym);
	if (!(buf = (char *) malloc (nbytes))) {
	    fprintf (stderr, "edsym: out of memory\n");
	    exit (13);
	}
	lseek (fd, shdr->sh_offset, 0);
	if (read (fd, buf, nbytes) != nbytes) {
	    fprintf (stderr, "edsym: cannot read symbol table\n");
	    exit (14);
	}

	/* Now edit the symbol table of the object file.
	 */
	for (sym = (Elf32_Sym *)buf;  --nsyms >= 0;  sym++) {
	    Elf32_Addr v = sym->st_value;
	    if (debug) {
		printf ("name=%d value=0x%x size=0x%x info=%o shndx=%d",
		    sym->st_name, sym->st_value, sym->st_size,
		    sym->st_info, sym->st_shndx);
	    }
	    if (sym->st_size == 0 &&
		    ELF32_ST_BIND(sym->st_info) == STB_GLOBAL && IS_TVECT(v)) {
		epa = (unsigned *)((char *)vec + (v - vshlib));
		v = (((*epa & 0x3fffff) << 10) | (*(epa+1) & 0x3ff));
		if (debug)
		    printf ("\t%x -> %x", sym->st_value, v);
		sym->st_value = (Elf32_Addr) v;
	    }
	    if (debug)
		printf ("\n");
	}

	/* Write out the edited symbol table. */
	lseek (fd, shdr->sh_offset, 0);
	if (write (fd, buf, nbytes) != nbytes) {
	    fprintf (stderr, "edsym: cannot update symbol table\n");
	    exit (7);
	}

	/* All done. */
	free (buf);
	free ((char *)vec);
	elf_end (elf_sh);
	close (fd_sh);
	elf_end (elf);
	close (fd);

	exit (0);
}
