/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#ifdef OSF1
#include <machine/fpu.h>
#undef SHLIB
#endif

#ifdef SHLIB
#ifdef SOLARIS
#include <libelf.h>
#include <sys/mman.h>
#include <unistd.h>
#include <dlfcn.h>
#else
#include <sys/mman.h>
#include <a.out.h>
#endif
#endif

#define	import_spp
#define	import_kernel
#define	import_knames
#define	import_xnames
#define import_prtype
#include <iraf.h>

#ifdef sun
#include <floatingpoint.h>
#endif

/*
 * ZZSTRT,ZZSTOP -- Routines to perform initialization and cleanup functions
 * during process startup and shutdown, when the IRAF kernel routines are being
 * called from a program which does not have a ZMAIN.
 */

/* #define DEBUG */

static	int prtype, ipc_isatty=NO;
static	char os_process_name[SZ_FNAME];
static	char osfn_bkgfile[SZ_PATHNAME];
extern	malloc(), realloc(), free();
extern	char *((*environ)[]);
extern	int errno;

#ifdef SHLIB
extern	int sh_debug;			/* map shared image writeable */
static	short debug_ieee = 0;
extern	unsigned USHLIB[], VSHLIB[];	/* shared library descriptors */
static	unsigned vshlib[8];
#define	v_version	vshlib[0]	/* shared image version number */
#define	v_base		vshlib[1]	/* exported shimage addresses */
#define	v_etext		vshlib[2]
#define	v_edata		vshlib[3]
#define	v_end		vshlib[4]
#define	u_version	USHLIB[0]	/* application version number */
#define	sh_machtype	USHLIB[6]	/* machine architecture */
#endif

#define	align(a)	((a)&(~pmask))

#ifdef i386
/* The following kludge is required due to a 386i linker error to prevent
 * a BSS size of 0 for small processes with a short BSS segment.  If BSS is
 * set to zero in the file header but some BSS storage is required, the image
 * will die on a segmentation violation during startup while trying to
 * initialize the value of "environ".
 */
int	BSS_kludge[256];
#endif


/* ZZSTRT -- Initialize the IRAF kernel at process startup time.
 */
ZZSTRT()
{
	XINT	wsetsize=0L, junk;
#ifdef SHLIB
	static	int fd = 0;
	struct  stat fi;
	char	*segname;
	XCHAR	*bp;
#endif

	sprintf (os_process_name, "%d", getpid());
	strcpy (osfn_bkgfile, "");
	prtype = PR_HOST;

#ifdef SHLIB
	/* Map in the Sun/IRAF shared library, if the calling process was
	 * linked with the shared library, and the shared library has not
	 * already been mapped (fd != 0).  (See unix/shlib for more info).
	 * (This is rather monolithic and should probably be isolated to a
	 * separate module, but we are not trying for a general solution here).
	 */
	if (USHLIB[0] && (!fd || (fd && fstat(fd,&fi) == -1))) {
	    register unsigned pgsize, pmask;
	    unsigned t_off, t_len;
	    unsigned d_off, d_len;
	    unsigned b_off, b_len;
	    unsigned b_start, b_bytes;
	    static   char envdef[SZ_FNAME];
	    char     shimage[SZ_FNAME];
	    char     *shlib, *arch;
	    extern   char *getenv();
	    caddr_t  addr;
	    unsigned hsize;
#ifdef SOLARIS
	    register Elf32_Phdr *phdr;
	    register Elf32_Ehdr *ehdr;
	    caddr_t t_loc, d_loc, b_loc;
	    int adjust, phnum, nseg, i;
	    Elf32_Phdr *phdr_array;
	    Elf32_Phdr seg[32];
	    Elf *elf;
#else
	    unsigned t_loc, d_loc, b_loc;
#endif

	    /* Determine the architecture of the shared library. */
	    switch (sh_machtype) {
	    case 1:				/* see shlib/mkshlib.csh */
		arch = "sparc";	break;
	    case 2:
		arch = "i386";	break;
	    case 3:
		arch = "f68881"; break;
	    case 4:
		arch = "ffpa";	break;
	    case 5:
		arch = "ssun";	break;
	    case 6:
		arch = "sf2c";	break;
	    default:
		arch = "fsoft";	break;
	    }

	    /* Define IRAFARCH if not already defined in the process
	     * environment or if the definition does not match the architecture
	     * of the executable being run.  This is necessary for irafpath(),
	     * below, to successfully find the shared image.
	     */
	    sprintf (envdef, "IRAFARCH=%s", arch);
	    if (!(arch = getenv("IRAFARCH")) || strcmp(envdef,arch))
		putenv (envdef);
	
	    /* Open the shared library file */
	    sprintf (shimage, "S%d.e", u_version);
	    shlib = irafpath (shimage);
	    if (shlib == NULL || (fd = open (shlib, 0)) == -1) {
		fprintf (stderr,
		    "Error: cannot open iraf shared library %s\n", shlib);
		exit (1);
	    }

#ifdef SOLARIS
	    /* With Solaris executables are ELF format files.  The file
	     * and program headers tell where everything is and how to map
	     * the image segments.
	     */
	    elf_version (EV_CURRENT);
	    elf = elf_begin (fd, ELF_C_READ, NULL);
	    if (!elf) {
		fprintf (stderr, "%s: not an ELF format file\n", shlib);
		exit (2);
	    }
	    if (!(ehdr = elf32_getehdr (elf))) {
		fprintf (stderr, "%s: cannot read file header\n", shlib);
		exit (1);
	    }
	    if ((phnum = ehdr->e_phnum) <= 0 ||
		    !(phdr_array = elf32_getphdr (elf))) {
		fprintf (stderr, "%s: cannot read program header table\n",
		    shlib);
		exit (1);
	    }

	    /* Get a list of the loadable segments. */
	    for (i=0, nseg=0;  i < phnum;  i++) {
		phdr = (Elf32_Phdr *)((char *)phdr_array + i*ehdr->e_phentsize);
		if (phdr->p_type == PT_LOAD)
		    seg[nseg++] = *phdr;
	    }

	    /* Read in the vshlib array, which is stored in the text segment
	     * of the shared image.
	     */
	    if (nseg) {
		phdr = &seg[0];
		hsize = (unsigned)((char *)VSHLIB) - USHLIB[1];
		/* lseek (fd, phdr->p_offset + (long)hsize, 0); */
		lseek (fd, (long)hsize, 0);
		if (read (fd, (char *)vshlib, sizeof(vshlib)) !=
			sizeof(vshlib)) {
		    fprintf (stderr, "Read error on %s\n", shlib);
		    exit (1);
		}
	    } else {
		fprintf (stderr,
		    "Error: cannot open iraf shared library %s\n", shlib);
		exit (1);
	    }

	    pgsize = sysconf (_SC_PAGESIZE);
	    pmask  = pgsize - 1;

	    /* Determine the file and memory offsets of each segment of the
	     * shared image.
	     */
	    phdr = &seg[0];
	    adjust = phdr->p_offset % pgsize;

	    t_off = phdr->p_offset - adjust;
	    t_loc = (caddr_t) ((int)phdr->p_vaddr - adjust);
	    t_len = phdr->p_filesz + adjust;

	    phdr = &seg[1];
	    adjust = phdr->p_offset % pgsize;

	    d_off = phdr->p_offset - adjust;
	    d_loc = (caddr_t) ((int)phdr->p_vaddr - adjust);
	    d_len = phdr->p_filesz + adjust;

	    /* Map the BSS segment beginning with the first hardware page
	     * following the end of the data segment.
	     */
	    b_off = 0;				/* anywhere will do */
	    b_loc = (caddr_t) align ((int)d_loc + d_len + pgsize);
	    b_len = phdr->p_vaddr + phdr->p_memsz - (int)b_loc;

	    b_start = phdr->p_vaddr + phdr->p_filesz;
	    b_bytes = phdr->p_memsz - phdr->p_filesz;

#else !SOLARIS
	    /* Compute the location and size of each segment of the shared
	     * image memory.  The shared image is mapped at address s_base.
	     */
	    hsize = (unsigned)((char *)VSHLIB) - USHLIB[1];
	    lseek (fd, (long)hsize, 0);
	    if (read (fd, (char *)vshlib, sizeof(vshlib)) != sizeof(vshlib)) {
		fprintf (stderr, "Read error on %s\n", shlib);
		exit (1);
	    }

#ifdef i386
	    /* Map the shared image on a Sun-386i (SysV COFF format).
	     */
	    pgsize = getpagesize();
	    pmask  = pgsize - 1;

	    /* Determine the file and memory offsets of each segment of the
	     * shared image.
	     */

	    t_off = 0;				/* file offset */
	    t_loc = v_base;			/* location in memory */
	    t_len = v_etext - v_base;		/* segment length */

	    d_off = align (v_etext) - v_base;	/* map file page twice */
	    d_loc = align (v_etext);
	    d_len = v_edata - d_loc;

	    b_off = 0;				/* anywhere will do */
	    b_loc = align (d_loc + d_len + pmask);
	    b_len = v_end - b_loc;

	    b_start = v_edata;
	    b_bytes = v_end - v_edata;
#else
	    /* Map the shared image on a Sun-3 or Sun-4.
	     */
	    pgsize = PAGSIZ;
	    pmask  = pgsize - 1;

	    /* Determine the file and memory offsets of each segment of the
	     * shared image.  We cannot use the <a.out.h> macros since the
	     * text segment does not begin at the default location.  Also,
	     * the size of the BSS segment in the file header is not correct
	     * (under SunOS 4.0), so we compute directly from _end.
	     */

	    t_off = 0;				/* file offset */
	    t_loc = v_base;			/* location in memory */
	    t_len = v_etext - v_base;		/* segment length */

	    d_off = align (t_len + pmask);
	    d_loc = (v_etext + SEGSIZ-1) / SEGSIZ * SEGSIZ;
	    d_len = v_edata - d_loc;

	    /* Map the BSS segment beginning with the first hardware page
	     * following the end of the data segment.  This need not be
	     * the same as the PAGSIZ used for a.out.  v_edata-1 is the
	     * address of the last byte of the data segment.
	     */
	    b_off = 0;				/* anywhere will do */
	    b_loc = ((v_edata-1) & ~(getpagesize()-1)) + getpagesize();
	    b_len = v_end - b_loc;

	    b_start = v_edata;
	    b_bytes = v_end - v_edata;
#endif i386
#endif SOLARIS

#ifdef DEBUG
	    fprintf (stderr, " text: %8x %8x %8x -> %8x etext = %8x\n",
		t_loc, t_len, t_off, t_loc + t_len, v_etext);
	    fprintf (stderr, " data: %8x %8x %8x -> %8x edata = %8x\n",
		d_loc, d_len, d_off, d_loc + d_len, v_edata);
	    fprintf (stderr, "  bss: %8x %8x %8x -> %8x   end = %8x\n",
		b_loc, b_len, b_off, b_loc + b_len, v_end);
	    fprintf (stderr, " zero: %8x %8x %8s -> %8x\n",
		b_start, b_bytes, "        ", b_start + b_bytes);
#endif DEBUG

	    /* Map the header region of the "text" segment read-write.
	     * This area contains any commons exported by the shared image.
	     */
	    addr = mmap (t_loc, hsize, PROT_READ|PROT_WRITE,
		MAP_PRIVATE|MAP_FIXED, fd, t_off);
	    if ((int)addr == -1) {
		segname = "header";
		goto maperr;
	    }

	    /* Map the text segment read-only shared, unless the sh_debug
	     * flag is set (-w command line option), in which case the shared
	     * text is mapped private so that it may be modified, e.g., to
	     * set breakpoints while debugging a process.
	     */
	    addr = mmap (t_loc+hsize, t_len-hsize, PROT_READ|PROT_EXEC,
		(sh_debug?MAP_PRIVATE:MAP_SHARED)|MAP_FIXED, fd, t_off+hsize);
	    if ((int)addr == -1) {
		segname = "text";
		goto maperr;
	    }

	    /* Map the data segment read-write. */
	    addr = mmap (d_loc, d_len, PROT_READ|PROT_WRITE|PROT_EXEC,
		MAP_PRIVATE|MAP_FIXED, fd, d_off);
	    if ((int)addr == -1) {
		segname = "data";
		goto maperr;
	    }

	    /* The BSS section has to be initialized to zero.  We can map this
	     * onto any convenient file data provided we map it private and
	     * promptly modify (zero) the pages.  We assume here that the size
	     * of the BSS segment does not exceed the file size; this would
	     * not be true in general but should always be true in our case.
	     */
	    addr = mmap (b_loc, b_len, PROT_READ|PROT_WRITE|PROT_EXEC,
		MAP_PRIVATE|MAP_FIXED, fd, b_off);

	    if ((int)addr == -1) {
		segname = "bss";
maperr:		fprintf (stderr, "Error: cannot map the iraf shared library");
		fprintf (stderr, ", seg=%s, errno=%d\n", segname, errno);
		exit (2);
	    }

	    /* Zero the bss segment. */
	    bzero (b_start, b_bytes);

	    /* Verify that the version number and base address match. */
	    if (USHLIB[0] != VSHLIB[0] || USHLIB[1] != VSHLIB[1]) {
		fprintf (stderr,
		    "Error: iraf shared library mismatch, please relink\n");
		exit (3); }

	    /* Link the memory allocator function in the main process to stubs
	     * in the shared library, so that the library routines will
	     * allocate memory in the data space of the client.
	     */
#ifdef SOLARIS
	    VLIBINIT (environ, malloc, realloc, free,
		dlopen, dlclose, dlsym, dlerror);
#else
	    VLIBINIT (environ, malloc, realloc, free);
#endif
	}
#endif SHLIB

	/* Dummy routine called to indicate that mapping is complete. */
	ready();

#ifdef OSF1
	ieee_set_fp_control (
	      IEEE_TRAP_ENABLE_INV
	    | IEEE_TRAP_ENABLE_DZE
	    | IEEE_TRAP_ENABLE_OVF
	 /* | IEEE_TRAP_ENABLE_UNF */
	 /* | IEEE_TRAP_ENABLE_INE */
	);
#else
#ifdef SOLARIS
	/* Enable the common IEEE exceptions.  _ieee_enbint is as$enbint.s.
	 */
	_ieee_enbint (
	    (1 << (int)fp_division) |
	    (1 << (int)fp_overflow) |
	    (1 << (int)fp_invalid)
	);

#else
#ifdef SUNOS4
	/* The following enables the common IEEE floating point exceptions
	 * invalid, overflow, and divzero, causing the program to abort if
	 * any of these are detected.  If ZZSTRT is called from an IRAF
	 * program the abort action will normally be overidden when the IRAF
	 * main posts it's own handler for X_ARITH class exceptions.
	 */
	ieee_handler ("set", "common", SIGFPE_ABORT);

	/* The following disables recomputation of subnormal results or
	 * operands, which is done in software with an exception handler
	 * for machines with Weitek hardware, hence is very slow.  This
	 * is a deviation from the IEEE standard, but is consistent with
	 * the behavior of most non-IEEE hardware, and well designed
	 * software should not generate any subnormal values in any case,
	 * let alone depend upon small deviations in the value of such
	 * subnormals.
	 */
	abrupt_underflow_();

	/* The bitflag variable debug_ieee may be set nonzero to modify
	 * the default behavior (rounding direction and precision) of
	 * the IEEE hardware.
	 */
#	define	FP_NEAREST	0001	/* round toward nearest */
#	define	FP_TOZERO	0002	/* round toward zero */
#	define	FP_NEGATIVE	0004	/* round toward negative infinity */
#	define	FP_POSITIVE	0010	/* round toward positive infinity */
#	define	FP_EXTENDED	0020	/* round to extended precision */
#	define	FP_DOUBLE	0040	/* round to ieee double precision */
#	define	FP_SINGLE	0100	/* round to ieee single precision */

	if (debug_ieee) {
	    char *set = "set";
	    char *direction = "direction";
	    char *precision = "precision";

	    /* Set the rounding direction mode. */
	    if (debug_ieee & FP_NEAREST)
		ieee_flags (set, direction, "nearest", NULL);
	    if (debug_ieee & FP_TOZERO)
		ieee_flags (set, direction, "tozero", NULL);
	    if (debug_ieee & FP_NEGATIVE)
		ieee_flags (set, direction, "negative", NULL);
	    if (debug_ieee & FP_POSITIVE)
		ieee_flags (set, direction, "positive", NULL);

	    /* Set the rounding precision mode. */
	    if (debug_ieee & FP_EXTENDED)
		ieee_flags (set, precision, "extended", NULL);
	    if (debug_ieee & FP_DOUBLE)
		ieee_flags (set, precision, "double", NULL);
	    if (debug_ieee & FP_SINGLE)
		ieee_flags (set, precision, "single", NULL);
	}
#else
#ifdef mc68000
	/* Enable the IEEE floating point exceptions, for old versions of
	 * SunOS.  Pretty much obsolete now...
	 */
#	define	FP_INEXACT	0000010
#	define	FP_DIVIDE	0000020
#	define	FP_UNDERFLOW	0000040
#	define	FP_OVERFLOW	0000100
#	define	FP_INVALID	0000200
#	define	FP_INEX1	0000400
#	define	FP_INEX2	0001000
#	define	FP_DZ		0002000
#	define	FP_UNFL		0004000
#	define	FP_OVFL		0010000
#	define	FP_OPERR	0020000
#	define	FP_SNAN		0040000
#	define	FP_BSUN		0100000
	{
	    int mode = FP_BSUN|FP_SNAN|FP_OPERR|FP_DZ|FP_OVFL|FP_INVALID;
	    fpmode_ (&mode);
	}
#endif mc68000
#endif SUNOS4
#endif SOLARIS
#endif OSF1

#ifdef SYSV
	/* Initialize the time zone data structures. */
	tzset();
#endif

	/* Place a query call to ZAWSET to set the process working set limit
	 * to the IRAF default value, in case we did not inherit a working set
	 * limit value from the parent process.
	 */
	ZAWSET (&wsetsize, &junk, &junk, &junk);

	/* Initialize the stdio streams. */
	{   int ro = READ_ONLY, wo = WRITE_ONLY, chan;
	    ZOPNTY (U_STDIN, &ro, &chan);
	    ZOPNTY (U_STDOUT, &wo, &chan);
	    ZOPNTY (U_STDERR, &wo, &chan);
	}
}


/* ZZSTOP -- Clean up prior to process shutdown.
 */
ZZSTOP(){}

/* ready -- This is a dummy routine used when debugging to allow a breakpoint
 * to be set at a convenient point after the shared image has been mapped in.
 */
ready(){}
