/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <errno.h>

#ifdef LINUX
# include <fpu_control.h>
#endif
#ifdef CYGWIN
# include <mingw/fenv.h>
#endif
#ifdef MACOSX
# include <math.h>
# include <fenv.h>
#endif

#ifdef SHLIB
#ifdef SOLARIS
#include <libelf.h>
#include <sys/mman.h>
#include <sys/utsname.h>
#include <unistd.h>
#include <dlfcn.h>
#else
#include <sys/mman.h>
#include <a.out.h>
#endif
#endif

#ifdef sun
#include <floatingpoint.h>
#endif

#ifdef SOLARIS
#include <ieeefp.h>
#endif

#define import_spp
#define import_kernel
#define import_knames
#define import_xnames
#define import_prtype
#include <iraf.h>

#include "zos.h"

/*
 * ZZSTRT,ZZSTOP -- Routines to perform initialization and cleanup functions
 * during process startup and shutdown, when the IRAF kernel routines are being
 * called from a program which does not have a ZMAIN.
 */

/* #define DEBUG */

static	XINT x_prtype, x_ipc_isatty=NO;
static	XINT x_ipc_in = 0, x_ipc_out = 0;
static	XCHAR x_os_process_name[SZ_FNAME];
static	XCHAR x_osfn_bkgfile[SZ_PATHNAME];

#ifdef SHLIB
extern	char *((*environ)[]);

static	short debug_ieee = 0;
static	unsigned int vshlib[8];
#define v_version	vshlib[0]	/* shared image version number */
#define v_base		vshlib[1]	/* exported shimage addresses */
#define v_etext		vshlib[2]
#define v_edata		vshlib[3]
#define v_end		vshlib[4]
#define u_version	USHLIB[0]	/* application version number */
#define sh_machtype	USHLIB[6]	/* machine architecture */
#endif

#define align(a)	((a)&(~pmask))

int ready( void );


/* ZZSTRT -- Initialize the IRAF kernel at process startup time.
 */
int ZZSTRT( void )
{
	XSIZE_T	wsetsize=0L, junk;
#ifdef SHLIB
	static	int fd = 0;
	struct  stat fi;
	const char *segname;
	XCHAR	*bp;
#endif
	spp_debug();

	/* Initialize globals.
	 */
	snprintf ((char *)x_os_process_name, SZ_FNAME, "%d", getpid());
	strcpy ((char *)x_osfn_bkgfile, "");
	x_prtype = PR_HOST;

	/* Initialize the kernel file descriptor. */
	zfd[0].fp = stdin;	zfd[0].flags = KF_NOSEEK;
	zfd[1].fp = stdout;	zfd[1].flags = KF_NOSEEK;
	zfd[2].fp = stderr;	zfd[2].flags = KF_NOSEEK;

#ifdef SHLIB
	/* Map in the Sun/IRAF shared library, if the calling process was
	 * linked with the shared library, and the shared library has not
	 * already been mapped (fd != 0).  (See unix/shlib for more info).
	 * (This is rather monolithic and should probably be isolated to a
	 * separate module, but we are not trying for a general solution here).
	 */
	if (USHLIB[0] && (!fd || (fd && fstat(fd,&fi) == -1))) {
	    unsigned int pgsize, pmask;
	    unsigned int t_off, t_len;
	    unsigned int d_off, d_len;
	    unsigned int b_off, b_len;
	    unsigned int b_start, b_bytes;
	    static char envdef[SZ_FNAME];
	    char shimage[SZ_FNAME];
	    const char *shlib, *arch;
	    caddr_t  addr;
	    unsigned int hsize;
#ifdef SOLARIS
	    register Elf32_Phdr *phdr;
	    register Elf32_Ehdr *ehdr;
	    caddr_t t_loc, d_loc, b_loc;
	    int adjust, phnum, nseg, i;
	    struct utsname uts;
	    Elf32_Phdr *phdr_array;
	    Elf32_Phdr seg[32];
	    Elf *elf;
#else
	    unsigned int t_loc, d_loc, b_loc;
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
	    snprintf (envdef, SZ_FNAME, "IRAFARCH=%s", arch);
	    if (!(arch = getenv("IRAFARCH")) || strcmp(envdef,arch))
		putenv (envdef);

#ifdef SOLARIS
	    /* Open the shared library file.  In the case of Solaris the
	     * statically linked shared library doesn't work for both Solaris
	     * 2.3 and 2.4, and a separate shared library is required for
	     * each.  Call uname() to get the OS version and use the 
	     * appropriate shared library.  If this isn't found attempt to
	     * fallback on the generic version.
	     */
	    uname (&uts);
	    snprintf (shimage, SZ_FNAME, "S%d_%s.e", u_version, uts.release);
	    shlib = irafpath (shimage);
	    if (shlib == NULL || (fd = open (shlib, 0)) == -1) {
		snprintf (shimage, SZ_FNAME, "S%d.e", u_version);
		shlib = irafpath (shimage);
	    }
	    if (shlib == NULL || (fd = open (shlib, 0)) == -1) {
		fprintf (stderr,
		    "Error: cannot open iraf shared library %s\n", shlib);
		exit (1);
	    }
#else
	    /* Open the shared library file */
	    snprintf (shimage, SZ_FNAME, "S%d.e", u_version);
	    shlib = irafpath (shimage);
	    if (shlib == NULL || (fd = open (shlib, 0)) == -1) {
		fprintf (stderr,
		    "Error: cannot open iraf shared library %s\n", shlib);
		exit (1);
	    }
#endif

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
		if (phdr->p_type == PT_LOAD) {
		    if ( 32 <= nseg ) {
			fprintf(stderr,"[Warning] nseg exceeds buffer size\n");
			nseg--;
		    }
		    seg[nseg++] = *phdr;
		}
	    }

	    /* Read in the vshlib array, which is stored in the text segment
	     * of the shared image.
	     */
	    if (nseg) {
		phdr = &seg[0];
		hsize = (unsigned int)((char *)VSHLIB) - USHLIB[1];
		/* lseek (fd, phdr->p_offset + (long)hsize, 0); */
		lseek (fd, (off_t)hsize, 0);
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

#else /* !SOLARIS */
	    /* Compute the location and size of each segment of the shared
	     * image memory.  The shared image is mapped at address s_base.
	     */
	    hsize = (unsigned int)((char *)VSHLIB) - USHLIB[1];
	    lseek (fd, (off_t)hsize, 0);
	    if (read (fd, (char *)vshlib, sizeof(vshlib)) != sizeof(vshlib)) {
		fprintf (stderr, "Read error on %s\n", shlib);
		exit (1);
	    }

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
#endif /* SOLARIS */

#ifdef DEBUG
	    fprintf (stderr, " text: %8x %8x %8x -> %8x etext = %8x\n",
		t_loc, t_len, t_off, t_loc + t_len, v_etext);
	    fprintf (stderr, " data: %8x %8x %8x -> %8x edata = %8x\n",
		d_loc, d_len, d_off, d_loc + d_len, v_edata);
	    fprintf (stderr, "  bss: %8x %8x %8x -> %8x   end = %8x\n",
		b_loc, b_len, b_off, b_loc + b_len, v_end);
	    fprintf (stderr, " zero: %8x %8x %8s -> %8x\n",
		b_start, b_bytes, "        ", b_start + b_bytes);
#endif /* DEBUG */

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
	    memset (b_start, 0, b_bytes);

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
#endif /* SHLIB */

	/* Dummy routine called to indicate that mapping is complete. */
	ready();

#if defined(MACOSX) || defined(CYGWIN)
        /*  Clears the exception-occurred bits in the FP status register.
         */
        feclearexcept (FE_ALL_EXCEPT);
#endif	/* MACOSX || CYGWIN */

#if defined(LINUX)
	/* Enable the common IEEE exceptions.  Newer Linux systems disable
	 * these by default, the usual SYSV behavior.
	 */

	/* Old code; replaced by SFPUCW in as$zsvjmp.s 
	    asm ("fclex");
	    setfpucw (0x1372);
	 */
	{   
	    /* 0x332: round to nearest, 64 bit precision, mask P-U-D. */
#ifdef POWERPC
	    XINT fpucw = _FPU_IEEE;
	    SFPUCW (&fpucw);
#else
	    /*
	    XINT fpucw = 0x332;
	    SFPUCW (&fpucw);
	    */
	    fpu_control_t cw = 
		(_FPU_EXTENDED | _FPU_MASK_PM | _FPU_MASK_UM | _FPU_MASK_DM);
	    _FPU_SETCW(cw);
#endif	/* POWERPC */
	}
#endif	/* LINUX */

#ifdef SOLARIS
	/* Enable the common IEEE exceptions.  _ieee_enbint is as$enbint.s.
	 */
#ifdef X86
	fpsetsticky (0x0);
	fpsetmask (FP_X_INV | FP_X_OFL | FP_X_DZ);
#else
	_ieee_enbint (
	    (1 << (int)fp_division) |
	    (1 << (int)fp_overflow) |
	    (1 << (int)fp_invalid)
	);
#endif

#else
#ifdef SUNOS
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
	    const char *set = "set";
	    const char *direction = "direction";
	    const char *precision = "precision";

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
#endif
#endif

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
	{
	    XINT ro = READ_ONLY;
	    XINT wo = WRITE_ONLY;
	    XINT chan;
	    ZOPNTY ((PKCHAR *)U_STDIN, &ro, &chan);
	    ZOPNTY ((PKCHAR *)U_STDOUT, &wo, &chan);
	    ZOPNTY ((PKCHAR *)U_STDERR, &wo, &chan);
	}

	/* Pass the values of the kernel parameters into the kernel. */
	ZZSETK (x_os_process_name, x_osfn_bkgfile, &x_prtype, &x_ipc_isatty, 
		&x_ipc_in /* zero...OK? */, &x_ipc_out /* zero...OK? */);

	return XOK;
}


/* ZZSTOP -- Clean up prior to process shutdown.
 */
int ZZSTOP( void )
{
	return XOK;
}

/* ready -- This is a dummy routine used when debugging to allow a breakpoint
 * to be set at a convenient point after the shared image has been mapped in.
 */
int ready( void )
{
	return XOK;
}
