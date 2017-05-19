/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <ctype.h>
#include <sys/stat.h>

#include <ar.h>
#ifdef MACOSX
#include <ranlib.h>
#include <mach-o/fat.h>
#endif

#define import_spp
#include <iraf.h>
#include "mkpkg.h"
#include "extern.h"

#ifdef  OLD_MACOSX
#define AR_EFMT1	1
#endif


/*
 * SCANLIB.C -- Routines to scan a 4.2BSD UNIX archive file and create a
 * symbol table naming the files in the archive and their dates.
 *
 * External entry points:
 *
 *	h_scanlibrary (libname)		extract list of modules and their dates
 *	h_ardate (modname)		return long integer module date
 */

#define	SZ_KEY		128		/* arbitrary			*/
extern	int forceupdate;		/* NOT IMPLEMENTED for UNIX	*/

char	mlb_sbuf[SZ_SBUF];		/* string buffer		*/
int	mlb_op = 0;			/* index into string buffer	*/
int	mlb_index[MAX_LIBFILES];	/* sbuf indices for each symbol	*/
long	mlb_fdate[MAX_LIBFILES];	/* file date of each module	*/
int	mlb_modified;			/* modified flag		*/
char	*mlb_filename();

struct	dbentry {			/* module entry on disk		*/
	long	fdate;
	int	keylen;
	/* key chars */
};


/**
 *  Local procedure declarations.
 */
int mlb_setdate (char *modname, long fdate);



/* SCANLIBRARY -- Scan the archive file, extract module names and dates,
 * building the "ar" module list.
 */
int
h_scanlibrary (char *library)
{
	register char	*ip, *op;
	register int	i, is_fat = 0;
	char	libfname[SZ_PATHNAME+1];
	char	modname[SZ_KEY+1];
	char	lbuf[SZ_LINE];
	struct	ar_hdr arf;
	long	length, fdate;
	int	len=0, len_arfmag, nmodules;
	FILE	*fp;
	struct stat library_stat;

	/* Get the library file name. */
	h_getlibname (library, libfname);

	/* Clear the symbol table.
	 */
	mlb_modified = NO;
	mlb_op = 1;
	nmodules = 0;

	len = 0;
	for (i=0;  i < MAX_LIBFILES;  i++)
	    mlb_index[i] = 0;

	/* Open the UNIX archive file.
	 */
	if ((fp = fopen (libfname, "r")) == NULL) {
	    printf ("warning: library `%s' not found\n", libfname);
	    fflush (stdout);
	    return (0);
	}

	/* Get the file date.
	 */
	stat(libfname, &library_stat);

	if (debug) {
	    printf ("scan unix archive %s:\n", libfname);
	    fflush (stdout);
	}

	/* Verify that file is indeed an archive file.
	 */
	memset (lbuf, 0, SZ_LINE);
	fread (lbuf, 1, SARMAG, fp);
	if (strncmp (lbuf, ARMAG, SARMAG) != 0) {
#ifndef MACOSX
	    printf ("file `%s' is not a library\n", libfname);
	    goto err;
#else
	    /* See if it's a FAT archive file.
	    */
	    struct fat_header fh;
	    struct fat_arch   fa;
	    char *ip;

	    rewind (fp);
	    memset (&fh, 0, sizeof(struct fat_header));
	    fread (&fh, 1, sizeof(struct fat_header), fp);  /* read header */
	    if (fh.magic == FAT_MAGIC || fh.magic == FAT_CIGAM) {
		int   narch = 0;

		is_fat++;

		/* The following is a cheat to avoid byte swapping the
		 * nfat_arch field in Intel systems.  Assumes we'll never
		 * see more that 8-bits worth of architectures. 8-)
		 */
	    	ip = (char *) &fh, ip += 7;
		memmove (&narch, ip, 1);
		for (i=0; i < narch; i++) {	    /* skip headers */
		    memset (&fa, 0, sizeof(struct fat_arch));
	            fread (&fa, 1, sizeof(struct fat_arch), fp);
	        }

		/* Read the AR header.
		 */
		memset (lbuf, 0, SZ_LINE);
		fread (lbuf, 1, SARMAG, fp);
	        if (strncmp (lbuf, ARMAG, SARMAG) != 0) {
	            printf ("file `%s' is not a library\n", libfname);
	            goto err;
	        }
	    } else {
	        printf ("file `%s' is not a library\n", libfname);
	        goto err;
	    }
#endif
	}

	len_arfmag = strlen (ARFMAG);
	memset (&arf, 0, sizeof(arf));
	while ((int)(fread (&arf, 1, sizeof(arf), fp)) > 0) {

	    /* Don't scan past the first architecture for FAT libs.
	     */
	    if (is_fat && strncmp (arf.ar_name, ARMAG, SARMAG) == 0) 
		break;

	    if (strncmp (arf.ar_fmag, ARFMAG, len_arfmag) != 0) {
		printf ("cannot decode library `%s'\n", libfname);
		goto err;
	    }

	    if (debug > 1) {
		char     name[17], date[13];
		strncpy (name, arf.ar_name, 16);  name[16] = '\0';
		strncpy (date, arf.ar_date, 12);  date[12] = '\0'; 
		printf ("objname='%s', date='%s'\n", name, date);
	    }

	    /* Extract module name.  */
	    for (ip=arf.ar_name;  *ip == ' ';  ip++) ;
	    for (op=modname;  (*op = *ip++) != ' ' && *op != '/';  op++) ;
	    *op++ = EOS;

	    /* Skip dummy entry with null modname (COFF format) as well
	     * as the __SYMDEF from ranlib.
	     */
#ifdef MACOSX
	    if (strncmp (modname, RANLIBMAG, 9) || modname[0] != EOS) {
#else
	    if (modname[0] != EOS) {
#endif
#if defined(AR_EFMT1) && !defined(__CYGWIN__)
	        /*
	         * BSD 4.4 extended AR format: #1/<namelen>, with name as the
	         * first <namelen> bytes of the file
	         */
	        if ((arf.ar_name[0] == '#') &&
		    (arf.ar_name[1] == '1') &&
		    (arf.ar_name[2] == '/') && (isdigit(arf.ar_name[3]))) {

		        char p[SZ_PATHNAME];

		        len = atoi(&arf.ar_name[3]);
	                bzero (p, SZ_PATHNAME);
		        if (fread(p, len, 1, fp) != 1) {
		            fprintf (stderr, "%s: premature EOF", libfname);
		        }
	                bzero (modname, SZ_KEY+1);
		        sprintf (modname, "%s", p);
	        } else 
		    len = 0;
#endif
	        /* Get module date.  */
	        sscanf (arf.ar_date, "%ld", &fdate);

		/* Use the library date if the module date is not available */
		if (fdate == 0) {
		  fdate = library_stat.st_mtime;
		}

	        /* Insert entry into symbol table. */
	        mlb_setdate (modname, fdate);
	    } 

	    /* Advance to the next entry.
	     */
	    if (sscanf (arf.ar_size, "%ld", &length) == 1) {
		if (length & 1)				/* must be even */
		    length++;
#if defined(AR_EFMT1) && !defined(__CYGWIN__)
		fseek (fp, length-len, 1);
#else
		fseek (fp, length, 1);
#endif
	    } else {
		printf ("could not decode length `%s' of library module\n",
		    arf.ar_size);
		goto err;
	    }
	
	    memset (&arf, 0, sizeof(arf));
	}

	fclose (fp);
	return (nmodules);

err:
	fflush (stdout);
	fclose (fp);
	return (ERR);
}


/* H_ARDATE -- Look up file in archive.  If found, return date of archive
 * version, otherwise return zero.  This is the entry point called by MKLIB
 * to get the update date of a library module.
 */
long
h_ardate (char *fname)
{
	extern	char *makeobj();
	long	mlb_getdate();

	return (mlb_getdate (makeobj (fname)));
}


/* MLB_SETDATE -- Enter the given module and file date into the symbol table,
 * or update the file date if the module is already present in the table.
 */
int
mlb_setdate (
  char	*modname,		/* module name		*/
  long	fdate 			/* object file date	*/
)
{
	register int	hashval, keylen, i;
	register char	*ip;
	int	start;


	if (*modname == EOS || fdate <= 0) {
	    printf ("warning, mlb_setdate: attempted illegal entry for %s\n",
		modname);
	    fflush (stdout);
	    return (ERR);
	}

	/* Hash the key.
	 */
	for (hashval=0, keylen=0, ip=modname;  *ip;  ip++, keylen++)
	    hashval += hashval + *ip;
	start = hashval % MAX_LIBFILES;

	mlb_modified = YES;

	/* Update the entry if the module is already in the table, else find
	 * an empty slot, checking for table overflow in the process.
	 */
	for (i=start;  mlb_index[i];  ) {
	    ip = &mlb_sbuf[mlb_index[i]];
	    if (*ip == *modname)
		if (strncmp (modname, ip, keylen) == 0) {
		    mlb_fdate[i] = fdate;
		    return (OK);
		}
	    if (++i >= MAX_LIBFILES)
		i = 0;
	    if (i == start) {
		printf ("error: library module list overflow\n");
		fflush (stdout);
		return (ERR);
	    }
	}

	if (mlb_op + keylen + 1 >= SZ_SBUF) {
	    printf ("error: library module list string buffer overflow\n");
	    fflush (stdout);
	    return (ERR);
	}

	/* Enter the module into the symbol table.
	 */
	mlb_index[i] = mlb_op;
	mlb_fdate[i] = fdate;

	strcpy (&mlb_sbuf[mlb_op], modname);
	mlb_op += (keylen + 1);

	return (OK);
}


/* MLB_GETDATE -- Lookup a module in the symbol table and return its date.
 * Return zero if the module is not found.
 */
long
mlb_getdate (char *modname)
{
	register int	hashval, keylen, i;
	register char	*ip;
	int	start;

	if (*modname == EOS)
	    return (0L);

	/* Hash the key.
	 */
	for (hashval=0, keylen=0, ip=modname;  *ip;  ip++, keylen++)
	    hashval += hashval + *ip;
	start = hashval % MAX_LIBFILES;

	/* Search the symbol table for the named module.
	 */
	for (i=start;  mlb_index[i];  ) {
	    ip = &mlb_sbuf[mlb_index[i]];
	    if (*ip == *modname)
		if (strncmp (modname, ip, keylen) == 0)
		    return (mlb_fdate[i]);
	    if (++i >= MAX_LIBFILES)
		i = 0;
	    if (i == start)
		return (0L);
	}

	return (0L);
}
