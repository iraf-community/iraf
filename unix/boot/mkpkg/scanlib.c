/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#include <stdio.h>

#ifdef i386
#define PORTAR 1
#endif
#include <ar.h>

#define import_spp
#include <iraf.h>
#include "mkpkg.h"
#include "extern.h"

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


/* SCANLIBRARY -- Scan the archive file, extract module names and dates,
 * building the "ar" module list.
 */
h_scanlibrary (library)
char	*library;
{
	register char	*ip, *op;
	register int	i;
	char	libfname[SZ_PATHNAME+1];
	char	modname[SZ_KEY+1];
	char	lbuf[SZ_LINE];
	struct	ar_hdr arf;
	long	length, fdate;
	int	len_arfmag, nmodules;
	FILE	*fp;

	/* Get the library file name. */
	h_getlibname (library, libfname);

	/* Clear the symbol table.
	 */
	mlb_modified = NO;
	mlb_op = 1;
	nmodules = 0;

	for (i=0;  i < MAX_LIBFILES;  i++)
	    mlb_index[i] = 0;

	/* Open the UNIX archive file.
	 */
	if ((fp = fopen (libfname, "r")) == NULL) {
	    printf ("warning: library `%s' not found\n", libfname);
	    fflush (stdout);
	    return (0);
	}

	if (debug) {
	    printf ("scan unix archive %s:\n", libfname);
	    fflush (stdout);
	}

	/* Verify that file is indeed an archive file.
	 */
	fread (lbuf, 1, SARMAG, fp);
	if (strncmp (lbuf, ARMAG, SARMAG) != 0) {
	    printf ("file `%s' is not a library\n", libfname);
	    goto err;
	}

	len_arfmag = strlen (ARFMAG);
	while ((int)(fread (&arf, 1, sizeof(arf), fp)) > 0) {
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
	    for (ip=arf.ar_name;  *ip == ' ';  ip++)
		;
	    for (op=modname;  (*op = *ip++) != ' ' && *op != '/';  op++)
		;
	    *op++ = EOS;

	    /* Skip dummy entry with null modname (COFF format). */
	    if (modname[0] != EOS) {
		/* Get module date.  */
		sscanf (arf.ar_date, "%ld", &fdate);

		/* Insert entry into symbol table. */
		mlb_setdate (modname, fdate);
	    }

	    /* Advance to the next entry.
	     */
	    if (sscanf (arf.ar_size, "%ld", &length) == 1) {
		if (length & 1)				/* must be even */
		    length++;
		fseek (fp, length, 1);
	    } else {
		printf ("could not decode length `%s' of library module\n",
		    arf.ar_size);
		goto err;
	    }
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
h_ardate (fname)
char	*fname;
{
	extern	char *makeobj();
	long	mlb_getdate();

	return (mlb_getdate (makeobj (fname)));
}


/* MLB_SETDATE -- Enter the given module and file date into the symbol table,
 * or update the file date if the module is already present in the table.
 */
mlb_setdate (modname, fdate)
char	*modname;		/* module name		*/
long	fdate;			/* object file date	*/
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
mlb_getdate (modname)
char	*modname;
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
