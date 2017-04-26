/*
 * dirent.h --
 *
 *	Declarations of a library of directory-reading procedures
 *	in the POSIX style ("struct dirent").
 *
 * Copyright (c) 1991 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission is hereby granted, without written agreement and without
 * license or royalty fees, to use, copy, modify, and distribute this
 * software and its documentation for any purpose, provided that the
 * above copyright notice and the following two paragraphs appear in
 * all copies of this software.
 * 
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 *
 * $Header: /user6/ouster/tcl/compat/RCS/dirent2.h,v 1.2 93/03/19 15:25:09 ouster Exp $ SPRITE (Berkeley)
 */

#ifndef _DIRENT
#define _DIRENT

#ifndef _TCL
#include <tcl.h>
#endif

/*
 * Dirent structure, which holds information about a single
 * directory entry.
 */

#define MAXNAMLEN 255
#define DIRBLKSIZ 512

struct dirent {
    long d_ino;			/* Inode number of entry */
    short d_reclen;		/* Length of this record */
    short d_namlen;		/* Length of string in d_name */
    char d_name[MAXNAMLEN + 1];	/* Name must be no longer than this */
};

/*
 * State that keeps track of the reading of a directory (clients
 * should never look inside this structure;  the fields should
 * only be accessed by the library procedures).
 */

typedef struct _dirdesc {
    int dd_fd;
    long dd_loc;
    long dd_size;
    char dd_buf[DIRBLKSIZ];
} DIR;

/*
 * Procedures defined for reading directories:
 */

extern void		closedir _ANSI_ARGS_((DIR *dirp));
extern DIR *		opendir _ANSI_ARGS_((char *name));
extern struct dirent *	readdir _ANSI_ARGS_((DIR *dirp));

#endif /* _DIRENT */
