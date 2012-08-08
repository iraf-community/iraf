/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#ifdef SUNOS

#include <sys/types.h>
#include <sys/time.h>
#include <sys/file.h>
#include <sys/proc.h>
#include <stdio.h>
#include <nlist.h>

#define	SYMBOLS		"/vmunix"
#define	KMEM		"/dev/kmem"


/* UID_EXECUTING -- Search the process table to determine if the given UID
 * belongs to any currently running processes.
 */
int
uid_executing (int uid)
{
	register struct proc *pt;
	register int	found, kmem, i;
	struct proc *get_processtable();
	int	nproc;

	if ((kmem = open (KMEM, 0)) == -1) {
	    fprintf (stderr, "Cannot open kernel memory\n");
	    return (-1);
	} else if ((pt = get_processtable (kmem, &nproc)) == NULL)
	    return (-1);

	for (found=0, i=0;  i < nproc;  i++)
	    if ((&pt[i])->p_stat)
		if ((&pt[i])->p_uid == uid) {
		    found++;
		    break;
		}
	
	free ((char *)pt);
	close (kmem);

	return (found);
}


/* GET_PROCESSTABLE -- Take a snapshot of the current kernel process table.
 */
struct proc *
get_processtable (
    int	kmem,			/* fd of kernel memory file */
    int	*o_nproc		/* number of processes in output table */
)
{
	char	*symbols = SYMBOLS;
	struct	proc *pt = NULL;
	struct	nlist nl[3];
	int	nproc, nb;
	long	proc;

	/* Check that the kernel symbol table file exists. */
	if (access (symbols, R_OK) < 0) {
	    fprintf (stderr, "Cannot open symbol file %s\n", symbols);
	    return (NULL);
	}

	/* Get addresses of symbols '_proc' and '_nproc'. */
	nl[0].n_name = "_proc";
	nl[1].n_name = "_nproc";
	nl[2].n_name = NULL;
	nlist (symbols, nl);
	if (nl[0].n_value == -1) {
	    fprintf (stderr, "Cannot read symbol file %s\n", symbols);
	    return (NULL);
	}

	/* Get values of these symbols from the kernel. */
	lseek (kmem, (long)nl[0].n_value, 0);
	if (read (kmem, &proc, sizeof(proc)) <= 0) {
kerr:	    fprintf (stderr, "Cannot read kernel memory\n");
	    return (NULL);
	}
	lseek (kmem, (long)nl[1].n_value, 0);
	if (read (kmem, &nproc, sizeof(nproc)) <= 0)
	    goto kerr;

	/* Read the kernel process table. */
	if (nproc > 0) {
	    nb = nproc * sizeof(struct proc);
	    pt = (struct proc *) malloc (nb);
	    lseek (kmem, proc, 0);
	    if (read (kmem, pt, nb) < nb)
		goto kerr;
	}

	*o_nproc = nproc;
	return (pt);
}

#else		/* Solaris */

#include <stdio.h>
#include <dirent.h>
#include <sys/types.h>
#include <sys/stat.h>


/* UID_EXECUTING -- Search the process table to determine if the given UID
 * belongs to any currently running processes.  This is straightfoward for
 * Solaris since each process has a file entry in /proc.
 */
int
uid_executing (int uid)
{
	register struct dirent *direntp;
	register DIR *dirp;
	char fname[256];
	struct stat st;

	dirp = opendir ("/proc");
	while ((direntp = readdir(dirp)) != NULL) {
	    sprintf (fname, "/proc/%s", direntp->d_name);
	    if (stat (fname, &st))
		return (0);
	    else if (st.st_uid == uid)
		return (1);
	}
	(void) closedir (dirp);

	return (0);
}

#endif
