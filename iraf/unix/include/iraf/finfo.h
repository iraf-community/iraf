#ifndef	_IRAF_FINFO_H
#define	_IRAF_FINFO_H

/* File info structure definitions (c_finfo).
 */

#include <iraf/spp.h>

#define	SZ_OWNERSTR		16
#define	FI_REGULAR		1	/* file types			*/
#define	FI_DIRECTORY		2
#define	FI_EXECUTABLE		3
#define	FI_SPECIAL		4

struct _finfo {
	XLONG	fi_type;		/* file type			*/
	XLONG	fi_size;		/* file size, machine bytes	*/
	XLONG	fi_atime;		/* time of last access		*/
	XLONG	fi_mtime;		/* time of last modify		*/
	XLONG	fi_ctime;		/* time of file creation	*/
	XLONG	fi_perm;		/* file permission bits		*/
	char	fi_owner[(SZ_OWNERSTR+1)*sizeof(XLONG)];
};

/* ../../../sys/libc/ */
extern int c_finfo ( const char *, struct _finfo * );

#endif	/* ! _IRAF_FINFO_H */
