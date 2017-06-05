/*	types.h	6.1	83/07/29	*/

/*
 * Basic system types and major/minor device constructing/busting macros.
 */

/* major part of a device */
#define	major(x)	((int)(((unsigned)(x)>>8)&0377))

/* minor part of a device */
#define	minor(x)	((int)((x)&0377))

/* make a device number */
#define	makedev(x,y)	((dev_t)(((x)<<8) | (y)))

typedef	unsigned char	u_char;
typedef	unsigned short	u_short;
typedef	unsigned int	u_int;
typedef	unsigned long	u_long;
typedef	unsigned short	ushort;		/* sys III compat */
typedef unsigned int	u_sock;		/* TCP/IP */

typedef	struct	_quad { long val[2]; } quad;
typedef	long	daddr_t;
typedef	char *	caddr_t;
typedef	u_long	ino_t;
typedef	long	swblk_t;
typedef	int	size_t;
typedef	int	time_t;
typedef	short	dev_t;
typedef	int	off_t;

typedef	struct	fd_set { int fds_bits[1]; } fd_set;
