/* File poll structure definitions (c_fpoll).
 */
#ifndef D_fpoll
#define D_fpoll

#define IRAF_POLLIN      0x0001    	/* There is data to read 	*/
#define IRAF_POLLPRI     0x0002    	/* There is urgent data to read */
#define IRAF_POLLOUT     0x0004    	/* Writing now will not block 	*/
#define IRAF_POLLERR     0x0008    	/* Error condition 		*/
#define IRAF_POLLHUP     0x0010    	/* Hung up 			*/
#define IRAF_POLLNVAL    0x0020    	/* Invalid request: fd not open */

#define SZ_POLLFD	 3		/* size of pollfd SPP struct	*/
#define MAX_POLL_FD	32		/* max number of polling fds	*/
#define INFTIM	 	-1		/* poll indefinitely (block)    */

struct _fpoll {
	XINT	fp_fd;			/* file type                    */
	XSHORT	fp_events;		/* file size, machine bytes     */
	XSHORT	fp_revents;		/* time of last access          */
} poll_fds[MAX_POLL_FD];

#ifndef NOLIBCNAMES
#define _IRAF_FPOLL_LIBCNAMES


#ifdef  POLLIN
#undef  POLLIN
#endif
#define POLLIN      IRAF_POLLIN

#ifdef  POLLPRI
#undef  POLLPRI
#endif

#define POLLPRI     IRAF_POLLPRI
#ifdef  POLLOUT
#undef  POLLOUT
#endif

#define POLLOUT     IRAF_POLLOUT
#ifdef  POLLERR
#undef  POLLERR
#endif
#define POLLERR     IRAF_POLLERR

#ifdef  POLLHUP
#undef  POLLHUP
#endif
#define POLLHUP     IRAF_POLLHUP

#ifdef  POLLNVAL
#undef  POLLNVAL
#endif
#define POLLNVAL    IRAF_POLLNVAL

#endif  /* ! NOLIBCNAMES */

#endif
