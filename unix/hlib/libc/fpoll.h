/* File poll structure definitions (c_fpoll).
 */
#define POLLIN      0x0001    		/* There is data to read 	*/
#define POLLPRI     0x0002    		/* There is urgent data to read */
#define POLLOUT     0x0004    		/* Writing now will not block 	*/
#define POLLERR     0x0008    		/* Error condition 		*/
#define POLLHUP     0x0010    		/* Hung up 			*/
#define POLLNVAL    0x0020    		/* Invalid request: fd not open */

#define SZ_POLLFD	 3		/* size of pollfd SPP struct	*/
#define MAX_POLL_FD	32		/* max number of polling fds	*/
#define INFTIM	 	-1		/* poll indefinitely (block)    */

struct _fpoll {
	XINT	fp_fd;			/* file type                    */
	XSHORT	fp_events;		/* file size, machine bytes     */
	XSHORT	fp_revents;		/* time of last access          */
} poll_fds[MAX_POLL_FD];

#define D_fpoll
