#ifndef	_IRAF_XWHEN_H
#define	_IRAF_XWHEN_H

/* Exception handling.
 */

#include <iraf/spp.h>

#define	X_ACV			501	/* access violation		*/
#define	X_ARITH			502	/* arithmetic error		*/
#define	X_INT			503	/* keyboard interrupt		*/
#define	X_IPC			504	/* write to IPC with no reader	*/
#define	X_IGNORE		NULL	/* ignore exception		*/
#define	X_FIRST_EXCEPTION	501

void c_xwhen ( int, XSIGFUNC, XSIGFUNC * );

#endif	/* ! _IRAF_XWHEN_H */
