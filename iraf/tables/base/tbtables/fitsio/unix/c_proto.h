#include <iraf/spptypes.h>

#if defined(NO_UNDERSCORE)

#define FTGCBF_U ftgcbf
#define FTPCBF_U ftpcbf
#define FTGBYT_U ftgbyt
#define FTPBYT_U ftpbyt

#else

#define FTGCBF_U ftgcbf_
#define FTPCBF_U ftpcbf_
#define FTGBYT_U ftgbyt_
#define FTPBYT_U ftpbyt_

#endif

extern int FTPCBF_U(XINT *iunit, XINT *convrt, XINT *nbytes, char *array,
		    XINT *status, ftnlen array_len);
extern int FTGCBF_U(XINT *iunit, XINT *convrt, XINT *nbytes, char *array,
		    XINT *status, ftnlen array_len);
