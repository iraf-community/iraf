#include <iraf/spptypes.h>

#if defined(NO_UNDERSCORE)

#define FSBDEF_U fsbdef
#define FSG2DB_U fsg2db
#define FSG3DB_U fsg3db
#define FSGCFB_U fsgcfb
#define FSGCVB_U fsgcvb
#define FSGGPB_U fsggpb
#define FSGPFB_U fsgpfb
#define FSGPVB_U fsgpvb
#define FSGSFB_U fsgsfb
#define FSGSVB_U fsgsvb
#define FSP2DB_U fsp2db
#define FSP3DB_U fsp3db
#define FSPCLB_U fspclb
#define FSPCNB_U fspcnb
#define FSPGPB_U fspgpb
#define FSPPNB_U fsppnb
#define FSPPRB_U fspprb
#define FSPSSB_U fspssb

#define FTBDEF_U ftbdef
#define FTG2DB_U ftg2db
#define FTG3DB_U ftg3db
#define FTGCFB_U ftgcfb
#define FTGCVB_U ftgcvb
#define FTGGPB_U ftggpb
#define FTGPFB_U ftgpfb
#define FTGPVB_U ftgpvb
#define FTGSFB_U ftgsfb
#define FTGSVB_U ftgsvb
#define FTP2DB_U ftp2db
#define FTP3DB_U ftp3db
#define FTPCLB_U ftpclb
#define FTPCNB_U ftpcnb
#define FTPGPB_U ftpgpb
#define FTPPNB_U ftppnb
#define FTPPRB_U ftpprb
#define FTPSSB_U ftpssb

#else

#define FSBDEF_U fsbdef_
#define FSG2DB_U fsg2db_
#define FSG3DB_U fsg3db_
#define FSGCFB_U fsgcfb_
#define FSGCVB_U fsgcvb_
#define FSGGPB_U fsggpb_
#define FSGPFB_U fsgpfb_
#define FSGPVB_U fsgpvb_
#define FSGSFB_U fsgsfb_
#define FSGSVB_U fsgsvb_
#define FSP2DB_U fsp2db_
#define FSP3DB_U fsp3db_
#define FSPCLB_U fspclb_
#define FSPCNB_U fspcnb_
#define FSPGPB_U fspgpb_
#define FSPPNB_U fsppnb_
#define FSPPRB_U fspprb_
#define FSPSSB_U fspssb_

#define FTBDEF_U ftbdef_
#define FTG2DB_U ftg2db_
#define FTG3DB_U ftg3db_
#define FTGCFB_U ftgcfb_
#define FTGCVB_U ftgcvb_
#define FTGGPB_U ftggpb_
#define FTGPFB_U ftgpfb_
#define FTGPVB_U ftgpvb_
#define FTGSFB_U ftgsfb_
#define FTGSVB_U ftgsvb_
#define FTP2DB_U ftp2db_
#define FTP3DB_U ftp3db_
#define FTPCLB_U ftpclb_
#define FTPCNB_U ftpcnb_
#define FTPGPB_U ftpgpb_
#define FTPPNB_U ftppnb_
#define FTPPRB_U ftpprb_
#define FTPSSB_U ftpssb_

#endif

extern int FSBDEF_U(XINT *ounit, XINT *nfield, XSHORT *tform, XINT *pcount,
		    XINT *nrows, XINT *status);
extern int FSG2DB_U(XINT *ounit, XINT *group, XCHAR *nulval, XINT *dim1,
		    XINT *nx, XINT *ny, void *array, XBOOL *anyflg,
		    XINT *status);
extern int FSG3DB_U(XINT *ounit, XINT *group, XCHAR *nulval, XINT *dim1,
		    XINT *dim2, XINT *nx, XINT *ny, XINT *nz, void *array,
		    XBOOL *anyflg, XINT *status);
extern int FSGCFB_U(XINT *iunit, XINT *colnum, XINT *frow, XINT *felem,
		    XINT *nelem, void *array, XBOOL *flgval, XBOOL *anynul,
		    XINT *status);
extern int FSGCVB_U(XINT *iunit, XINT *colnum, XLONG *frow, XLONG *felem,
		    XLONG *nelem, XCHAR *nulval, void *array, XBOOL *anynul,
		    XINT *status);
extern int FSGGPB_U(XINT *iunit, XINT *group, XINT *fparm, XINT *nparm,
		    void *array, XINT *status);
extern int FSGPFB_U(XINT *iunit, XINT *group, XINT *felem, XINT *nelem,
		    void *array, XBOOL *flgval, XBOOL *anynul, XINT *status);
extern int FSGPVB_U(XINT *iunit, XINT *group, XINT *felem, XINT *nelem,
		    XCHAR *nulval, void *array, XBOOL *anynul, XINT *status);
extern int FSGSFB_U(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes,
		    XINT *fpixel, XINT *lpixel, XINT *inc, void *array,
		    XBOOL *flgval, XBOOL *anyflg, XINT *status);
extern int FSGSVB_U(XINT *iunit, XINT *colnum, XINT *naxis, XINT *naxes,
		    XINT *fpixel, XINT *lpixel, XINT *inc, XCHAR *nulval,
		    void *array, XBOOL *anyflg, XINT *status);
extern int FSP2DB_U(XINT *ounit, XINT *group, XINT *dim1, XINT *nx, XINT *ny,
		    void *array, XINT *status);
extern int FSP3DB_U(XINT *ounit, XINT *group, XINT *dim1, XINT *dim2, XINT *nx,
		    XINT *ny, XINT *nz, void *array, XINT *status);
extern int FSPCLB_U(XINT *ounit, XINT *colnum, XLONG *frow, XLONG *felem,
		    XLONG *nelem, void *array, XINT *status);
extern int FSPCNB_U(XINT *ounit, XINT *colnum, XINT *frow, XINT *felem,
		    XINT *nelem, void *array, XCHAR *nulval, XINT *status);
extern int FSPGPB_U(XINT *ounit, XINT *group, XINT *fparm, XINT *nparm,
		    void *array, XINT *status);
extern int FSPPNB_U(XINT *ounit, XINT *group, XINT *felem, XINT *nelem,
		    void *array, XCHAR *nulval, XINT *status);
extern int FSPPRB_U(XINT *ounit, XINT *group, XINT *felem, XINT *nelem,
		    void *array, XINT *status);
extern int FSPSSB_U(XINT *iunit, XINT *group, XINT *naxis, XINT *naxes,
		    XINT *fpixel, XINT *lpixel, void *array, XINT *status);
