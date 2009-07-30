
#include <iraf/spptypes.h>

#include "underscore.h"	/* appends underscore, if needed */

extern void FTDREC_U (XINT *funit, XINT *keypos, XINT *x_status);
extern void FTCMSG_U();
extern void FSGIOU_U (XINT *funit, XINT *x_status);
extern void FSFIOU_U (XINT *funit, XINT *x_status);
extern void FSCLOS_U (XINT *funit, XINT *x_status);
extern void FSCOPY_U (XINT *in_funit, XINT *out_funit, XINT *morekeys,
		      XINT *x_status);
extern void FSCRHD_U (XINT *funit, XINT *x_status);
extern void FSDHDU_U (XINT *funit, XINT *x_hdutyp, XINT *x_status);
extern void FSDROW_U (XINT *funit, XLONG *frow, XLONG *nrows, XINT *x_status);
extern void FSGTBB_U (XINT *funit, XLONG *frow, XLONG *felem,
		      XLONG *nbytes, XCHAR array[], XINT *x_status);
extern void FSPTBB_U (XINT *funit, XLONG *frow, XLONG *felem, XLONG *nbytes,
		      XCHAR array[], XINT *x_status);
extern void FSGCL_U (XINT *funit, XINT *colnum,
		     XLONG *frow, XLONG *felem, XLONG *nelem,
		     XBOOL lray[], XINT *x_status);
extern void FSGCFL_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XBOOL lray[], XBOOL flgval[], XBOOL *x_anynul, XINT *x_status);
extern void FSGCVD_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XDOUBLE *nulval, XDOUBLE array[], XBOOL *x_anynul, XINT *x_status);
extern void FSGCVE_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XREAL *nulval, XREAL array[], XBOOL *x_anynul, XINT *x_status);
extern void FSGCVI_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XSHORT *nulval, XSHORT array[], XBOOL *x_anynul, XINT *x_status);
extern void FSGCVJ_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XINT *nulval, XINT array[], XBOOL *x_anynul, XINT *x_status);
extern void FSGCVS_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XCHAR nulval[], XCHAR array[], XINT *dim1,
		      XBOOL *x_anynul, XINT *x_status);
extern void FSGHSP_U (XINT *funit, XINT *x_nexist, XINT *x_nmore, XINT *x_status);
extern void FSGKEY_U (XINT *funit, XCHAR sppkey[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
extern void FSGKYD_U (XINT *funit, XCHAR sppkey[], XDOUBLE *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGKYJ_U (XINT *funit, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGKYK_U (XINT *funit, XCHAR sppkey[], XLONG *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGKYS_U (XINT *funit, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGMSG_U (XCHAR sppmsg[]);
extern void FSGREC_U (XINT *funit, XINT *nrec, XCHAR spprecord[], XINT *x_status);
extern void FSGRSZ_U (XINT *funit, XLONG *maxrows, XINT *x_status);
extern void FSGTDM_U (XINT *funit, XINT *colnum, XINT *maxdim,
		      XINT *x_naxis, XLONG naxes[], XINT *x_status);
extern void FSIBIN_U (XINT *funit, XLONG *nrows, XINT *nfields,
		      XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
		      XCHAR sppextnam[], XLONG *pcount, XINT *x_status);
extern void FSICOL_U (XINT *funit, XINT *colnum,
		      XCHAR sppttype[], XCHAR spptform[], XINT *x_status);
extern void FSINIT_U (XINT *funit, XCHAR sppname[],
		      XINT *blocksize, XINT *x_status);
extern void FSIROW_U (XINT *funit, XLONG *frow, XLONG *nrows, XINT *x_status);
extern void FSMAHD_U (XINT *funit, XINT *hdunum, XINT *x_exttype, XINT *x_status);
extern void FSMCOM_U (XINT *funit, XCHAR sppkey[], XCHAR sppcomm[], XINT *x_status);
extern void FSMKYD_U (XINT *funit, XCHAR sppkey[], XDOUBLE *dval,
		      XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSMKYE_U (XINT *funit, XCHAR sppkey[], XREAL *rval,
		      XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSMKYJ_U (XINT *funit, XCHAR sppkey[], XINT *intval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMKYK_U (XINT *funit, XCHAR sppkey[], XLONG *intval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMKYL_U (XINT *funit, XCHAR sppkey[], XBOOL *logval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMKYS_U (XINT *funit, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMREC_U (XINT *funit, XINT *nkey, XCHAR sppcard[], XINT *x_status);
extern void FSOPEN_U (XINT *funit, XCHAR sppname[], XINT *iomode,
		      XINT *blocksize, XINT *x_status);
extern void FSPCLD_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XDOUBLE array[], XINT *x_status);
extern void FSPCLE_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XREAL array[], XINT *x_status);
extern void FSPCLI_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XSHORT array[], XINT *x_status);
extern void FSPCLJ_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XINT array[], XINT *x_status);
extern void FSPCLL_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XBOOL array[], XINT *x_status);
extern void FSPCLS_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XCHAR array[], XINT *dim1, XINT *x_status);
extern void FSPCLU_U (XINT *funit, XINT *colnum,
		      XLONG *frow, XLONG *felem, XLONG *nelem,
		      XINT *x_status);
extern void FSPCOM_U (XINT *funit, XCHAR sppcomm[], XINT *x_status);
extern void FSPHBN_U (XINT *funit, XLONG *nrows, XINT *nfields,
		      XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
		      XCHAR sppextnam[], XLONG *pcount, XINT *x_status);
extern void FSPHIS_U (XINT *funit, XCHAR sppcomm[], XINT *x_status);
extern void FSPHPR_U (XINT *funit, XBOOL *simple, XINT *bitpix,
		      XINT *naxis, XLONG naxes[], XLONG *pcount, XLONG *gcount,
		      XBOOL *extend, XINT *x_status);
extern void FSPKYD_U (XINT *funit, XCHAR sppkey[],
		      XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYE_U (XINT *funit, XCHAR sppkey[],
		      XREAL *rval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYJ_U (XINT *funit, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSPKYL_U (XINT *funit, XCHAR sppkey[],
		      XBOOL *logval, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYS_U (XINT *funit, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSPREC_U (XINT *funit, XCHAR sppcard[], XINT *x_status);
extern void FSPSVC_U (XCHAR sppcard[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
extern void FSPTDM_U (XINT *funit, XINT *colnum,
		      XINT *naxis, XLONG naxes[], XINT *x_status);
extern void FSRDEF_U (XINT *funit, XINT *x_status);
extern void FSUKYD_U (XINT *funit, XCHAR sppkey[],
		      XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSUKYJ_U (XINT *funit, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSUKYL_U (XINT *funit, XCHAR sppkey[], XBOOL *x_logval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSUKYS_U (XINT *funit, XCHAR sppkey[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
