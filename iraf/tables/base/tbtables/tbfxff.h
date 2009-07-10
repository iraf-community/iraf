
#include <iraf/spptypes.h>

#include "underscore.h"	/* appends underscore, if needed */

extern void FTDREC_U (XPOINTER *x_fptr, XINT *keypos, XINT *x_status);
extern void FTCMSG_U();
extern void FSGIOU_U (XPOINTER *x_fptr, XINT *x_status);
extern void FSFIOU_U (XPOINTER *x_fptr, XINT *x_status);
extern void FSCLOS_U (XPOINTER *x_fptr, XINT *x_status);
extern void FSCOPY_U (XPOINTER *x_infptr, XPOINTER *x_outfptr, XINT *morekeys,
		      XINT *x_status);
extern void FSCRHD_U (XPOINTER *x_fptr, XINT *x_status);
extern void FSDHDU_U (XPOINTER *x_fptr, XINT *x_hdutyp, XINT *x_status);
extern void FSDROW_U (XPOINTER *x_fptr, XINT *frow, XINT *nrows, XINT *x_status);
extern void FSGTBB_U (XPOINTER *x_fptr, XINT *frow, XINT *felem, XINT *nbytes,
		      XCHAR array[], XINT *x_status);
extern void FSPTBB_U (XPOINTER *x_fptr, XINT *frow, XINT *felem, XINT *nbytes,
		      XCHAR array[], XINT *x_status);
extern void FSGCL_U (XPOINTER *x_fptr, XINT *colnum,
		     XINT *frow, XINT *felem, XINT *nelem,
		     XINT lray[], XINT *x_status);
extern void FSGCFL_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XINT lray[], XINT flgval[], XINT *x_anynul, XINT *x_status);
extern void FSGCVD_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XDOUBLE *nulval, XDOUBLE array[], XINT *x_anynul, XINT *x_status);
extern void FSGCVE_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XREAL *nulval, XREAL array[], XINT *x_anynul, XINT *x_status);
extern void FSGCVI_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XSHORT *nulval, XSHORT array[], XINT *x_anynul, XINT *x_status);
extern void FSGCVJ_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XINT *nulval, XINT array[], XINT *x_anynul, XINT *x_status);
extern void FSGCVS_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XCHAR nulval[], XCHAR array[], XINT *dim1,
		      XINT *x_anynul, XINT *x_status);
extern void FSGHSP_U (XPOINTER *x_fptr, XINT *x_nexist, XINT *x_nmore, XINT *x_status);
extern void FSGKEY_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
extern void FSGKYD_U (XPOINTER *x_fptr, XCHAR sppkey[], XDOUBLE *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSGMSG_U (XCHAR sppmsg[]);
extern void FSGREC_U (XPOINTER *x_fptr, XINT *nrec, XCHAR spprecord[], XINT *x_status);
extern void FSGRSZ_U (XPOINTER *x_fptr, XINT *maxrows, XINT *x_status);
extern void FSGTDM_U (XPOINTER *x_fptr, XINT *colnum, XINT *maxdim,
		      XINT *x_naxis, XINT naxes[], XINT *x_status);
extern void FSIBIN_U (XPOINTER *x_fptr, XINT *nrows, XINT *nfields,
		      XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
		      XCHAR sppextnam[], XINT *pcount, XINT *x_status);
extern void FSICOL_U (XPOINTER *x_fptr, XINT *colnum,
		      XCHAR sppttype[], XCHAR spptform[], XINT *x_status);
extern void FSINIT_U (XPOINTER *x_fptr, XCHAR sppname[],
		      XINT *blocksize, XINT *x_status);
extern void FSIROW_U (XPOINTER *x_fptr, XINT *frow, XINT *nrows, XINT *x_status);
extern void FSMAHD_U (XPOINTER *x_fptr, XINT *hdunum, XINT *x_exttype, XINT *x_status);
extern void FSMCOM_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppcomm[], XINT *x_status);
extern void FSMKYD_U (XPOINTER *x_fptr, XCHAR sppkey[], XDOUBLE *dval,
		      XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSMKYE_U (XPOINTER *x_fptr, XCHAR sppkey[], XREAL *rval,
		      XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSMKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *intval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMKYL_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *logval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSMREC_U (XPOINTER *x_fptr, XINT *nkey, XCHAR sppcard[], XINT *x_status);
extern void FSOPEN_U (XPOINTER *x_fptr, XCHAR sppname[], XINT *iomode,
		      XINT *blocksize, XINT *x_status);
extern void FSPCLD_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XDOUBLE array[], XINT *x_status);
extern void FSPCLE_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XREAL array[], XINT *x_status);
extern void FSPCLI_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XSHORT array[], XINT *x_status);
extern void FSPCLJ_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XINT array[], XINT *x_status);
extern void FSPCLL_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XINT array[], XINT *x_status);
extern void FSPCLS_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XCHAR array[], XINT *dim1, XINT *x_status);
extern void FSPCLU_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *frow, XINT *felem, XINT *nelem,
		      XINT *x_status);
extern void FSPCOM_U (XPOINTER *x_fptr, XCHAR sppcomm[], XINT *x_status);
extern void FSPHBN_U (XPOINTER *x_fptr, XINT *nrows, XINT *nfields,
		      XCHAR sppttype[], XCHAR spptform[], XCHAR spptunit[],
		      XCHAR sppextnam[], XINT *pcount, XINT *x_status);
extern void FSPHIS_U (XPOINTER *x_fptr, XCHAR sppcomm[], XINT *x_status);
extern void FSPHPR_U (XPOINTER *x_fptr, XINT *simple, XINT *bitpix,
		      XINT *naxis, XLONG naxes[], XLONG *pcount, XLONG *gcount,
		      XINT *extend, XINT *x_status);
extern void FSPKYD_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYE_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XREAL *rval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSPKYL_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XINT *logval, XCHAR sppcomm[], XINT *x_status);
extern void FSPKYS_U (XPOINTER *x_fptr, XCHAR sppkey[], XCHAR sppvalue[],
		      XCHAR sppcomm[], XINT *x_status);
extern void FSPREC_U (XPOINTER *x_fptr, XCHAR sppcard[], XINT *x_status);
extern void FSPSVC_U (XCHAR sppcard[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
extern void FSPTDM_U (XPOINTER *x_fptr, XINT *colnum,
		      XINT *naxis, XINT naxes[], XINT *x_status);
extern void FSRDEF_U (XPOINTER *x_fptr, XINT *x_status);
extern void FSUKYD_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XDOUBLE *dval, XINT *decim, XCHAR sppcomm[], XINT *x_status);
extern void FSUKYJ_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *value,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSUKYL_U (XPOINTER *x_fptr, XCHAR sppkey[], XINT *x_logval,
		      XCHAR sppcomm[], XINT *x_status);
extern void FSUKYS_U (XPOINTER *x_fptr, XCHAR sppkey[],
		      XCHAR sppvalue[], XCHAR sppcomm[], XINT *x_status);
