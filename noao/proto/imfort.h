/* C/IRAF IMIO interface definitions.
 */

#define	XCHAR		short
#define	IM_MAXDIM	7		/* maximum dimension of image	*/
#define	SZ_IMMAGIC	5		/* used to verify structure	*/
#define	SZ_IMPIXFILE	79		/* name of pixel storage file	*/
#define	SZ_IMHDRFILE	79		/* name of header storage file	*/
#define	SZ_IMTITLE	79		/* image title string		*/
#define	SZ_IMHISTORY	511		/* image history record		*/
#define	SZ_BUNIT	9		/* brightness units string	*/
#define	SZ_CTYPE	9		/* coord axes units string	*/
#define	LEN_HGM		20		/* allocation for hgm struct	*/ 
#define	LEN_CTRAN	50		/* allocation for ctran struct	*/

#define	TY_BOOL		1		/* SPP datatypes		*/
#define	TY_CHAR		2
#define	TY_SHORT	3
#define	TY_INT		4
#define	TY_LONG		5
#define	TY_REAL		6
#define	TY_DOUBLE	7
#define	TY_COMPLEX	8


/* The Histogram structure (field IM_HGM) */

struct hgm {
	long	hgm_time;		/* time when hgm was computed	*/
	long	hgm_len;		/* number of bins in hgm	*/
	long	hgm_npix;		/* npix used to compute hgm	*/
	float	hgm_min;		/* min hgm value		*/
	float	hgm_max;		/* max hgm value		*/
	float	hgm_integral;		/* integral of hgm		*/
	float	hgm_mean;		/* mean value			*/
	float	hgm_variance;		/* variance about mean		*/
	float	hgm_skewness;		/* skewness of hgm		*/
	float	hgm_mode;		/* modal value of hgm		*/
	float	hgm_lcut;		/* low cutoff value		*/
	float	hgm_hcut;		/* high cutoff value		*/
};


/* The Coordinate Transformation Structure (IM_CTRAN) */

struct ctran {
	int	ct_valid;		/* (y/n) is structure valid?	*/
	float	ct_bscale;		/* pixval scale factor		*/
	float	ct_bzero;		/* pixval offset		*/
	float	ct_crval;		/* value at pixel		*/
	float	ct_crpix;		/* index of pixel		*/
	float	ct_cdelt;		/* increment along axis		*/
	float	ct_crota;		/* rotation angle		*/
	XCHAR	ct_bunit[SZ_BUNIT+1];	/* pixval ("brightness") units	*/
	XCHAR	ct_ctype[SZ_CTYPE+1];	/* coord units string		*/
};


/* The Image Header Structure */

struct imhdr {
	XCHAR	im_magic[SZ_IMMAGIC+1];		/* verify image header	*/
	int	im_hdrlen;			/* actual header length	*/
	int	im_pixtype;			/* pixel type code	*/
	int	im_ndim;			/* dimension of image	*/
	long	im_len[IM_MAXDIM];		/* axes lengths		*/
	long	im_physlen[IM_MAXDIM];		/* physical axes lengths*/
	long	im_ssmtype;			/* storage mode		*/
	long	im_lutoff;			/* offsets to axes luts	*/
	long	im_pixels;			/* offset to pixels	*/
	long	im_hgmoff;			/* offset to histogram	*/
	long	im_blist;			/* offset to badpix list*/
	long	im_nbpix;			/* number of bad pix	*/
	long	im_ctime;			/* creation time	*/
	long	im_mtime;			/* modify time		*/
	long	im_limtime;			/* time min/max set	*/
	float	im_max;				/* max pixel value	*/
	float	im_min;				/* min pixel value	*/
	int	notused;			/* placeholder		*/
	union {
		struct	hgm im_hgm;		/* histogram structure	*/
		int	hgm_alloc[LEN_HGM];	/* reserved storage	*/
	} h;
	union {
		struct	ctran im_ctran;		/* coord transforms	*/
		int	ct_alloc[LEN_CTRAN];	/* reserved storage	*/
	} ct;
	int	im_pfnum;			/* used by IMIO		*/
	XCHAR	im_pixfile[SZ_IMPIXFILE+1];	/* pixel storage file	*/
	XCHAR	im_hdrfile[SZ_IMHDRFILE+1];	/* header file		*/
	XCHAR	im_title[SZ_IMTITLE+1];		/* image title string	*/
	XCHAR	im_history[SZ_IMHISTORY+1];	/* history string	*/
};
