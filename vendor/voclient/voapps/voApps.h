/**
 *  VOAPPS.h -- Task declarations for the VOClient Package applications.
 *
 *  @file       voApps.h
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Task declarations for the VOClient Package applications.
 */


#include <getopt.h>


#ifdef  SZ_FORMAT
#undef  SZ_FORMAT
#endif
#define	SZ_FORMAT		32

#ifdef  SZ_FNAME
#undef  SZ_FNAME
#endif
#define SZ_FNAME		256

#ifdef  SZ_PATH
#undef  SZ_PATH
#endif
#define SZ_PATH			512

#ifdef  SZ_LINE
#undef  SZ_LINE
#endif
#define	SZ_LINE			4096

#define PARG_ERR		-127

#define	dabs(x)		((x<0.0?-x:x))

/*  Debug and verbose flags.
 */
#define VOAPP_DEBUG  (getenv("VOAPP_DBG")||access("/tmp/VOAPP_DBG",F_OK)==0)
#define VOAPP_VERB   (getenv("VOAPP_VERB")||access("/tmp/VOAPP_VERB",F_OK)==0)

/**
 *  Output formats.
 */
#define FORMATS "|vot|asv|bsv|csv|tsv|html|shtml|fits|ascii|xml|raw|"

#define VOT     0                       /* A new VOTable                */
#define ASV     1                       /* ascii separated values       */
#define BSV     2                       /* bar separated values         */
#define CSV     3                       /* comma separated values       */
#define TSV     4                       /* tab separated values         */
#define HTML    5                       /* standalone HTML document     */
#define SHTML   6                       /* single HTML <table>          */
#define FITS    7                       /* FITS binary table            */
#define ASCII   8                       /* ASV alias                    */
#define XML     9                       /* VOTable alias                */
#define RAW     10                      /*    "      "                  */



/*  SAMP Definitions.
 */
#define SESS_CALLBACK   0               /* mgr sends callback port      */
#define SESS_QUIT       1               /* mgr sends quit/client quits  */
#define SESS_SEND       2               /* mgr/client forwards cmd      */
#define SESS_READY      3               /* mgr ready on port            */

#define SESS_DEFPORT  3000              /* default session mgr port 	*/
#define SESS_DEFHOST  "140.252.1.86"    /* default session mgr host 	*/

#define SAMP_CMD      000               /* message is a command         */
#define SAMP_DATA     001               /* message is data file         */
#define SAMP_RESULT   002               /* message is cmd result        */
#define SAMP_QUIT     004               /* message is quit request      */
#define SAMP_RELAY    010               /* message is relayed           */
#define SAMP_TEST     020               /* message is a test connect    */


/*  File Formats.
 */
#define	VOT_FITS		0
#define	VOT_VOTABLE		1
#define	VOT_FITS_SPEC		2
#define	VOT_VOTABLE_SPEC	3

/*  URL Formats.
 */
#define	VOS_LOCALURL		0	/* e.g. http://127.0.0.1/foo	*/
#define	VOS_LOCALURI		1	/* e.g. file:///path/foo	*/
#define	VOS_LOCALFILE		2	/* e.g. /path/foo		*/
#define	VOS_REMOTE 		3	/* e.g. http://foo.bar/junk	*/



/******************************************************************************
 *  Image information structure.  Note that although we can report 3-D
 *  images, we're really only setup to deal with equatorial sky coord
 *  systems.
 *****************************************************************************/
typedef struct {
    char   *imname;                             /* image name 		    */
    int     is_image;                           /* is it an image?  	    */
    int     is_table;                           /* is it a table?  	    */
    int     has_wcs;                            /* image has wcs 	    */

    int     extnum;                             /* extension number 	    */
    int     naxis;                              /* number of axes 	    */
    int     naxes[3];                           /* axis dimensions 	    */
    int     bitpix;                             /* pixel size 		    */
    int     axflip;                             /* are axes flipped?	    */

    double  xc[4], yc[4];                       /* corner positions (wcs)   */
    double  cx, cy;                             /* center position (wcs)    */
    double  lx, ly;                             /* LL corner (wcs) 	    */
    double  ux, uy;                             /* UR corner (wcs) 	    */
    double  xrval, yrval;                       /* CRVAL values 	    */
    double  xrpix, yrpix;                       /* CRPIX values 	    */

    double  width, height;                      /* image width/height (deg) */
    double  radius;                             /* cone radius (deg) 	    */
    double  rotang;                             /* rotation angle (deg)     */
    double  scale;                              /* plate scale (""/pix)     */
    char    ctype[5];                           /* coordinate type 	    */
} frameInfo, *frameInfoP;

typedef struct {
    char        imname[SZ_PATH];                /* full image name 	    */
    int         nextend;                        /* number of extensions     */

    frameInfo   frame;                          /* full frame information   */
    frameInfo  *extns;                          /* image extn information   */
} ImInfo, *ImInfoP;


ImInfo *vot_imageInfo (char *name, int do_all);
void    vot_printImageInfo (FILE *fd, ImInfo *im);
int     vot_imageNExtns (char *image);
void    vot_freeImageInfo (ImInfo *img);



/*  Task structure.
 */
typedef struct {
   char	 *name;				/* task name		      	*/
   int  (*func)(int argc, char **argv, size_t *len, void **result);

   int   ntests;			/* number of unit tests		*/
   int   npass;				/* number of passed tests	*/
   int   nfail;				/* number of failed tests	*/
} Task;


/*  Tasking execution procedure.
 */
int  vo_runTask (char *method, Task *apps, int argc, char **argv, size_t *len, 
		    void **result);
int  vo_taskTest (Task *self, char *arg, ...);
void vo_taskTestFile (char *str, char *fname);
void vo_taskTestReport (Task self);
void vo_taskDbg (void);

int  vo_setResultFromFile (char *fname, size_t *len, void **data);
int  vo_setResultFromString (char *str, size_t *len, void **data);
int  vo_setResultFromInt (int value, size_t *len, void **data);
int  vo_setResultFromReal (float value, size_t *len, void **data);
int  vo_appendResultFromString (char *str, size_t *len, void **data, 
		size_t *maxlen);



/*  Tasking parameter procedures.
 */
char **vo_paramInit (int argc, char *argv[],
                char *opts, struct option long_opts[]);
int    vo_paramNext (char *opts, struct option long_opts[], 
		int argc, char *argv[], char *optval, int *posindex);
void   vo_paramFree (int argc, char *argv[]);
