/**
 *  VOAPPSP.h -- Internal declarations for the VOClient Package applications.
 *
 *  @file       voAppsP.h
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Internal declarations for the VOClient Package applications.
 */

#include <unistd.h>

#ifndef SZ_LINE
#define SZ_LINE                4096
#endif
#ifndef SZ_URL
#define SZ_URL                 1024
#endif
#define DEF_SR                  0.1



/* Local processing definitions.
*/
#define MAX_DOWNLOADS            8      /* max downloads to run         */
#define MAX_THREADS            128      /* max threads to run           */
#define MAX_PROCS               64      /* max processes to run         */
#define DEF_DOWNLOADS            1      /* default no. downloads to run */
#define DEF_NTHREADS            16      /* default num threads to run   */
#define DEF_NPROCS              10      /* default num processes to run */
#define DEF_PGID              6200      /* default process group id	*/

#define SZ_TARGET               64      /* size of target name          */
#define DEF_SIZE                0.1     /* default search size (deg)    */

#ifndef FAILED_ONLY
#define FAILED_ONLY             1       /* summarize only failed procs? */
#endif

/* NVO tool contexts.
*/
#define CX_DATA                 0001    /* DAL data access              */
#define CX_REGISTRY             0002    /* Registry resolution          */
#define CX_SESAME               0004    /* Object name resolution       */
#define CX_INVENTORY            0010    /* Data Inventory               */

/* Output file formats.
*/
#define F_ASCII                 0001    /* ASCII table 			*/
#define F_RAW                   0002	/* Raw VOTable			*/
#define F_CSV                   0004	/* Comma-separated-values	*/
#define F_TSV                   0010	/* Tab-separated-values		*/
#define F_FITS                  0020	/* FITS binary table		*/
#define F_HTML                  0040	/* HTML table			*/
#define F_KML                   0100	/* KML placemark table		*/
#define F_XML                   0200	/* XML document			*/
#define F_META                  0400	/* Metadata listing		*/

/* Service Types.
*/
#define SVC_CONE                0001    /* Cone search service type	*/
#define SVC_SIAP                0002	/* SIAP service			*/
#define SVC_SSAP                0004	/* SSAP service			*/
#define SVC_VIZIER              0010	/* Vizier TabularSkyService	*/
#define SVC_SKYNODE             0020	/* Skynode			*/
#define SVC_OTHER               0040	/* Other type of service	*/

/* Table pretty-print defs
*/
#define PP_WIDTH                40	
#define PP_OFFSET               40
#define PP_MAXCHARS             256

/* Registry query buffers
*/
#define SZ_SQL_TERM             4096	
#define SZ_RESULT               40960

/* Range constants.
*/
#define RANGE_ALL		-1
#define RANGE_NONE		-2
#define MAX_RANGES		1024

/* Data type codes.
*/
#define DT_ANY             	000	/* Any or not-specified		*/	
#define DT_CATALOG              001	/* Catalog data			*/
#define DT_IMAGE              	002	/* Image data			*/
#define DT_SPECTRA              004	/* Spectral data		*/
#define DT_RADIO              	010	/* Radio data			*/
#define DT_EVENT              	020	/* Event-based data		*/


/* Extraction types
*/
#define EX_NONE		      0000	/* No extractions		*/
#define EX_ALL		      0777	/* Generate all files		*/
#define EX_BOTH		      0001	/* Get positions and acrefs	*/
#define EX_SAVE		      0002	/* Save intermediate results	*/
#define EX_COLLECT	      0004	/* Collect intermediate results	*/
#define EX_POS		      0010	/* Get positions only		*/
#define EX_ACREF	      0020	/* Get acrefs only		*/
#define EX_HTML		      0040	/* Generate HTML table		*/
#define EX_KML		      0100	/* Generate KML file		*/
#define EX_XML		      0200	/* Generate XML document	*/

/* Error codes
*/
#define E_NONE			0	/* No Error			*/
#define E_NODATA		1	/* No Data Returned		*/
#define E_REQFAIL		2	/* Request Failed		*/
#define E_FILOPEN		3	/* File Open Error		*/
#define E_VOCINIT		4	/* VOClient init failed		*/




/*  Utility macros.
*/
#define VOT_NEXTARG(argc,argv,i) {if(i+1>=argc||(strlen(argv[i+1])>1&&argv[i+1][0]=='-'&&(!isdigit(argv[i+1][1])))){fprintf(stderr,"Error: Option '%s' requires an argument\n",argv[i]);break;}}


typedef int (*PFI)();           	/* ptr to func returning an int */

#ifdef   max
#undef   max
#endif
#define  max(a,b)        	(((a)>(b))?(a):(b))

#ifdef   min
#undef   min
#endif
#define  min(a,b)        	(((a)<(b))?(a):(b))

#ifdef   abs
#undef   abs
#endif
#define  abs(a)        		(((a)<0)?-(a):(a))


/*************************************************************************
** Service calling params.
*/
typedef struct {
    /* Input params.	*/
    char    service_url[SZ_LINE];	/* base service URL		*/
    char    identifier[SZ_LINE];	/* service identifier		*/
    char    name[SZ_LINE];		/* service short name		*/
    char    oname[SZ_LINE];		/* object name			*/
    char    title[SZ_LINE];		/* object name			*/
    char    band[SZ_LINE];		/* BAND param (SSA)		*/
    char    time[SZ_LINE];		/* TIME param (SSA)		*/
    int     type;			/* service type			*/
    int     etype;			/* extraction type		*/
    double  ra;				/* J2000 RA (dec. degrees)	*/
    double  dec;			/* J2000 Dec (dec. degrees) 	*/
    float   sr;				/* search radius (degrees)	*/
    int     fmt;			/* output format		*/
    int     index;			/* output index			*/
    int     svc_index;			/* output service index		*/
    int     obj_index;			/* output object index		*/
} svcParams;


/*************************************************************************
** Object/Position params.
*/
typedef struct {
    char    name[SZ_FNAME];		/* object name			*/
    char    id[SZ_FNAME];		/* object ID 			*/
    double  ra;				/* Right Ascension (J200)	*/
    double  dec;			/* Declination (J200)		*/
    int	    index;			/* list index			*/
    void    *next;			/* linked list pointer		*/
} Object;


/*************************************************************************
**  Range specification used for row/column selection. 
*/
typedef struct {
    char    rstring[SZ_FNAME];		/* range string			*/
    int     ranges[MAX_RANGES];		/* expanded range array		*/
    int     nvalues;			/* num. values in range string	*/
} Range;

Range   colRange;			/* column selection range	*/
Range   rowRange;			/* row selection range	*/
Range   fileRange;			/* acref selection range	*/


/*************************************************************************
** AccessReference or URL params.
*/

#define AC_PENDING			1
#define AC_WORKING			2
#define AC_COMPLETE			3
#define AC_ERROR			-1

typedef struct {
    char    url[SZ_LINE];		/* AcRef or URL			*/
    char    fname[SZ_FNAME];		/* saved output file name	*/
    long    nbytes;			/* size of result		*/
    int     status;			/* error status			*/
    int	    index;			/* list index			*/
    void    *next;			/* linked list pointer		*/
} Acref;


/*************************************************************************
** Process call list for results summary.
*/
typedef struct {
    pid_t   pid;			/* process pid			*/
    Object  *obj;			/* Object 			*/
    int	    status;			/* return status		*/
    int	    count;			/* query result count		*/
    char    root[SZ_FNAME];		/* root file name		*/
    void    *svc;			/* back-pointer			*/
    void    *next;			/* linked list pointer		*/
} Proc;


/*************************************************************************
** DAL Service params.  Each service is comprised of a service URL that
** represents a specific type of DAL service.  Once processed, the 'proc'
** list will contain summary information on each request and the 'acList'
** will contain all the images that need to be downloaded.  Column and
** row selection are applied to each request, the download are done following
** the queries so we can parallelize them across all requested services.
*/
typedef struct {
    char    name[SZ_FNAME];		/* service short name		*/
    char    service_url[SZ_LINE];	/* base service URL		*/
    char    identifier[SZ_LINE];	/* service identifier		*/
    char    title[SZ_LINE];		/* service title string		*/
    int     type;			/* service type			*/
    int	    (*func)(svcParams *p);	/* function to call		*/
    int	    cached;			/* cached resource (NYI)	*/

    int	    count;			/* query result total count	*/
    int	    index;			/* list index			*/

    Proc    *proc;			/* process results list		*/
    int	    nfailed;			/* no. of failed requests	*/
    int	    nnodata;			/* no. of failed requests	*/

    Acref   *acList;			/* acref list for service	*/
    int	    nrefs;			/* no. of acrefs to download	*/

    void    *next;			/* linked list pointer		*/
} Service;


