/*  
 *  CDLP.H -- Private definitions for the Client Display Library.
 */

/*  Default values, size limiting values.
 */
#define	SZ_FNAME		256	/* size of an image name	*/
#define	SZ_NAME			256	/* size of an image name	*/
#define	SZ_LINE			256	/* size of temp buffer		*/
#define	SZ_IMTDEV		128	/* size of an IMTDEV string 	*/
#define SZ_IMCURVAL     	160     /* cursor value str length 	*/
#define SZ_OLD_WCSBUF   	320     /* old wcs buffer length   	*/
#define SZ_WCSBUF       	1024    /* wcs buffer length       	*/

#define	OK			0	/* success			*/
#define	ERR			1	/* failure			*/
#define	EOS			'\0'	/* end of statement		*/



/* Utility macros.
 */
#undef	max
#define	max(a,b)	(a > b ? a : b)
#undef	min
#define	min(a,b)	(a < b ? a : b)

/* Handy values.
 */
#undef	True
#define	True		1
#undef  False
#define False		0


/* Define the structures needed by the IMD interface.  This is a kludge
 * to avoid having applications include the IMD header.
 */

typedef struct {
    int config;                         /* configuration number         */
    int nframes;                        /* number of frames             */
    int width, height;                  /* frame buffer dimensions      */
} FBTab;

struct IMD {
    int     datain, dataout;        	/* connection file descriptors  */
    int     domain;                 	/* connection type              */
    int     model;                 	/* subraster display model	*/

    short   frame;                  	/* display frame number         */
    short   fbconfig;               	/* frame buffer config number   */
    FBTab   *fbtab[MAX_FBCONFIG];   	/* fb configuration table       */

    short   xs, xe;                 	/* X start/end values           */
    short   ys, ye;                 	/* Y start/end values           */
    char    *name;                  	/* image name                   */
    char    *title;                 	/* image title                  */
    float   a, b, c, d;             	/* WCS values                   */
    float   tx, ty;                 	/* translation values           */
    float   z1, z2;                 	/* zscale values                */
    short   ztrans;                 	/* Z trans type                 */

    /* Coordinate mappings on the frame buffer. */
    int     iis_version;            	/* server IIS version   */
    int     iis_valid;              	/* valid mapping flag   */
    char    *ref;        		/* img reference        */
    char    *region;     		/* region name          */
    float   sx, sy;                 	/* source rect          */
    int     snx, sny;
    int     dx, dy;                 	/* destination rect     */
    int     dnx, dny;
};
typedef struct IMD *IMDPtr;



#ifdef CDL_NEED_COLORMAPS

/* Define a grayscale+static colormap used by the print routines. */
static unsigned char cmap_r[] = {
  0,   1,   3,   4,   5,   6,   8,   9,  10,  12,  13,  14,  15,  17,  18,  19,
 20,  22,  23,  24,  26,  27,  28,  29,  31,  32,  33,  35,  36,  37,  38,  40,
 41,  42,  44,  45,  46,  47,  49,  50,  51,  52,  54,  55,  56,  58,  59,  60,
 61,  63,  64,  65,  67,  68,  69,  70,  72,  73,  74,  76,  77,  78,  79,  81,
 82,  83,  84,  86,  87,  88,  90,  91,  92,  93,  95,  96,  97,  99, 100, 101,
102, 104, 105, 106, 108, 109, 110, 111, 113, 114, 115, 116, 118, 119, 120, 122,
123, 124, 125, 127, 128, 129, 131, 132, 133, 134, 136, 137, 138, 140, 141, 142,
143, 145, 146, 147, 148, 150, 151, 152, 154, 155, 156, 157, 159, 160, 161, 163,
164, 165, 166, 168, 169, 170, 172, 173, 174, 175, 177, 178, 179, 180, 182, 183,
184, 186, 187, 188, 189, 191, 192, 193, 195, 196, 197, 198, 200, 201, 202, 204,
205, 206, 207, 209, 210, 211, 212, 214, 215, 216, 218, 219, 220, 221, 223, 224,
225, 227, 228, 229, 230, 232, 233, 234, 236, 237, 238, 239, 241, 242, 243, 244,
246, 247, 248, 250, 251, 252, 253, 255, 255, 255,   0, 255, 255,   0,   0, 255,
  0, 255, 255, 177, 255, 255, 219,   0, 239, 255,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
}; 

static unsigned char cmap_g[] = {
  0,   1,   3,   4,   5,   6,   8,   9,  10,  12,  13,  14,  15,  17,  18,  19,
 20,  22,  23,  24,  26,  27,  28,  29,  31,  32,  33,  35,  36,  37,  38,  40,
 41,  42,  44,  45,  46,  47,  49,  50,  51,  52,  54,  55,  56,  58,  59,  60,
 61,  63,  64,  65,  67,  68,  69,  70,  72,  73,  74,  76,  77,  78,  79,  81,
 82,  83,  84,  86,  87,  88,  90,  91,  92,  93,  95,  96,  97,  99, 100, 101,
102, 104, 105, 106, 108, 109, 110, 111, 113, 114, 115, 116, 118, 119, 120, 122,
123, 124, 125, 127, 128, 129, 131, 132, 133, 134, 136, 137, 138, 140, 141, 142,
143, 145, 146, 147, 148, 150, 151, 152, 154, 155, 156, 157, 159, 160, 161, 163,
164, 165, 166, 168, 169, 170, 172, 173, 174, 175, 177, 178, 179, 180, 182, 183,
184, 186, 187, 188, 189, 191, 192, 193, 195, 196, 197, 198, 200, 201, 202, 204,
205, 206, 207, 209, 210, 211, 212, 214, 215, 216, 218, 219, 220, 221, 223, 224,
225, 227, 228, 229, 230, 232, 233, 234, 236, 237, 238, 239, 241, 242, 243, 244,
246, 247, 248, 250, 251, 252, 253, 255, 255, 255,   0, 255,   0, 255,   0, 255,
255,   0, 127,  48, 166, 247, 112, 246, 131, 232,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0 
};

static unsigned char cmap_b[] = {
  0,   1,   3,   4,   5,   6,   8,   9,  10,  12,  13,  14,  15,  17,  18,  19,
 20,  22,  23,  24,  26,  27,  28,  29,  31,  32,  33,  35,  36,  37,  38,  40,
 41,  42,  44,  45,  46,  47,  49,  50,  51,  52,  54,  55,  56,  58,  59,  60,
 61,  63,  64,  65,  67,  68,  69,  70,  72,  73,  74,  76,  77,  78,  79,  81,
 82,  83,  84,  86,  87,  88,  90,  91,  92,  93,  95,  96,  97,  99, 100, 101,
102, 104, 105, 106, 108, 109, 110, 111, 113, 114, 115, 116, 118, 119, 120, 122,
123, 124, 125, 127, 128, 129, 131, 132, 133, 134, 136, 137, 138, 140, 141, 142,
143, 145, 146, 147, 148, 150, 151, 152, 154, 155, 156, 157, 159, 160, 161, 163,
164, 165, 166, 168, 169, 170, 172, 173, 174, 175, 177, 178, 179, 180, 182, 183,
184, 186, 187, 188, 189, 191, 192, 193, 195, 196, 197, 198, 200, 201, 202, 204,
205, 206, 207, 209, 210, 211, 212, 214, 215, 216, 218, 219, 220, 221, 223, 224,
225, 227, 228, 229, 230, 232, 233, 234, 236, 237, 238, 239, 241, 242, 243, 244,
246, 247, 248, 250, 251, 252, 253, 255, 255, 255,   0, 255,   0,   0, 255,   0,
255, 255,  80,  96,   0, 144, 215, 255, 239, 187,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
  0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,   0
};

#endif


#ifdef CDL_NEED_LINESTYLES

/* Define the linestyle arrays needed in the marker code.  */

#define	HOLLOW_LINE_WIDTH	5
#define	SHADOW_LINE_WIDTH	4

static char dash_pattern[]    = {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0 };
static char dot_pattern[]     = {1, 1, 0, 0, 0, 0 };
static char dotdash_pattern[] = {1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
				 1, 1, 0, 0, 0, 0, 0 };

static int  dash    = 0,    sv_dash    = 0,    p_dash    = 0,
	    dot     = 0,    sv_dot     = 0,    p_dot     = 0,
	    dotdash = 0,    sv_dotdash = 0,    p_dotdash = 0;

#define LEN_DASH	12
#define LEN_DOT		6
#define LEN_DOTDASH	19

#define DASH_PIXEL      (dash_pattern[(dash++)%LEN_DASH])
#define DOT_PIXEL       (dot_pattern[(dot++)%LEN_DOT])
#define DOTDASH_PIXEL   (dotdash_pattern[(dotdash++)%LEN_DOTDASH])

#define	SAVE_DASH_COUNT		sv_dash=dash;
#define	CLEAR_DASH_COUNT	dash=0;
#define	RESTORE_DASH_COUNT	dash=sv_dash;

#define	SAVE_DOT_COUNT		sv_dot=dot;
#define	CLEAR_DOT_COUNT		dot=0;
#define	RESTORE_DOT_COUNT	dot=sv_dot;

#define	SAVE_DOTDASH_COUNT	sv_dotdash=dotdash;
#define	CLEAR_DOTDASH_COUNT	dotdash=0;
#define	RESTORE_DOTDASH_COUNT	dotdash=sv_dotdash;

#endif
