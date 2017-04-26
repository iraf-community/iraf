/* 
 * EPS.H -- Include definitions for EPS hardcopy routines.
 */


/* Handy macro definitions */

#define MONO(rd,gn,bl) ((int)(rd*11 + gn*16 + bl*5) >> 5)  /*.33R+ .5G+ .17B*/
#undef MAX
#define MAX(a,b) ((a) > (b) ? (a) : (b))
#undef MIN
#define MIN(a,b) ((a) < (b) ? (a) : (b))


#define	Orientation(ps)		( ps->page.orientation )
#define	Scale(ps)		( ps->page.scale )
#define	MaxScale(ps)		( ps->page.maxscale )
#define	PageType(ps)		( ps->page.page_type )
#define DoAutoScale(ps)		( ps->page.flags & EPS_AUTOSCALE )
#define DoAutoRotate(ps)	( ps->page.flags & EPS_AUTOROTATE )
#define DoMaxAspect(ps)		( ps->page.flags & EPS_MAXASPECT )

#define	PIX_PER_LINE		72

/* Compression options. */
#define	NoCompression		0	/* Don't compress image */
#define	RLECompression		1	/* RLE compression */
#define	LZWCompression		2	/* LZW compression (not implemted) */
#define	JPEGCompression		3	/* JPEG compression (not implemted) */

/* Output color classes. */
#define EPS_GRAYSCALE           0	/* Write a grayscale image */
#define EPS_PSEUDOCOLOR         1	/* Write a pseudocolor image */
#define EPS_TRUECOLOR           2	/* Write a RGB image */

/* Page option flags. */
#define	EPS_PORTRAIT		0	/* Page orientations */
#define	EPS_LANDSCAPE		1
#define EPS_AUTOSCALE		0x01	/* Auto scale to fit on page */
#define	EPS_AUTOROTATE		0x02	/* Auto rotate to fit on page */
#define EPS_MAXASPECT		0x04	/* Increase scale to max aspect */

/* Transformation parameters. */
#define EPS_UNITARY       0
#define EPS_LINEAR        1
#define EPS_LOG           2

/* Page layout structure. */
typedef struct {
	int	orientation;		/* page orioentation */
	float	scale;			/* image scale factor */
	float	maxscale;		/* maxaspect image scale factor */
	int	dpi;			/* dpi resolution */
	int	page_type;		/* type of paper being used */
	int	flags;			/* option flags */
} PSPage, *PSPagePtr;

/* Colormap structure. */
typedef struct {
	int	IsDefault;		/* Are we using the default colormap? */
	int	ncolors;		/* number of colormap colors */
	int	min, max;		/* image min/max */
	unsigned char	r[256];		/* red colormap */
	unsigned char	g[256];		/* green colormap */
	unsigned char	b[256];		/* blue colormap */
	char	*cmap_name;		/* colormap name */
} PSCmap, *PSCmapPtr;

/* Main EPS structure. */
typedef struct {
	int	cols;			/* num output columns */
	int	rows;			/* num output rows */
	int	colorClass;		/* output color class */
	int	compression;		/* output compression type */
	int	annotate;		/* annotate the output image? */
	int	llx, lly, urx, ury;	/* image coordinate corners */
	float	z1, z2;			/* zscale transform values */
	int	ztype;			/* type of transformation */
	float	offset, scale;		/* brightness/contrast values */
	char	*label;			/* label string */

	PSPage	page;			/* Page layout structure */
	PSCmap	cmap;			/* Colormap struct */
} PSImage, *PSImagePtr;


/* Page sizes and resolution information. */
#define EPS_LETTER	0
#define EPS_LEGAL	1
#define EPS_A4		2
#define EPS_B5		3
#define EPS_BSIZE	4
#define EPS_4BY5	5
#define EPS_35MM	6

/* Page layout definitions. */
#define X_ANNOT_MARGIN     30           /* margin in pixels for annotation   */
#define Y_ANNOT_MARGIN     60           /* margin in pixels for annotation   */
#define TITLE_OFFSET	   20		/* Offset for title string           */
#define AXIS_OOFFSET	   7		/* Outer offset for axis             */
#define AXIS_IOFFSET	   2		/* Inner offset for axis             */
#define AXIS_OWIDTH	   1.5		/* Outer axis width                  */
#define AXIS_IWIDTH	   1.0		/* Inner axis width                  */
#define MAJOR_TICK_SIZE    5		/* Size of major tic mark            */
#define MAJOR_TICK_WIDTH   1.5		/* Width of major tic mark           */
#define MINOR_TICK_SIZE    3		/* Size of major tic mark            */
#define MINOR_TICK_WIDTH   0.5		/* Width of major tic mark           */
#define NTICMARKS	   5		/* Number of major tick marks        */

