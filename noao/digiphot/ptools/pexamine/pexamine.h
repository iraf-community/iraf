# The PEXAMINE structure definitions.

# Define the task termination conditions.

define	PX_QUIT		0
define	PX_EXIT		1

# Define the delete indices

define	PX_GOOD		0
define	PX_DELETE	1
define	PX_MARK		2

# Define some useful constants.

define	PX_SZCOLNAME	19	# the maximum length of a column name
define	PX_MAXNCOLS	20	# the maximum number of columns

# Define the default photometry columns.

define	PX_DAOCOLS  ",GROUP,ID,XCENTER,YCENTER,MSKY,STDEV,MAG,MERR,NITER,\
CHI,SHARPNESS,ROUNDNESS"
define	PX_APCOLS  ",ID,XCENTER,YCENTER,MSKY,STDEV,MAG,MERR,"

# Define the structure.

define	LEN_PXSTRUCT (15 +  10 * PX_SZCOLNAME + 10)

define	PX_RNPHOT	Memi[$1]	# number of req'd photometry columns
define	PX_RNUSER	Memi[$1+1]	# number of req'd user columns
define	PX_RNCOLS	Memi[$1+2]	# total number of req'd columns
define	PX_RCOLNAMES	Memi[$1+3]      # ptr to list of req'd column names
define	PX_NPHOT	Memi[$1+4]	# number of photometry columns
define	PX_NUSER	Memi[$1+5]	# number of user columns
define	PX_NCOLS	Memi[$1+6]	# total number of stored columns
define	PX_COLNAMES	Memi[$1+7]      # ptr to list of stored column names
define 	PX_COLPTRS	Memi[$1+8]	# ptr to array of stored column pointers

define	PX_RXCOLNAME	Memc[P2C($1+10)]                  # the req'd x column
define	PX_RYCOLNAME	Memc[P2C($1+10+PX_SZCOLNAME+1)]   # the req'd y column
define	PX_XCOLNAME	Memc[P2C($1+10+2*PX_SZCOLNAME+2)] # the x column
define	PX_YCOLNAME	Memc[P2C($1+10+3*PX_SZCOLNAME+3)] # the y column
define	PX_RXPOSNAME	Memc[P2C($1+10+4*PX_SZCOLNAME+4)] # the req'd xp column
define	PX_RYPOSNAME	Memc[P2C($1+10+5*PX_SZCOLNAME+5)] # the req'd yp column
define	PX_XPOSNAME	Memc[P2C($1+10+6*PX_SZCOLNAME+6)] # the x coord column
define	PX_YPOSNAME	Memc[P2C($1+10+7*PX_SZCOLNAME+7)] # the y coord column
define	PX_RHCOLNAME	Memc[P2C($1+10+8*PX_SZCOLNAME+8)] # the req'd hgm column
define	PX_HCOLNAME	Memc[P2C($1+10+9*PX_SZCOLNAME+9)] # the  hgm column

# Define the colon commands arguments

define	PX_PCMDS "|photcolumns|usercolumns|xcolumn|ycolumn|hcolumn|xposcolumn|\
yposcolumn|eparam|unlearn|x1|x2|y1|y2|marker|szmarker|grid|logx|logy|box|\
ticklabels|majrx|minrx|majry|minry|round|fill|nbins|z1|z2|top_closed|rinner|\
router|ncolumns|nlines|axes|angh|angv|floor|ceiling|zero|ncontours|interval|\
nhi|dashpat|label|delete|"

define	PX_PCMD_PHOTCOLUMNS	1
define	PX_PCMD_USERCOLUMNS	2
define	PX_PCMD_XCOLUMN		3
define	PX_PCMD_YCOLUMN		4
define	PX_PCMD_HCOLUMN		5
define	PX_PCMD_XPOSCOLUMN	6
define	PX_PCMD_YPOSCOLUMN	7
define	PX_PCMD_EDIT		8
define	PX_PCMD_UNLEARN		9
define	PX_PCMD_X1		10
define	PX_PCMD_X2		11
define	PX_PCMD_Y1		12
define	PX_PCMD_Y2		13
define	PX_PCMD_MARKER		14
define	PX_PCMD_SZMARKER	15
define	PX_PCMD_GRID		16
define	PX_PCMD_LOGX		17
define	PX_PCMD_LOGY		18
define	PX_PCMD_BOX		19
define	PX_PCMD_TICKLABELS	20
define	PX_PCMD_MAJRX		21
define	PX_PCMD_MINRX		22
define	PX_PCMD_MAJRY		23
define	PX_PCMD_MINRY		24
define	PX_PCMD_ROUND		25
define	PX_PCMD_FILL		26
define	PX_PCMD_NBINS		27
define	PX_PCMD_Z1		28
define	PX_PCMD_Z2		29
define	PX_PCMD_TOP_CLOSED	30
define	PX_PCMD_RIN		31
define	PX_PCMD_ROUT		32
define	PX_PCMD_NCOLUMNS	33
define	PX_PCMD_NLINES		34
define	PX_PCMD_AXES		35
define	PX_PCMD_ANGH		36
define	PX_PCMD_ANGV		37
define	PX_PCMD_FLOOR		38
define	PX_PCMD_CEILING		39
define	PX_PCMD_ZERO		40
define	PX_PCMD_NCONTOURS	41
define	PX_PCMD_INTERVAL	42
define	PX_PCMD_NHI		43
define	PX_PCMD_DASHPAT		44
define	PX_PCMD_LABEL		45
define	PX_PCMD_DELETE		46

# Define the plot types

define	PX_PLOTTYPES	"|xyplot|histplot|radplot|surfplot|cntrplot|"

define	PX_XYPLOT	1
define	PX_HISTPLOT	2
define	PX_RADPLOT	3
define	PX_SURFPLOT	4
define	PX_CNTRPLOT	5

define PX_MARKERS	"|point|box|plus|cross|circle|hline|vline|diamond|"
