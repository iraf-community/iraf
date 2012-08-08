# Definitions file for WCSLAB

# Define various important dimensions

define MAX_DIM		10   # Maximum number of dimensions
define N_DIM 		2    # Dimensionality of plotting space 
define N_SIDES    	4    # Number of sides to a window
define MAX_LABEL_POINTS 100  # The maximum number of possible label points
define N_EDGES    	20   # Number of edges being examined from the window

# Define the types of graphs possible.

define GRAPHTYPES	"|normal|polar|near_polar|"
define NORMAL     	1
define POLAR      	2
define NEAR_POLAR	3

# Define the graph sides. The ordering matches the calls to the GIO package.

define	GRAPHSIDES	"|left|right|bottom|top|"
define	LEFT   1
define	RIGHT  2
define	BOTTOM 3
define	TOP    4

# Define which index refers to the X-axis and which refers to the Y-axis.

define X_DIM 1
define Y_DIM 2
define AXIS1 1
define AXIS2 2

# Define which axis is longitude and which axis is latitude.

define LONGITUDE 1
define LATITUDE  2

# Define the available precisions for labelling

define HOUR        1
define DEGREE      1
define MINUTE      2
define SECOND      3
define SUBSEC_LOW  4
define SUBSEC_HIGH 5

# Define the possible MWCS transformation types.

define	RA_DEC_DICTIONARY	"|tan|arc|sin|tnx|"
define	LINEAR_DICTIONARY	"|linear|"

define NUMBER_OF_SUPPORTED_TYPES 2
define RA_DEC                    1
define LINEAR                    2

define AXIS 3B			# transform all axes in any MWCS call

# Some useful graphics definitions and defaults

define NDC_WCS		0	    # the base graphics WCS
define POLE_MARK_SHAPE	4           # the pole mark is a cross
define POLE_MARK_SIZE	3.0         # the half-size of the cross
define DISTANCE_TO_POLE 0.1	    # % distance to pole for lines of longitude
define LINE_SIZE	1.	    # line width for lines and ticks
define MIN_ANGLE	10.	    # minimum angle for text rotation
define BOTTOM_LEFT	.1	    # default bottom left corner of viewport
define TOP_RIGHT	.9	    # default top right corner of viewport

# Units conversion macros

define  RADTOST    (240*RADTODEG($1))      # Radians to seconds of time
define  RADTOSA    (3600*RADTODEG($1))     # Radians to seconds of arc
define  STTORAD    (DEGTORAD(($1)/240))    # Seconds of time to radians
define  SATORAD    (DEGTORAD(($1)/3600))   # Seconds of arc to radians
define  RADTOHRS   (RADTODEG(($1)/15))     # Radians to hours
define  HRSTORAD   (DEGTORAD(15*($1)))     # Hours to radians
define  DEGTOST    (240*($1))              # Degrees to seconds of time.
define  STTODEG    (($1)/240)              # Seconds of time to degrees.
define  DEGTOSA    (3600*($1))             # Degrees to seconds of arc.
define  SATODEG    (($1)/3600)             # Seconds of arc to degrees.
define  HRSTODEG   (($1)*15)               # Hours to degrees.
define  DEGTOHRS   (($1)/15)               # Degrees to hours.
define  STPERDAY   86400                   # Seconds per day

# Other useful macros

define INVERT ($1 < 45 || $1 > 315 || ( $1 > 135 && $1 < 225 ))

# Define the latitudes of the north and south poles

define	NORTH_POLE_LATITUDE 90.0D0
define	SOUTH_POLE_LATITUDE -90.0D0

# Define sections of a circle

define QUARTER_CIRCLE 90.0D0
define HALF_CIRCLE    180.0D0
define FULL_CIRCLE    360.0D0
