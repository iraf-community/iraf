# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

include <mach.h>


define	SZ_FNT		32768
define	CH_DELIM	20B		    # used to flag image section

define	IMT_FILE	0		    # file list
define	IMT_IMAGE	1		    # image list
define	IMT_TABLE	2		    # table list (ascii file)
define	IMT_VOTABLE	3		    # table list (XML file)
define	IMT_URL		4		    # file URL
define	IMT_DIR		5		    # directory

define  IMT_OUTPUTS     "|none|list|file|"  # expansion options
define  IMTY_NONE       1                   # No output
define  IMTY_LIST       2                   # List output
define  IMTY_FILE       3                   # File output

define  SZ_RANGE        100                 # Size of extension range list
define  SZ_LISTOUT      16384               # Size of extension output list

define  FIRST   	1                   # Default starting range
define  LAST    	MAX_INT             # Default ending range
define  STEP    	1                   # Default step
define  EOLIST  	-1                  # End of list

