# EXFCN.H - Include file for the special functions supported by the EXPORT task.

# Outbands expressions functions.
define	OB_FUNCTIONS	"|band|line|flipx|flipy|\
			 |cmap|setcmap|psdpi|psscale|\
			 |zscale|grey|gray|bscale|gamma|\
			 |block|"

define	BAND		1		# force band-interleaved storage
define	LINE		2		# force line-interleaved storage
define	FLIPX		3		# flip image left-to-right
define	FLIPY		4		# flip image top-to-bottom
#newline
define	CMAP		6		# create 8-bit colormap
define	SETCMAP		7		# apply a colormap
define	PSDPI		8		# set dpi for output
define	PSSCALE		9		# set scale of PS output
#newline
define	ZSCALE		11		# scale to a fixed number of bins
define	GREY		12		# RGB to greyscale conversion
define	GRAY		13		#  "   "      "        "
define	BSCALE		14		# linearly transform intensity scale
define	GAMMA		15		# apply a gamma correction
#newline
define	BLOCK		17		# floodfill a block w/ a constant
