# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# PSIO.H -- Private definitions for the PSIO interface.

# Page size definitions.
define  LETTER_WIDTH    612             # resolutions at 72 points (300 dpi)
define  LETTER_HEIGHT   792
define  LEGAL_WIDTH     612
define  LEGAL_HEIGHT    1008
define  A4_WIDTH        595
define  A4_HEIGHT       850
define  B5_WIDTH        524
define  B5_HEIGHT       765

# Font definitions.
define  FIXED_WIDTH     54              # width of a courier 9-pt font
define  SPACE_WIDTH     30              # width of a 10-point space character
define  FONT_SIZE	10              # default font size (points)
define  START_CH	32              # width table start character
define  END_CH		128             # width table end character
define  LINE_HEIGHT	12              # height of a line (points)

define  RESOLUTION      10              # pixel resolution scale factor
define  PPI             72              # points-per-inch

# Default margins.
define  TMARGIN         1.25             # default margins (inches)
define  BMARGIN         1.0
define  LMARGIN         1.0
define  RMARGIN         1.0


# The main PSIO data structure.
define	LEN_PSSTRUCT	45
define	SZ_WORD		128

define	PS_FD		Memi[P2I($1+00)]	# output file descriptor
define	PS_INITIALIZED	Memi[P2I($1+01)]	# prolog written flag
define	PS_NUMBER	Memi[P2I($1+02)]	# number pages?
define	PS_PNUM		Memi[P2I($1+03)]	# current page number
define	PS_JUSTIFY	Memi[P2I($1+04)]	# text justification flag

define	PS_PAGE		Memi[P2I($1+06)]	# page size 	    (letter|legal|a4|b5)
define	PS_PWIDTH	Memi[P2I($1+07)]	# page width	    (points)
define	PS_PHEIGHT	Memi[P2I($1+08)]	# page height	    (points)
define	PS_FONTSZ	Memi[P2I($1+09)]	# default font size (points)

define	PS_PLMARGIN	Memi[P2I($1+10)]	# perm. L margin    (pixres)
define	PS_PRMARGIN	Memi[P2I($1+11)]	# perm. R margin    (pixres)
define	PS_PTMARGIN	Memi[P2I($1+12)]	# perm. L margin    (pixres)
define	PS_PBMARGIN	Memi[P2I($1+13)]	# perm. R margin    (pixres)
define	PS_CLMARGIN	Memi[P2I($1+14)]	# current L margin  (pixres)
define	PS_CRMARGIN	Memi[P2I($1+15)]	# current R margin  (pixres)
define	PS_PRMPOS	Memi[P2I($1+16)]	# perm R margin pos (pixres)
define	PS_CRMPOS	Memi[P2I($1+17)]	# cur. R margin pos (pixres)
define	PS_CURPOS	Memi[P2I($1+18)]	# current page pos  (pixres)

define	PS_LMARGIN	Memr[P2R($1+20)]	# left margin	    (inches)
define	PS_RMARGIN	Memr[P2R($1+21)]	# right margin	    (inches)
define	PS_TMARGIN	Memr[P2R($1+22)]	# top margin	    (inches)
define	PS_BMARGIN	Memr[P2R($1+23)]	# bottom margin	    (inches)

define	PS_HLE		Memi[P2I($1+25)]	# header left edge tag str
define	PS_HCE		Memi[P2I($1+26)]	# header center tag str
define	PS_HRE		Memi[P2I($1+27)]	# header right edge tag str
define	PS_FLE		Memi[P2I($1+28)]	# footer left edge tag str
define	PS_FCE		Memi[P2I($1+29)]	# footer center tag str
define	PS_FRE		Memi[P2I($1+30)]	# footer right edge tag str

define	PS_WBPTR	Memi[P2I($1+31)]	# word buffer ptr

# Runtime descriptor.
define  PS_XPOS         Memi[P2I($1+35)]     # current page X position
define  PS_YPOS         Memi[P2I($1+36)]     # current page Y position
define  PS_CFONT        Memi[P2I($1+37)]     # current font type
define  PS_PFONT        Memi[P2I($1+38)]     # previous font
define  PS_SFONT        Memi[P2I($1+39)]     # special font (forced)
define  PS_CFONT_CH     Memi[P2I($1+40)]     # current font code char
define  PS_PFONT_CH     Memi[P2I($1+41)]     # special font code char
define  PS_SFONT_CH     Memi[P2I($1+42)]     # special font code char
define  PS_LINE_WIDTH   Memi[P2I($1+43)]     # current allowable line (points)


# Utility shorthand macros.
define	HLEDGE	     Memc[PS_HLE($1)]	# Header tag strings
define	HCENTER	     Memc[PS_HCE($1)]
define	HREDGE	     Memc[PS_HRE($1)]
define	FLEDGE	     Memc[PS_FLE($1)]	# Footer tag strings
define	FCENTER	     Memc[PS_FCE($1)]
define	FREDGE	     Memc[PS_FRE($1)]
