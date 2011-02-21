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

define	PS_FD		Memi[$1+00]	# output file descriptor
define	PS_INITIALIZED	Memi[$1+01]	# prolog written flag
define	PS_NUMBER	Memi[$1+02]	# number pages?
define	PS_PNUM		Memi[$1+03]	# current page number
define	PS_JUSTIFY	Memi[$1+04]	# text justification flag

define	PS_PAGE		Memi[$1+06]	# page size 	    (letter|legal|a4|b5)
define	PS_PWIDTH	Memi[$1+07]	# page width	    (points)
define	PS_PHEIGHT	Memi[$1+08]	# page height	    (points)
define	PS_FONTSZ	Memi[$1+09]	# default font size (points)

define	PS_PLMARGIN	Memi[$1+10]	# perm. L margin    (pixres)
define	PS_PRMARGIN	Memi[$1+11]	# perm. R margin    (pixres)
define	PS_PTMARGIN	Memi[$1+12]	# perm. L margin    (pixres)
define	PS_PBMARGIN	Memi[$1+13]	# perm. R margin    (pixres)
define	PS_CLMARGIN	Memi[$1+14]	# current L margin  (pixres)
define	PS_CRMARGIN	Memi[$1+15]	# current R margin  (pixres)
define	PS_PRMPOS	Memi[$1+16]	# perm R margin pos (pixres)
define	PS_CRMPOS	Memi[$1+17]	# cur. R margin pos (pixres)
define	PS_CURPOS	Memi[$1+18]	# current page pos  (pixres)

define	PS_LMARGIN	Memr[P2R($1+20)]# left margin	    (inches)
define	PS_RMARGIN	Memr[P2R($1+21)]# right margin	    (inches)
define	PS_TMARGIN	Memr[P2R($1+22)]# top margin	    (inches)
define	PS_BMARGIN	Memr[P2R($1+23)]# bottom margin	    (inches)

define	PS_HLE		Memi[$1+25]	# header left edge tag str
define	PS_HCE		Memi[$1+26]	# header center tag str
define	PS_HRE		Memi[$1+27]	# header right edge tag str
define	PS_FLE		Memi[$1+28]	# footer left edge tag str
define	PS_FCE		Memi[$1+29]	# footer center tag str
define	PS_FRE		Memi[$1+30]	# footer right edge tag str

define	PS_WBPTR	Memi[$1+31]	# word buffer ptr

# Runtime descriptor.
define  PS_XPOS         Memi[$1+35]     # current page X position
define  PS_YPOS         Memi[$1+36]     # current page Y position
define  PS_CFONT        Memi[$1+37]     # current font type
define  PS_PFONT        Memi[$1+38]     # previous font
define  PS_SFONT        Memi[$1+39]     # special font (forced)
define  PS_CFONT_CH     Memi[$1+40]     # current font code char
define  PS_PFONT_CH     Memi[$1+41]     # special font code char
define  PS_SFONT_CH     Memi[$1+42]     # special font code char
define  PS_LINE_WIDTH   Memi[$1+43]     # current allowable line (points)


# Utility shorthand macros.
define	HLEDGE	     Memc[PS_HLE($1)]	# Header tag strings
define	HCENTER	     Memc[PS_HCE($1)]
define	HREDGE	     Memc[PS_HRE($1)]
define	FLEDGE	     Memc[PS_FLE($1)]	# Footer tag strings
define	FCENTER	     Memc[PS_FCE($1)]
define	FREDGE	     Memc[PS_FRE($1)]
