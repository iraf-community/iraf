C ========================================================================
C                                                                        =
C  CDLFTN.INC  -- Include file for Fortran programs to give symbolic     =
C  names to various constants used by the CDL.                           =
C                                                                        =
C ========================================================================

	integer	NULL
	integer	FB_AUTO
	integer	CDL_UNITARY, CDL_LINEAR, CDL_LOG

	integer	C_BLACK, C_WHITE, C_RED, C_GREEN, C_BLUE, C_YELLOW
	integer	C_CYAN, C_MAGENTA, C_CORAL, C_MAROON, C_ORANGE, C_KHAKI
	integer C_ORCHID, C_TURQUOISE, C_VIOLET, C_WHEAT

	integer M_FILL, M_POINT, M_BOX, M_PLUS, M_CROSS, M_DIAMOND
	integer M_STAR, M_HLINE, M_VLINE, M_HBLINE, M_VBLINE, M_CIRCLE

	integer F_ROMAN, F_GREEK, F_FUTURA, F_TIMES, F_BOLD

	integer L_SOLID, L_DASHED, L_DOTTED, L_DOTDASH, L_HOLLOW, L_SHADOW

C Commonly used C constants
	parameter (NULL=0)

C Automatically select frame buffer
	parameter (FB_AUTO=-1)

C Types of greyscale transformations.
	parameter (CDL_UNITARY=0)
	parameter (CDL_LINEAR=1)
	parameter (CDL_LOG=2)

C Overlay colors.
	parameter (C_BLACK=202)
	parameter (C_WHITE=203)
	parameter (C_RED=204)
	parameter (C_GREEN=205)
	parameter (C_BLUE=206)
	parameter (C_YELLOW=207)
	parameter (C_CYAN=208)
	parameter (C_MAGENTA=209)
	parameter (C_CORAL=210)
	parameter (C_MAROON=211)
	parameter (C_ORANGE=212)
	parameter (C_KHAKI=213)
	parameter (C_ORCHID=214)
	parameter (C_TURQUOISE=215)
	parameter (C_VIOLET=216)
	parameter (C_WHEAT=217)

C Overlay point mark types.
	parameter (M_FILL=1)
	parameter (M_POINT=2)
	parameter (M_BOX=4)
	parameter (M_PLUS=8)
	parameter (M_CROSS=16)
	parameter (M_DIAMOND=32)
	parameter (M_CIRCLE=64)
	parameter (M_STAR=128)
	parameter (M_HLINE=256)
	parameter (M_VLINE=512)
	parameter (M_HBLINE=1024)
	parameter (M_VBLINE=2048)

C Font types.
	parameter (F_ROMAN=0)
	parameter (F_GREEK=1)
	parameter (F_FUTURA=2)
	parameter (F_TIMES=3)
	parameter (F_BOLD=4)

C Line styles.
	parameter (L_SOLID=0)
	parameter (L_DASHED=1)
	parameter (L_DOTTED=2)
	parameter (L_DOTDASH=3)
	parameter (L_HOLLOW=4)
	parameter (L_SHADOW=5)
