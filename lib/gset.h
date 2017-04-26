# GSET.H -- GIO internal parameters and magic constants defining possible
# parameter values.

define	G_FD			1		# general graphics parameters
define	G_TTY			2
define	G_WCS			3
define	G_XTRAN			4
define	G_YTRAN			5
define	G_CLIP			6
define	G_CURSOR		7
define	G_SZMARKER1		8
define	G_SZMARKER2		9
define	G_SZMARKER3		10
define	G_SZMARKER4		11
define	G_DEVNAME		12

define	G_PLSTATE		13		# polyline attributes
define	G_PLTYPE		14
define	G_PLWIDTH		15
define	G_PLCOLOR		16

define	G_PMSTATE		17		# polymarker attributes
define	G_PMLTYPE		18
define	G_PMWIDTH		19
define	G_PMCOLOR		20

define	G_TXSTATE		21		# text drawing attributes
define	G_TXUP			22
define	G_TXSIZE		23
define	G_TXPATH		24
define	G_TXSPACING		25
define	G_TXHJUSTIFY		26
define	G_TXVJUSTIFY		27
define	G_TXFONT		28
define	G_TXQUALITY		29
define	G_TXCOLOR		30

define	G_FASTATE		31		# fill area attributes
define	G_FASTYLE		32
define	G_FACOLOR		33

define	G_DRAWTITLE		34		# GLABAX, general parameters
define	G_TITLESIZE		35
define	G_TITLEJUST		36
define	G_NTITLELINES		37
define	G_ASPECT		38
define	G_CHARSIZE		39		# char height in NDC units
define	G_TITLECOLOR		40
define	G_FRAMECOLOR		41
define	G_DRIDCOLOR		42

define	G_RASTER		43		# recent additions


# GLABAX parameters.
# ------------------

define	FIRST_GLABAX_PARAM	101
define	LAST_GLABAX_PARAM	324

define	G_XDRAWAXES		101		# GLABAX, x axis parameters
define	G_XSETAXISPOS		102
define	G_XAXISPOS1		103
define	G_XAXISPOS2		104
define	G_XDRAWGRID		105
define	G_XROUND		106
define	G_XLABELAXIS		107
define	G_XAXISLABELSIZE	108
define	G_XDRAWTICKS		109
define	G_XLABELTICKS		110
define	G_XNMAJOR		111
define	G_XNMINOR		112
define	G_XMAJORLENGTH		113
define	G_XMINORLENGTH		114
define	G_XMAJORWIDTH		115
define	G_XMINORWIDTH		116
define	G_XAXISWIDTH		117
define	G_XTICKLABELSIZE	118
define	G_XTICKFORMAT		119
define	G_XGRIDCOLOR		120
define	G_XAXISLABELCOLOR	121
define	G_XAXISCOLOR		122
define	G_XTICKLABELCOLOR	123
define	G_XTICKCOLOR		124

define	G_YDRAWAXES		201		# GLABAX, y axis parameters
define	G_YSETAXISPOS		202
define	G_YAXISPOS1		203
define	G_YAXISPOS2		204
define	G_YDRAWGRID		205
define	G_YROUND		206
define	G_YLABELAXIS		207
define	G_YAXISLABELSIZE	208
define	G_YDRAWTICKS		209
define	G_YLABELTICKS		210
define	G_YNMAJOR		211
define	G_YNMINOR		212
define	G_YMAJORLENGTH		213
define	G_YMINORLENGTH		214
define	G_YMAJORWIDTH		215
define	G_YMINORWIDTH		216
define	G_YAXISWIDTH		217
define	G_YTICKLABELSIZE	218
define	G_YTICKFORMAT		219
define	G_YGRIDCOLOR		220
define	G_YAXISLABELCOLOR	221
define	G_YAXISCOLOR		222
define	G_YTICKLABELCOLOR	223
define	G_YTICKCOLOR		224

define	G_DRAWAXES		301		# GLABAX, simultaneous x and y
define	G_SETAXISPOS		302
define	G_AXISPOS1		303
define	G_AXISPOS2		304
define	G_DRAWGRID		305
define	G_ROUND			306
define	G_LABELAXIS		307
define	G_AXISLABELSIZE		308
define	G_DRAWTICKS		309
define	G_LABELTICKS		310
define	G_NMAJOR		311
define	G_NMINOR		312
define	G_MAJORLENGTH		313
define	G_MINORLENGTH		314
define	G_MAJORWIDTH		315
define	G_MINORWIDTH		316
define	G_AXISWIDTH		317
define	G_TICKLABELSIZE		318
define	G_TICKFORMAT		319
define	G_GRIDCOLOR		320
define	G_AXISLABELCOLOR	321
define	G_AXISCOLOR		322
define	G_TICKLABELCOLOR	323
define	G_TICKCOLOR		324


# Graphics operand types.
# -------------------------

define	GW_LINEAR		0		# WCS transformation types
define	GW_LOG			1
define	GW_ELOG			2

define	AW_DEFER		100		# Defer OPENWS (gopen mode)
define	AW_PAUSE		1		# De/Reactive ws flags
define	AW_CLEAR		2

define	GL_CLEAR		0		# polyline attribute values
define	GL_SOLID		1
define	GL_DASHED		2
define	GL_DOTTED		3
define	GL_DOTDASH		4

define	GM_FIRSTMARK		1
define	GM_LASTMARK		9

define	GM_POINT		0		# polymarker attribute values
define	GM_FILL			1
define	GM_BOX			2
define	GM_PLUS			4
define	GM_CROSS		8
define	GM_DIAMOND		16
define	GM_HLINE		32
define	GM_VLINE		64
define	GM_HEBAR		128
define	GM_VEBAR		256
define	GM_CIRCLE		512

define	GT_NORMAL		0		# text attribute values
define	GT_CENTER		1
define	GT_LEFT			2
define	GT_RIGHT		3
define	GT_UP			4
define	GT_DOWN			5
define	GT_TOP			6
define	GT_BOTTOM		7
define	GT_ROMAN		8
define	GT_GREEK		9
define	GT_ITALIC		10
define	GT_BOLD			11
define	GT_LOW			12
define	GT_MEDIUM		13
define	GT_HIGH			14

define	GF_CLEAR		0		# fill area attribute values
define	GF_HOLLOW		1
define	GF_SOLID		2
define	GF_HATCH1		3
define	GF_HATCH2		4
define	GF_HATCH3		5
define	GF_HATCH4		6

define	GR_RESETALL		777B		# GRESET flag bits
define	GR_RESETGIO		1
define	GR_RESETWCS		2
define	GR_RESETGLABAX		4
