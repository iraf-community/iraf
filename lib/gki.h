# GKI.H -- Graphics Kernel Interface (GIO metacode instructions).

define	BOI			-1			# do not parenthesize!
define	GKI_MAXNDC		32767			# maximum GKI coordinate
define	GKI_PACKREAL		(($1)*1E2)		# pack real in an int
define	GKI_UNPACKREAL		(($1)/1E2)		# unpack real from int
define	LEN_GKIDD		29			# device table size

define	TY_INLINE		(-1)			# flag inline kernel
define	TY_FILE			0			# flag normal file
define	KERNEL_PID		(gk_type($1))		# get kernel pid
define	IS_FILE			(gk_type($1)==0)	# kernel is a file
define	IS_INLINE		(gk_type($1)<0)		# kernel is inline
define	IS_SUBKERNEL		(gk_type($1)>0)		# kernel is a subprocess

define	LEN_GKIHDR		3
define	GKI_HDR_BOI		1
define	GKI_HDR_OPCODE		2
define	GKI_HDR_LENGTH		3
define	GKI_DATAFIELDS		4

define	GKI_EOF			0
define	GKI_EOF_LEN		3
define	GKI_EOF_L		3

define	GKI_OPENWS		1
define	GKI_OPENWS_LEN		5
define	GKI_OPENWS_L		3
define	GKI_OPENWS_M		4
define	GKI_OPENWS_N		5
define	GKI_OPENWS_D		6

define	GKI_CLOSEWS		2
define	GKI_CLOSEWS_LEN		4
define	GKI_CLOSEWS_L		3
define	GKI_CLOSEWS_N		4
define	GKI_CLOSEWS_D		5

define	GKI_REACTIVATEWS	3
define	GKI_REACTIVATEWS_LEN	4
define	GKI_REACTIVATEWS_L	3
define	GKI_REACTIVATEWS_F	4

define	GKI_DEACTIVATEWS	4
define	GKI_DEACTIVATEWS_LEN	4
define	GKI_DEACTIVATEWS_L	3
define	GKI_DEACTIVATEWS_F	4

define	GKI_MFTITLE		5
define	GKI_MFTITLE_LEN		4
define	GKI_MFTITLE_L		3
define	GKI_MFTITLE_N		4
define	GKI_MFTITLE_T		5

define	GKI_CLEAR		6
define	GKI_CLEAR_LEN		3
define	GKI_CLEAR_L		3

define	GKI_CANCEL		7
define	GKI_CANCEL_LEN		3
define	GKI_CANCEL_L		3

define	GKI_FLUSH		8
define	GKI_FLUSH_LEN		3
define	GKI_FLUSH_L		3

define	GKI_POLYLINE		9
define	GKI_POLYLINE_LEN	4
define	GKI_POLYLINE_L		3
define	GKI_POLYLINE_N		4
define	GKI_POLYLINE_P		5

define	GKI_POLYMARKER		10
define	GKI_POLYMARKER_LEN	4
define	GKI_POLYMARKER_L	3
define	GKI_POLYMARKER_N	4
define	GKI_POLYMARKER_P	5

define	GKI_TEXT		11
define	GKI_TEXT_LEN		6
define	GKI_TEXT_L		3
define	GKI_TEXT_P		4
define	GKI_TEXT_N		6
define	GKI_TEXT_T		7

define	GKI_FILLAREA		12
define	GKI_FILLAREA_LEN	4
define	GKI_FILLAREA_L		3
define	GKI_FILLAREA_N		4
define	GKI_FILLAREA_P		5

define	GKI_PUTCELLARRAY	13
define	GKI_PUTCELLARRAY_LEN	9
define	GKI_PUTCELLARRAY_L	3
define	GKI_PUTCELLARRAY_LL	4
define	GKI_PUTCELLARRAY_UR	6
define	GKI_PUTCELLARRAY_NC	8
define	GKI_PUTCELLARRAY_NL	9
define	GKI_PUTCELLARRAY_P	10

define	GKI_SETCURSOR		14
define	GKI_SETCURSOR_LEN	6
define	GKI_SETCURSOR_L		3
define	GKI_SETCURSOR_CN	4
define	GKI_SETCURSOR_POS	5

define	GKI_PLSET		15
define	GKI_PLSET_LEN		6
define	GKI_PLSET_L		3
define	GKI_PLSET_LT		4
define	GKI_PLSET_LW		5
define	GKI_PLSET_CI		6

define	GKI_PMSET		16
define	GKI_PMSET_LEN		6
define	GKI_PMSET_L		3
define	GKI_PMSET_MT		4
define	GKI_PMSET_MW		5
define	GKI_PMSET_CI		6

define	GKI_TXSET		17
define	GKI_TXSET_LEN		12
define	GKI_TXSET_L		3
define	GKI_TXSET_UP		4
define	GKI_TXSET_SZ		5
define	GKI_TXSET_SP		6
define	GKI_TXSET_P		7
define	GKI_TXSET_HJ		8
define	GKI_TXSET_VJ		9
define	GKI_TXSET_F		10
define	GKI_TXSET_Q		11
define	GKI_TXSET_CI		12

define	GKI_FASET		18
define	GKI_FASET_LEN		5
define	GKI_FASET_L		3
define	GKI_FASET_FS		4
define	GKI_FASET_CI		5

define	GKI_GETCURSOR		19
define	GKI_GETCURSOR_LEN	4
define	GKI_GETCURSOR_L		3
define	GKI_GETCURSOR_CN	4

define	GKI_CURSORVALUE		19
define	GKI_CURSORVALUE_LEN	10
define	GKI_CURSORVALUE_L	3
define	GKI_CURSORVALUE_CN	4
define	GKI_CURSORVALUE_KEY	5
define	GKI_CURSORVALUE_SX	6
define	GKI_CURSORVALUE_SY	7
define	GKI_CURSORVALUE_RN	8
define	GKI_CURSORVALUE_RX	9
define	GKI_CURSORVALUE_RY	10

define	GKI_GETCELLARRAY	20
define	GKI_GETCELLARRAY_LEN	9
define	GKI_GETCELLARRAY_L	3
define	GKI_GETCELLARRAY_LL	4
define	GKI_GETCELLARRAY_UR	6
define	GKI_GETCELLARRAY_NC	8
define	GKI_GETCELLARRAY_NL	9

define	GKI_CELLARRAY		20
define	GKI_CELLARRAY_LEN	4
define	GKI_CELLARRAY_L		3
define	GKI_CELLARRAY_NP	4
define	GKI_CELLARRAY_P		5

define	GKI_UNKNOWN		24

define	GKI_ESCAPE		25
define	GKI_ESCAPE_LEN		5
define	GKI_ESCAPE_L		3
define	GKI_ESCAPE_FN		4
define	GKI_ESCAPE_N		5
define	GKI_ESCAPE_DC		6

define	GKI_SETWCS		26
define	GKI_SETWCS_LEN		4
define	GKI_SETWCS_L		3
define	GKI_SETWCS_N		4
define	GKI_SETWCS_WCS		5

define	GKI_GETWCS		27
define	GKI_GETWCS_LEN		4
define	GKI_GETWCS_L		3
define	GKI_GETWCS_N		4
