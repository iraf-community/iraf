# QPIOSET.H -- QPIO User accessible interface parameters.

define	qpio_stati	qpiost		# (name collision)
define	qpio_statr	qpiovr		# (name collision)
define	qpio_setr	qpiour		# (name collision)

# Read-Write Parameters.
define	QPIO_BLOCKFACTOR	1	# blocking factor for image matrices
define	QPIO_XBLOCKFACTOR	2	# X blocking factor for image matrices
define	QPIO_YBLOCKFACTOR	3	# Y blocking factor for image matrices
define	QPIO_BUCKETLEN		4	# event list bucket size, nevents
define	QPIO_DEBUG		5	# debug level, debug=0 for no messages
define	QPIO_EVXOFF		6	# short offset of X field of event
define	QPIO_EVXTYPE		7	# datatype of X field of event
define	QPIO_EVYOFF		8	# short offset of Y field of event
define	QPIO_EVYTYPE		9	# datatype of Y field of event
define	QPIO_EX			10	# QPEX descriptor (event attr. filter)
define	QPIO_NODEFFILT		11	# disable use of default filter
define	QPIO_NODEFMASK		12	# disable use of default mask
define	QPIO_NOINDEX		13	# flag to disable use of index
define	QPIO_OPTBUFSIZE		14	# optimum buffer size for IMIO/QPF/FIO
define	QPIO_PL			15	# PLIO descriptor (pixel mask)

# Read-Only Parameters.
define	QPIO_EVENTLEN		16	# length of event struct, shorts
define	QPIO_FD			17	# file descriptor of event list lfile
define	QPIO_INDEXLEN		18	# event list index length (0=noindex)
define	QPIO_IXXOFF		19	# short offset of X field used in index
define	QPIO_IXXTYPE		20	# datatype of X field used in index
define	QPIO_IXYOFF		21	# short offset of Y field used in index
define	QPIO_IXYTYPE		22	# datatype of Y field used in index
define	QPIO_LF			23	# lfile in which event list is stored
define	QPIO_MASKP		24	# char pointer to mask-name buffer
define	QPIO_MAXEVP		25	# pointer to MAX-event fields struct
define	QPIO_MINEVP		26	# pointer to MIN-event fields struct
define	QPIO_NCOLS		27	# number of columns in image
define	QPIO_NLINES		28	# number of lines in image
define	QPIO_PARAMP		29	# char pointer to param-name buffer
define	QPIO_QP			30	# backpointer to QPOE descriptor
