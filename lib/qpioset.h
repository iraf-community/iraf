# QPIOSET.H -- QPIO User accessible interface parameters.

define	qpio_stati	qpiost		# (name collision with qpio_seti)

# Read-Write Parameters.
define	QPIO_BLOCKFACTOR	1	# blocking factor for image matrices
define	QPIO_BUCKETLEN		2	# event list bucket size, nevents
define	QPIO_DEBUG		3	# debug level, debug=0 for no messages
define	QPIO_EVXOFF		4	# short offset of X field of event
define	QPIO_EVYOFF		5	# short offset of Y field of event
define	QPIO_EX			6	# QPEX descriptor (event attr. filter)
define	QPIO_NODEFFILT		7	# disable use of default filter
define	QPIO_NODEFMASK		8	# disable use of default mask
define	QPIO_NOINDEX		9	# flag to disable use of index
define	QPIO_OPTBUFSIZE		10	# optimum buffer size for IMIO/QPF/FIO
define	QPIO_PL			11	# PLIO descriptor (pixel mask)

# Read-Only Parameters.
define	QPIO_EVENTLEN		12	# length of event struct, shorts
define	QPIO_FD			13	# file descriptor of event list lfile
define	QPIO_INDEXLEN		14	# event list index length (0=noindex)
define	QPIO_IXXOFF		15	# short offset of X field used in index
define	QPIO_IXYOFF		16	# short offset of Y field used in index
define	QPIO_LF			17	# lfile in which event list is stored
define	QPIO_MASKP		18	# char pointer to mask-name buffer
define	QPIO_MAXEVP		19	# pointer to MAX-event fields struct
define	QPIO_MINEVP		20	# pointer to MIN-event fields struct
define	QPIO_NCOLS		21	# number of columns in image
define	QPIO_NLINES		22	# number of lines in image
define	QPIO_PARAMP		23	# char pointer to param-name buffer
define	QPIO_QP			24	# backpointer to QPOE descriptor
