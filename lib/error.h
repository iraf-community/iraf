# ERROR.H -- Error handling definitions.

define	SZ_XERMSG	SZ_LINE		# max length error message string
define	EA_FATAL	1		# take "fatal" error action
define	EA_ERROR	2		# take "error" error action
define	EA_WARN		3		# print warning message
define	EA_RESTART	-99		# used by the system

define	SYS_XACV	501		# Exceptions
define	SYS_XARITH	502		# ALSO DEFINED in syserr.h
define	SYS_XINT	503
