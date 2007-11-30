# IDB.H -- Image header database interface.  In this version of the interface
# the standard image header fields are maintained in binary in a fixed
# structure and the user fields are maintained in FITS format (text) in the
# a string buffer following the binary image header.

define	IDB_RECLEN		80		# length of a FITS record (card)
define	IDB_STARTVALUE		10		# first column of value field
define	IDB_ENDVALUE		30		# last  column of value field
define	IDB_LENNUMERICRECORD	80		# length of new numeric records
define	IDB_LENSTRINGRECORD	80		# length of new string records
define	IDB_SZFITSKEY		8		# max length FITS keyword

# Standard header keywords accessible via the database interface.

define	I_CTIME			1
define	I_HISTORY		2
define	I_LIMTIME		3
define	I_MAXPIXVAL		4
define	I_MINPIXVAL		5
define	I_MTIME			6
define	I_NAXIS			7
define	I_PIXFILE		8
define	I_PIXTYPE		9
define	I_TITLE			10

define  BEFORE                  1
define  AFTER			2
