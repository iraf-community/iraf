# Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.

# MWSV.H -- Definitions for the MWSV external save-MWSV data format.  This
# has been generalized slightly over the original "version 0" design, but is
# still not very general and should be redone at some point.  There is an
# implicit assumption that most of the elements of the MWSV structure are
# identical to those in the MWCS runtime descriptor.

define	MWSV_MAGIC	4D57X			# identifies MWSV descriptor
define	MWSV_VERSION	1			# current MWSV version
define	MWSV_MAXWCS	8			# max wcs per mwcs
define	MWSV_LENWCS0	282			# LENWCS for MWSV version 0

# Header for the saved MWCS object.  Object LENs are in the natural units of
# whatever object the field refers to.  Save buffer offsets are type char
# regardless of the object type.  The unused fields at the end of the header
# are reserved for future use and are set to zero in the current version.

define	LEN_SVHDR	16
define	SV_MAGIC	Memi[$1]		# magic marker
define	SV_CWCSLEN	Memi[$1+1]		# length of compressed MWSV
define	SV_MWSVLEN	Memi[$1+2]		# full length of MWSV descr.
define	SV_MWSVOFF	Memi[$1+3]		# char offset of saved MWSV
define	SV_DBUFLEN	Memi[$1+4]		# length of saved DBUF
define	SV_DBUFOFF	Memi[$1+5]		# char offset of saved DBUF
define	SV_SBUFLEN	Memi[$1+6]		# length of saved SBUF
define	SV_SBUFOFF	Memi[$1+7]		# char offset of saved SBUF
define	SV_VERSION	Memi[$1+8]		# MWSV save file version number
define	SV_NWCS		Memi[$1+9]		# number of saved WCS structs
define	SV_LENWCS	Memi[$1+10]		# length of WCS substruct

# MWSV descriptor.  This is very similar to the MWCS runtime descriptor
# except that the size of a WCS sub-structure (LENWCS) can vary.  If the
# MWSV version is 0 lenwcs is fixed at MS_LENWCS0, otherwise the value of
# lenwcs is given in the save header as the value of field SV_LENWCS.

define	MWSV_BASELEN	70
define	LEN_MWSV	(MWSV_BASELEN+($1)*($2))

define	MS_MAGIC	Memi[$1]		# magic marker
define	MS_WCSP		($1+70+(($2)-1)*($3))	# $1=ms $2=wcs $3=lenwcs
