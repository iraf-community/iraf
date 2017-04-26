# FMSET.H -- User definitions for FMIO.

# SET/STAT codes.
define	FM_ACMODE		1	#RO datafile access mode
define	FM_FCACHESIZE		2	#RW number of files in open file cache
define	FM_MAXFBSIZE		3	#RW maximum lfile-FIO buffer size
define	FM_MAXLFILES		4	#RW number of lfiles in datafile
define	FM_MAXPTPAGES		5	#RW max page table pages (max filesize)
define	FM_OPTFBSIZE		6	#RW default lfile-FIO buffer size
define	FM_OSCHAN		7	#RO os channel of datafile
define	FM_PAGESIZE		8	#RW datafile page size, bytes
define	FM_VERSION		9	#RO FMIO version number of datafile

# FM_DEBUG flags.
define	FMD_HEADER		001B	# general header parameters
define	FMD_FTABLE		002B	# summarize file table contents
define	FMD_PTINDEX		004B	# print page table index
define	FMD_PTABLE		010B	# print page table
define	FMD_ALL			017B	# print everything

# FM_FCDEBUG flags.
define	FCD_CACHE		001B	# print current cache status
define	FCD_LFSTATISTICS	002B	# print statistics on lfile getfd's
define	FCD_ALL			003B	# print everything
