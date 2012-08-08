# This is the include file for the keyword translation structure.  The 
# pointer in the main package structure is allocated to a length of
# LEN_KEYWSTRUCT and each of the keywords is maintain as a string whose
# maximum length is LEN_KEYWEL (this is usually set by a FITS standard)

define  LEN_KEYWEL	10			# Length of keyword element
define 	LEN_KEYWSTRUCT	(16*LEN_KEYWEL)		# Length of structure

define	KW_RA		Memc[RV_KEYW($1)]
define	KW_DEC		Memc[RV_KEYW($1)+ 1*LEN_KEYWEL+1]
define	KW_UT		Memc[RV_KEYW($1)+ 2*LEN_KEYWEL+1]
define	KW_UTMID	Memc[RV_KEYW($1)+ 3*LEN_KEYWEL+1]
define	KW_EXPTIME	Memc[RV_KEYW($1)+ 4*LEN_KEYWEL+1]
define	KW_EPOCH	Memc[RV_KEYW($1)+ 5*LEN_KEYWEL+1]
define	KW_DATE_OBS	Memc[RV_KEYW($1)+ 6*LEN_KEYWEL+1]
define	KW_HJD		Memc[RV_KEYW($1)+ 7*LEN_KEYWEL+1]
define	KW_MJD_OBS	Memc[RV_KEYW($1)+ 8*LEN_KEYWEL+1]
define	KW_VOBS		Memc[RV_KEYW($1)+ 9*LEN_KEYWEL+1]
define	KW_VREL		Memc[RV_KEYW($1)+10*LEN_KEYWEL+1]
define	KW_VHELIO	Memc[RV_KEYW($1)+11*LEN_KEYWEL+1]
define	KW_VLSR		Memc[RV_KEYW($1)+12*LEN_KEYWEL+1]
define	KW_VSUN		Memc[RV_KEYW($1)+13*LEN_KEYWEL+1]
