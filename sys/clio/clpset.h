# CLPSET.H -- CL pset access package header file.

define	SZ_PSPSETNAME	31
define	SZ_PSPARNAME	63

define	LEN_PSETDES	96
define	PS_PSETNAMEP	P2C($1)				# pset name pointer
define	PS_PSETNAME	Memc[P2C($1)]			# pset name
define	PS_PARNAMEP	(P2C($1)+SZ_PSPSETNAME+1)	# pointer to tempbuf
define	PS_PARNAME	Memc[P2C($1)+SZ_PSPSETNAME+1]	# temp buffer

define	PARNAME		Memc[clpset_parname($1,$2)]
