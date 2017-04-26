# PRDEFS.H -- Parser definitions.


# ----------------------------------------------------------------------
# Severity error codes.
define	PERR_WARNING	1		# warning
define	PERR_SYNTAX	2		# syntax error
define	PERR_SEMANTIC	3		# semantic error
define	PERR_POSTPROC	4		# post processing error


# ----------------------------------------------------------------------
# Equation sections. Equations are divided in sections by the parser, to
# take different semantic actions over expressions depending on the section.

define	PRS_SETEQ	1		# beginning of set equation

define	PRS_TRNREF	2		# transformation reference equation
define	PRS_TRNFIT	3		# transformation fitting equation
define	PRS_TRNDER	4		# transformation derivative equations
define	PRS_TRNPLOT	5		# transformation plot equations

define	PRS_ERREQ	6		# error equation
define	PRS_WTSEQ	7		# weight equation
define	PRS_LMTEQ	8		# limit equations

# -------------------------------------------------------------------------
# Define some default parameter values

define	DEF_PFITDELTA	0.1
