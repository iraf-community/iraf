# Input buffer must allow space for tab expansion and standout mode control
# characters.  Word and output buffer dimensions depend on margins.

define	SZ_IBUF		(2*SZ_LINE)
define	MAX_NLS		20		# nesting level for LS
define	MAX_NHLEVEL	10		# max level for numbered sections

# Default formatter parameters.
define	DEF_IHINDENT	4		# .ih indent level
define	DEF_LSINDENT	4		# .ls indent level
define	DEF_IHNSKIP	2		# .ih number of lines to skip
define	DEF_NHNSKIP	2		# .nh number of lines to skip
define	DEF_SHNSKIP	2		# .sh number of lines to skip
define	DEF_TPNLINES	2		# .tp nlines left on page

define	TABSIZE		8
define	INVISIBLE	($1 < BLANK)

# Lroff Directive Opcodes.
define	FI		1		# enter fill mode
define	NF		2		# leave fill mode (nofill)
define	JU		3		# enter line justification mode
define	NJ		4		# leave line justification mode
define	RJ		5		# right justify text on nf,nj line
define	SH		6		# section heading
define	IH		7		# indented section heading
define	NH		8		# numbered section heading
define	BR		9		# break line
define	CE		10		# center next line
define	SP		11		# break, space N spaces on output
define	IN		12		# indent +/- N spaces
define	LS		13		# begin labelled section
define	LE		14		# end labelled section
define	BP		15		# break page
define	TP		16		# test space left on page
define	KS		17		# start floating keep
define	KE		18		# end floating keep
define	HR		19		# HTML href tag
define	HN		20		# HTML name tag
define	ENDHELP		21		# end of help block
define	HELP		22		# start of help block
