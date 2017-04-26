/*
 * OPCODES.H -- This structure is a template for each instruction in the
 * dictionary.  C_opcode is a constant, from below, and is an index into
 * opcodetbl[]; c_length is the total length, including the opcode, in # of
 * integers; the address of c_args will be the address of the first argument
 * (or if there is just one, it IS the first argument).
 *
 * The intent is to allow invoking the opcode with
 * 	(*opcodetbl[cp->c_opcode]) (&cp->c_args)
 *	where cp is a ptr to struct codeentry.
 */

struct codeentry {
	memel c_opcode;		/* opcodetbl index; see below		*/
	memel c_scriptln;	/* script line of opcode instruction	*/
	memel c_length;		/* total length in memory elements	*/
	memel c_args;		/* addr of this is addr of first arg	*/
};

#define	SZ_CE		4	/* size of codeentry			*/


extern void (*opcodetbl[])();

/* manifest constant opcodes used in c_opcode.
 * value is index into opcodetbl[].
 */

#define	ABSARGSET	1
#define	ADD		2
#define	ADDASSIGN	3
#define	ADDPIPE		4
#define	ALLAPPEND 	5

#define	ALLREDIR	6
#define	AND		7
#define	APPENDOUT	8
#define	ASSIGN		9
#define	BIFF		10

#define	CALL		11
#define CASE		12
#define	CHSIGN		13
#define	CONCAT		14
#define DEFAULT		15

#define	DIV		16
#define DIVASSIGN	17
#define	END		18
#define	EQ		19
#define	EXEC		20

#define FSCAN		21
#define FSCANF		22
#define	GE		23
#define	GOTO		24
#define	GETPIPE		25

#define	GT		26
#define	IMMED		27
#define	INDIRABSSET	28
#define	INDIRPOSSET	29
#define INDXINCR	30

#define	INSPECT		31
#define	INTRINSIC	32
#define	LE		33
#define	LT		34
#define	MUL		35

#define	MULASSIGN	36
#define	NE		37
#define	NOT		38
#define	OR		39
#define	OSESC		40

#define	POSARGSET	41
#define	POW		42
#define	PRINT		43
#define	PUSHCONST	44
#define	PUSHINDEX	45

#define	PUSHPARAM	46
#define	REDIR		47
#define	REDIRIN		48
#define	RMPIPES		49
#define	RETURN		50

#define	SCAN		51
#define	SCANF		52
#define	SUB		53
#define	SUBASSIGN	54
#define	SWITCH		55

#define	SWOFF		56
#define	SWON		57
#define	FIXLANGUAGE	58
#define	GSREDIR		59
#define	CATASSIGN	60


#ifdef OP_DEBUG
/* Opcodes string definitions for debug output.
 */
static char *opstrings[] = {
    "ABSARGSET",   "ADD",       "ADDASSIGN",    "ADDPIPE",      "ALLAPPEND",
    "ALLREDIR",    "AND",       "APPENDOUT",    "ASSIGN",       "BIFF",
    "CALL",        "CASE",      "CHSIGN",       "CONCAT",       "DEFAULT",
    "DIV",         "DIVASSIGN", "END",          "EQ",           "EXEC",
    "FSCAN",       "FSCANF",    "GE",           "GOTO",         "GETPIPE",
    "GT",          "IMMED",     "INDIRABSSET",  "INDIRPOSSET",  "INDXINCR",
    "INSPECT",     "INTRINSIC", "LE",           "LT",           "MUL",
    "MULASSIGN",   "NE",        "NOT",          "OR",           "OSESC",
    "POSARGSET",   "POW",       "PRINT",        "PUSHCONST",    "PUSHINDEX",
    "PUSHPARAM",   "REDIR",     "REDIRIN",      "RMPIPES",      "RETURN",
    "SCAN",        "SCANF",     "SUB",          "SUBASSIGN",    "SWITCH",
    "SWOFF",       "SWON",      "FIXLANGUAGE",  "GSREDIR",      "CATASSIGN",
    ""
};

#define op2str(op) 	((char *)(opstrings[op-1] ? opstrings[op-1] : ""))

#else

#define op2str(op) 	(" ")

#endif
