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
	int c_opcode;		/* opcodetbl index; see below		*/
	int c_length;		/* total length, in ints		*/
	unsigned c_args;	/* addr of this is addr of first arg	*/
};

extern int (*opcodetbl[])();

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
#define	GE		22
#define	GOTO		23
#define	GETPIPE		24
#define	GT		25

#define	IMMED		26
#define	INDIRABSSET	27
#define	INDIRPOSSET	28
#define INDXINCR	29
#define	INSPECT		30

#define	INTRINSIC	31
#define	LE		32
#define	LT		33
#define	MUL		34
#define	MULASSIGN	35

#define	NE		36
#define	NOT		37
#define	OR		38
#define	OSESC		39
#define	POSARGSET	40

#define	POW		41
#define	PRINT		42
#define	PUSHCONST	43
#define	PUSHINDEX	44
#define	PUSHPARAM	45

#define	REDIR		46
#define	REDIRIN		47
#define	RMPIPES		48
#define	RETURN		49
#define	SCAN		50

#define	SUB		51
#define	SUBASSIGN	52
#define	SWITCH		53
#define	SWOFF		54
#define	SWON		55

#define	FIXLANGUAGE	56
#define	GSREDIR		57
#define	CATASSIGN	58
