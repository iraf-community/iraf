/*
 * OPERAND.H -- Definition of an operand, defined operation codes and function
 * type declarations.
 */

/* ----------
 * union of all possible fundamentally allowed data types in an operand
 */
union value {
	int v_i;		/* integer, also doubles as boolean */
	double v_r;		/* floating real; all assumed double precision*/
	char *v_s;		/* char string	*/
	struct arr_desc	*v_a;	/*  Array of int, double or string.	*/
};

struct operand {
	short o_type;		/* need 16 bits; see type codes below	*/
	union value o_val;
};

union arrhead {
	int	*a_i;		/* Pointer to ints (or bools).	*/
	double	*a_r;		/* Pointer to reals.		*/
	char	**a_s;		/* Pointer to strings.		*/
};

struct arr_desc {
	union arrhead a_ptr;	/* Pointer to elements in array.*/
	int	a_dim;		/* Dimensionality of array.	*/
	short	a_len;		/* Length of first dimension.	*/
	short	a_off;		/* Offset of first dimension.	*/
};
/* Note that in an multi-dimensional array a_len and a_off will
 * be repeated for each dimension.
 */


/* this should be the size of operand IN INTS so that the instruction
 * pointer instptr and operand stack index topos can be properly manipulated.
 */
#define	OPSIZ	btoi (sizeof (struct operand))


/* ----------
 * return value of operand *o.
 * not useful for strings as cannot include v_s in this.
 * note that both OT_INT and OT_BOOL use v_i.
 * we assume that o_type only includes OT_BASIC bits.
 */
#define	VALU(o)	(((o)->o_type == OT_REAL) ? (o)->o_val.v_r : (o)->o_val.v_i)


/* ----------
 * o_type flag defn's; also used in p_type, see param.h.
 * the value of o_type&OT_BASIC is the basic type of the operand. there is
 * no such thing as an undefined type, only an undefined value.
 * an operand's o_value is unused if OT_INDEF or UNDEF is set.
 */
#define	OT_BOOL		0	/* actually stored as an int, 0 or 1	*/
#define	OT_INT		1	/* ints store least 16 bits		*/
#define	OT_REAL		2	/* no float/double distinction		*/
#define	OT_STRING	3	/* any kind of in-core char storage	*/
#define	OT_BASIC	03	/* mask to get only the type bits	*/

#define	OT_INDEF	004	/* value is undefined (not an err)	*/
#define	OT_UNDEF	010	/* value is just not known (an err)	*/


/* test and set functions for indefinite and undefined operands.
 * note that the basic type is not disturbed during setting.
 */
#define	opindef(op)	(((op)->o_type & OT_INDEF) != 0)
#define	opundef(op)	(((op)->o_type & OT_UNDEF) != 0)
#define	setopindef(op)	((op)->o_type |= OT_INDEF)
#define	setopundef(op)	((op)->o_type |= OT_UNDEF)


/* ----------
 * binary operations, handled by binop().
 * if these are each in numeric order, the switches in binop(), unop(), etc
 * will be compiled as jump tables.
 */
#define	OP_ADD		1
#define	OP_SUB		2
#define	OP_MUL		3
#define	OP_DIV		4
#define	OP_POW		5	/* power, as in a**x			*/
#define	OP_MAX		6
#define	OP_MIN		7
#define	OP_MOD		8
#define	OP_ATAN2	9	/* arctangent with two args		*/
#define	OP_CONCAT	10	/* string concatenatation		*/
#define OP_RADIX	11	/* string = radix (decimal, newradix)	*/
#define OP_STRIDX	12	/* first occurrence of a char in str	*/
#define OP_STRLDX	13	/* last occurrence of a char in str	*/
#define OP_STRSTR	14	/* first occurrence of str1 in str2	*/
#define OP_STRLSTR	15	/* last occurrence of str1 in str2	*/

/* binary logical expressions, handled by binexp();
 * uses o_val.v_i as boolean result
 */
#define	OP_LT		1
#define	OP_GT		2
#define	OP_LE		3
#define	OP_GE		4
#define	OP_EQ		5
#define	OP_NE		6
#define	OP_OR		7
#define	OP_AND		8

/* unary expressions, handled by unexp(); interprets o_val as boolean */
#define	OP_TRUE		1	/* sets o_val to 1			*/
#define	OP_FALSE	2	/*  "        "   0			*/
#define	OP_NOT		3	/* sets non-0 o_val to 0, 0 to 1	*/

/* unary operations, handled by unop() */
#define	OP_ABS		1	/* absolute value			*/
#define	OP_ACCESS	2	/* does named file exist		*/
#define	OP_COS		3	/* cosine				*/
#define	OP_DEFTASK	4	/* is named task defined		*/
#define	OP_DEFPAR	5	/* is named parameter defined		*/
#define	OP_DEFPAC	6	/* is named package loaded		*/
#define	OP_DEFVAR	7	/* does environment variable exist	*/
#define	OP_ENVGET	8	/* get environment variable defn	*/
#define	OP_EXP		9	/* natural antilog, as in e ** x	*/
#define	OP_FRAC		10	/* fractional part of a real number	*/
#define	OP_IMACCESS	11	/* does named image exist		*/
#define	OP_INT		12	/* coerce to int			*/
#define	OP_LOG		13	/* natural logarithm			*/
#define	OP_LOG10	14	/* decimal logarithm			*/
#define	OP_NSCAN	15	/* number of items conv. in last SCAN	*/
#define	OP_MINUS	16	/* unary negation			*/
#define	OP_MKTEMP	17	/* make unique file name		*/
#define	OP_NINT		18	/* return nearest integer (round)	*/
#define	OP_OSFN		19	/* convert vfn to OS filename		*/
#define OP_REAL		20	/* coerce to real			*/
#define	OP_SIN		21	/* sine					*/
#define	OP_SQRT		22	/* square root				*/
#define	OP_STR		23	/* coercion to type string		*/
#define	OP_SUBSTR	24	/* extract substring			*/
#define	OP_TAN		25	/* tangent				*/
#define OP_STRLEN	26	/* length of a string constant		*/
#define OP_ISINDEF	27	/* is value INDEF			*/
#define OP_STRLWR	28	/* convert string to lower case		*/
#define OP_STRUPR	29	/* convert string to upper case		*/

/* These area used by intrinsic() to categorize the various opcodes.
 * The lower OP_BITS encode the specific function, while bits above that
 * encode the category.  Thus, none of the OP_XXX codes above may use more
 * than OP_BITS, ie, be larger than OP_MASK.
 */
#define	OP_BITS		8
#define	OP_MASK		255	/* could be 2**OP_BITS-1 if C had **	*/
#define	UNOP		(1<<OP_BITS)
#define	BINOP		(2<<OP_BITS)
#define	MULTOP		(3<<OP_BITS)


#define	INTWIDTH	15	/* approx max chars in a printed integer*/
#define	REALWIDTH	25	/* approx max chars in a printed real	*/

extern char *truestr, *falsestr;

struct operand popop(), pushop();
struct operand makeop();
struct operand readlist();	/* read and return operand from list	*/
struct operand sexa();		/* convert n:n:n string to sexagesimal	*/
