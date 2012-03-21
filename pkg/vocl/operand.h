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
#define	OP_DATAN2	10	/* arctangent with result in degrees	*/
#define	OP_FPEQUAL	11	/* floating point comparison		*/
#define OP_HYPOT	12	/* euclidean distance			*/
#define OP_BAND		13	/* bitwise AND operator			*/
#define OP_BOR		14	/* bitwise OR operator			*/
#define OP_BXOR		15	/* bitwise XOR operator			*/

#define	OP_CONCAT	16	/* string concatenatation		*/
#define OP_RADIX	17	/* string = radix (decimal, newradix)	*/
#define OP_STRIDX	18	/* first occurrence of a char in str	*/
#define OP_STRLDX	19	/* last occurrence of a char in str	*/
#define OP_STRSTR	20	/* first occurrence of str1 in str2	*/
#define OP_STRLSTR	21	/* last occurrence of str1 in str2	*/
#define OP_STRDIC	22	/* index of string in a dictionary	*/


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
#define OP_ACOS		3	/* inverse cosine			*/
#define OP_ASIN		4	/* inverse sine				*/
#define OP_BNOT		5	/* bitwise NOT operator			*/
#define	OP_COS		6	/* cosine				*/
#define	OP_DACOS	7	/* inverse cosine (output in degrees)	*/
#define	OP_DASIN	8	/* inverse sine (output in degrees)	*/
#define	OP_DCOS		9	/* cosine (arg in degrees)		*/
#define	OP_DSIN		10	/* sine (arg in degrees)		*/
#define	OP_DTAN		11	/* tangent (arg in degrees)		*/
#define	OP_DEFPAC	12	/* is named package loaded		*/
#define	OP_DEFPAR	13	/* is named parameter defined		*/
#define	OP_DEFTASK	14	/* is named task defined		*/
#define	OP_DEFVAR	15	/* does environment variable exist	*/
#define OP_DEG		16	/* convert to degrees			*/
#define	OP_ENVGET	17	/* get environment variable defn	*/
#define	OP_EXP		18	/* natural antilog, as in e ** x	*/
#define	OP_FRAC		19	/* fractional part of a real number	*/
#define	OP_IMACCESS	21	/* does named image exist		*/
#define	OP_INT		22	/* coerce to int			*/
#define OP_ISINDEF	23	/* is value INDEF			*/
#define	OP_LOG		24	/* natural logarithm			*/
#define	OP_LOG10	25	/* decimal logarithm			*/
#define	OP_MINUS	26	/* unary negation			*/
#define	OP_MKTEMP	27	/* make unique file name		*/
#define	OP_NINT		28	/* return nearest integer (round)	*/
#define	OP_OSFN		29	/* convert vfn to OS filename		*/
#define OP_RAD		30	/* convert to radians			*/
#define OP_REAL		31	/* coerce to real			*/
#define	OP_SIGN		32	/* sign					*/
#define	OP_SIN		33	/* sine					*/
#define	OP_SQRT		34	/* square root				*/
#define	OP_STR		35	/* coercion to type string		*/
#define OP_STRLEN	36	/* length of a string constant		*/
#define OP_STRLWR	37	/* convert string to lower case		*/
#define OP_STRSUB	38	/* string substitution			*/
#define OP_STRUPR	39	/* convert string to upper case		*/
#define	OP_TAN		40	/* tangent				*/

/* Multiple operators, handled by intrfun() directly */
#define OP_ERRPOP	50	/* pop the error handler		*/
#define OP_ERRPEEK	51	/* peek at error flag			*/
#define OP_ERRMSG	52	/* return the error message		*/
#define OP_ERRCODE	53	/* return the error code		*/
#define OP_ERRTASK	54	/* return task which posted error	*/
#define	OP_NSCAN	55	/* number of items conv. in last SCAN	*/
#define	OP_SUBSTR	56	/* extract substring			*/
#define OP_TRIM		57	/* trim both sides of a string		*/
#define OP_TRIML	58	/* trim left side of a string		*/
#define OP_TRIMR	59	/* trim right side of a string		*/


/* VOClient operators, handled by vocop() */
#define OP_INITVOC      100     /* initialize VO Client                 */
#define OP_CLOSEVOC     101     /* close VO Client                      */
#define OP_RESTARTVOC   102     /* restart VO Client                    */

#define OP_CONESVC      110     /* call a Cone service                  */
#define OP_SIAPSVC      111     /* call a SIAP service                  */
#define OP_RECCNT       112     /* get count of result records          */
#define OP_GETREC       113     /* get specified record number          */
#define OP_GETDATA      114     /* get specified record number          */
#define OP_GETSTR       115     /* get specified record number          */
#define OP_GETINT       116     /* get specified record number          */
#define OP_GETDBL       117     /* get specified record number          */

#define OP_ATTRCNT      120     /* get count record attributes          */
#define OP_ATTRNAME     121     /* get attribute name by index          */
#define OP_INTATTR      122     /* get attribute (int)                  */
#define OP_FLOATATTR    123     /* get attribute (real)                 */
#define OP_STRATTR      124     /* get attribute (string)               */
#define OP_DATASET      125     /* get dataset                          */
#define OP_ATTRSCAN     126     /* get attribute value                  */

#define OP_REGSEARCH    130     /* search the Registry                  */
#define OP_REGSVCSEARCH 131     /* search the Registry by svctype       */
#define OP_REGCOUNT     132     /* get count of Resource records        */
#define OP_REGBPASS     133     /* set bandpass constraint 		*/
#define OP_REGSVC       134     /* set serviceType constraint 		*/
#define OP_REGCONTENT   135     /* set contentLevel constraint 		*/
#define OP_REGVALUE     136     /* get entity value of Resource         */
#define OP_REGRESOLVER  137     /* resolve shortname/ivorn to attr      */
#define OP_NRESOLVED    138     /* number of resolved resources         */

#define OP_VALIDOBJ     140     /* validate and object in VO Client     */
#define OP_VOCREADY     141     /* verify VO Client is ready            */

#define OP_SAMP		150     /* SAMP master command			*/
#define OP_SAMPIMLOAD	151     /* image.load.fits              	*/
#define OP_SAMPTBLVOT	152     /* table.load.votable           	*/
#define OP_SAMPTBLFITS	153     /* table.load.fits              	*/
#define OP_SAMPSPECLOAD	154     /* spectrum.load.ssa-generic    	*/
#define OP_SAMPBIBLOAD	155     /* bibcode.load				*/
#define OP_SAMPRESLOAD	156     /* voresource.loadlist			*/

#define OP_SAMPTBLROW	157     /* table.highlight.row          	*/
#define OP_SAMPSELRLIST	158     /* table.select.rowList         	*/
#define OP_SAMPPOINTAT	159     /* coord.pointAt.sky            	*/
#define OP_SAMPCMDEXEC	160     /* client.cmd.exec			*/
#define OP_SAMPENVGET	161     /* client.env.get			*/
#define OP_SAMPENVSET	162     /* client.env.set			*/
#define OP_SAMPPARAMGET	163     /* client.param.get			*/
#define OP_SAMPPARAMSET	164     /* client.param.set			*/

#define OP_SAMPHANDLER	170     /* add a user-defined handler		*/
#define OP_SAMPHUBACC	171     /* check on hub access			*/
#define OP_SAMPACCESS	172     /* check an external application	*/
#define OP_SAMPSTATUS	173     /* set/print samp status		*/
#define OP_SAMPMETA	174     /* declare app metadata			*/
#define OP_SAMPNAME	175     /* set/print application name		*/





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
#define	VOCOP		(4<<OP_BITS)
#define	SAMPOP		(5<<OP_BITS)


#define	INTWIDTH	15	/* approx max chars in a printed integer*/
#define	REALWIDTH	25	/* approx max chars in a printed real	*/

extern char *truestr, *falsestr;

struct operand popop(), pushop();
struct operand makeop();
struct operand readlist();	/* read and return operand from list	*/
struct operand sexa();		/* convert n:n:n string to sexagesimal	*/
