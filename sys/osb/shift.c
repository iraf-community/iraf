/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/*
 * SHIFT.C -- Bitwise shift operators.  A positive bitshift shifts to the left,
 * zero-filling at the right, i.e., a left shift by 1 is equivalent to a
 * multiplication by 2 (but does not cause integer overflow).  A negative shift
 * shifts to the right and is equivalent to a division.
 */

/* SHIFTI -- Bitwise boolean SHIFT of two integer variables.
 */
XINT
SHIFTI (
  XINT	*a_a,			/* operand to be shifted	*/
  XINT	*a_bits 		/* number of bits to shift	*/
)
{
	register XINT	a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}

/* SHIFTS -- Bitwise boolean SHIFT of two short-integer variables.
 */
XSHORT
SHIFTS (
  XSHORT *a_a,			/* operand to be shifted	*/
  XSHORT *a_bits 		/* number of bits to shift	*/
)
{
	register XSHORT	a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}

/* SHIFTL -- Bitwise boolean SHIFT of two long-integer variables.
 */
XLONG
SHIFTL (
  XLONG	*a_a,			/* operand to be shifted	*/
  XLONG	*a_bits 		/* number of bits to shift	*/
)
{
	register XLONG	a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}
