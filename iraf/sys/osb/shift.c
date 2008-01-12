/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
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
/* a_a    : operand to be shifted	*/
/* a_bits : number of bits to shift	*/
XINT SHIFTI ( XINT *a_a, XINT *a_bits )
{
	XINT a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}

/* SHIFTS -- Bitwise boolean SHIFT of two short-integer variables.
 */
/* a_a    : operand to be shifted	*/
/* a_bits : number of bits to shift	*/
XSHORT SHIFTS ( XSHORT *a_a, XSHORT *a_bits )
{
	XSHORT a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}

/* SHIFTL -- Bitwise boolean SHIFT of two long-integer variables.
 */
/* a_a    : operand to be shifted	*/
/* a_bits : number of bits to shift	*/
XLONG SHIFTL ( XLONG *a_a, XLONG *a_bits )
{
	XLONG a = *a_a, bits = *a_bits;
	return (bits > 0 ? (a << bits) : (a >> -bits));
}
