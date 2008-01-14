/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define import_spp
#define import_knames
#include <iraf.h>

/*
 * BITFIELDS.C -- Portable C routines for extracting and inserting small
 * integers into an integer value.
 */

static const unsigned XINT bitmask[] = {	0,		/* MACHDEP */
	01,		03,		07,
	017,		037,		077,
	0177,		0377,		0777,
	01777,		03777,		07777,
	017777,		037777,		077777,
	0177777,	0377777,	0777777,
	01777777,	03777777,	07777777,
	017777777,	037777777,	077777777,
	0177777777,	0377777777,	0777777777,
	01777777777,	03777777777,	07777777777,
	017777777777,	037777777777
};


/* BITPAK -- Pack an unsigned integer value into a bitfield in a longword.
 * The size of the bitfield may not exceed the number of bits in an integer.
 */
/* arg_ival  : value to be placed in bitfield	*/
/* arg_wordp : longword to be written into		*/
/* offset    : one-indexed offset of first bit	*/
/* nbits     : number of bits to be set		*/
int BITPAK ( XINT *arg_ival, XINT *arg_wordp, XINT *offset, XINT *nbits )
{
	unsigned XINT *ival;
	unsigned XINT *wordp;
	unsigned XINT shift;
	unsigned XINT mask;

	ival = (unsigned XINT *)arg_ival;
	wordp = (unsigned XINT *)arg_wordp;

	shift = *offset - 1;
	mask = bitmask[*nbits] << shift;
	*wordp = (*wordp & ~mask) | ((*ival << shift) & mask);

	return 0;
}


/* BITUPK -- Unpack an unsigned integer bit field from a longword.
 */
/* arg_wordp : longword to be examined		*/
/* offset    : one-indexed offset of first bit	*/
/* nbits     : number of bits to be set		*/
XINT BITUPK ( XINT *arg_wordp, XINT *offset, XINT *nbits )
{
	unsigned XINT *wordp;
	wordp = (unsigned XINT *)arg_wordp;
	return ((*wordp >> (*offset-1)) & bitmask[*nbits]);
}
