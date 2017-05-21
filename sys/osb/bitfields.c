/* Copyright(c) 1986 Association of Universities for Research in Astronomy Inc.
 */

#define	import_spp
#define import_knames
#include <iraf.h>

/*
 * BITFIELDS.C -- Portable C routines for extracting and inserting small
 * integers into an integer value.
 */

unsigned XINT bitmask[] = {	0,		/* MACHDEP		*/
  01,			    03,			     07,
  017,			    037,		     077,
  0177,			    0377,		     0777,
  01777,		    03777,		     07777,
  017777,		    037777,		     077777,
  0177777,		    0377777,		     0777777,
  01777777,		    03777777,		     07777777,
  017777777,		    037777777,		     077777777,
  0177777777,		    0377777777,		     0777777777,
  01777777777,		    03777777777,	     07777777777,
  017777777777,		    037777777777,   	     077777777777,
  0177777777777,	    0377777777777,   	     0777777777777,
  01777777777777,	    03777777777777,   	     07777777777777,
  017777777777777,	    037777777777777,   	     077777777777777,
  0177777777777777,	    0377777777777777,        0777777777777777,
  01777777777777777,	    03777777777777777,       07777777777777777,
  017777777777777777,	    037777777777777777,      077777777777777777,
  0177777777777777777,	    0377777777777777777,     0777777777777777777,
  01777777777777777777,	    03777777777777777777,    07777777777777777777,
  017777777777777777777,    037777777777777777777,   077777777777777777777,
  0177777777777777777777,   0377777777777777777777,  0777777777777777777777,
  01777777777777777777777
};



/* BITPAK -- Pack an unsigned integer value into a bitfield in a longword.
 * The size of the bitfield may not exceed the number of bits in an integer.
 */
void
BITPAK (
  unsigned XINT	*ival,		/* value to be placed in bitfield	*/
  unsigned XINT	*wordp,		/* longword to be written into		*/
  XINT	*offset,		/* one-indexed offset of first bit	*/
  XINT	*nbits 			/* number of bits to be set		*/
)
{
	register unsigned XINT	shift;
	register unsigned XINT	mask;

	shift = *offset - 1;
	mask = bitmask[*nbits] << shift;
	*wordp = (*wordp & ~mask) | ((*ival << shift) & mask);
}


/* BITUPK -- Unpack an unsigned integer bit field from a longword.
 */
XINT
BITUPK (
  unsigned XINT	*wordp,		/* longword to be examined		*/
  XINT	*offset,		/* one-indexed offset of first bit	*/
  XINT	*nbits 			/* number of bits to be set		*/
)
{
	return ((*wordp >> (*offset-1)) & bitmask[*nbits]);
}
