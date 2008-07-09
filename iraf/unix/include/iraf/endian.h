#ifndef	_IRAF_ENDIAN_H
#define	_IRAF_ENDIAN_H

#ifdef LINUX
# include <endian.h>
#endif
#if (defined(BSD) || defined(MACOSX))
# include <machine/endian.h>
#endif
#ifdef CYGWIN
# include <sys/param.h>
#endif
#ifdef SOLARIS
# include <sys/isa_defs.h>
#endif

/* */

#ifndef __LITTLE_ENDIAN
# ifdef _LITTLE_ENDIAN
#  define __LITTLE_ENDIAN _LITTLE_ENDIAN
# endif
#endif

#ifndef __BIG_ENDIAN
# ifdef _BIG_ENDIAN
#  define __BIG_ENDIAN _BIG_ENDIAN
# endif
#endif

#if (!defined(__LITTLE_ENDIAN) && !defined(__BIG_ENDIAN))
# error "Cannot get ENDIAN macro symbols"
#endif

#if (defined(__LITTLE_ENDIAN) && defined(__BIG_ENDIAN))
# ifndef __BYTE_ORDER
#  ifdef _BYTE_ORDER
#   define __BYTE_ORDER _BYTE_ORDER
#  else
#   error "Cannot get __BYTE_ORDER macro symbol"
#  endif
# endif
# ifndef __FLOAT_WORD_ORDER
#  ifdef _FLOAT_WORD_ORDER
#   define __FLOAT_WORD_ORDER _FLOAT_WORD_ORDER
#  endif
# endif
#endif

#if (defined(__LITTLE_ENDIAN) && !defined(__BIG_ENDIAN))
# undef __LITTLE_ENDIAN
# define __LITTLE_ENDIAN 1234
# define __BIG_ENDIAN    4321
# define __BYTE_ORDER __LITTLE_ENDIAN
#endif

#if (!defined(__LITTLE_ENDIAN) && defined(__BIG_ENDIAN))
# undef __BIG_ENDIAN
# define __LITTLE_ENDIAN 1234
# define __BIG_ENDIAN    4321
# define __BYTE_ORDER __BIG_ENDIAN
#endif

#ifndef __FLOAT_WORD_ORDER
# define __FLOAT_WORD_ORDER __BYTE_ORDER
#endif

/* */

#include <iraf/spp.h>

#if __BYTE_ORDER == __LITTLE_ENDIAN
#define BYTE_SWAP YES
#else
#define BYTE_SWAP NO
#endif

#if __FLOAT_WORD_ORDER == __LITTLE_ENDIAN
#define IEEE_SWAP YES
#else
#define IEEE_SWAP NO
#endif

#endif	/* ! _IRAF_ENDIAN_H */
