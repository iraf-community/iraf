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

#ifndef LITTLE_ENDIAN
# ifdef _LITTLE_ENDIAN
#  define LITTLE_ENDIAN _LITTLE_ENDIAN
# else
#  ifdef __LITTLE_ENDIAN
#   define LITTLE_ENDIAN __LITTLE_ENDIAN
#  endif
# endif
#endif

#ifndef BIG_ENDIAN
# ifdef _BIG_ENDIAN
#  define BIG_ENDIAN _BIG_ENDIAN
# else
#  ifdef __BIG_ENDIAN
#   define BIG_ENDIAN __BIG_ENDIAN
#  endif
# endif
#endif

#if (!defined(LITTLE_ENDIAN) && !defined(BIG_ENDIAN))
# error "Cannot get ENDIAN macro symbols"
#endif

#if (defined(LITTLE_ENDIAN) && defined(BIG_ENDIAN))
# ifndef BYTE_ORDER
#  ifdef _BYTE_ORDER
#   define BYTE_ORDER _BYTE_ORDER
#  else
#   ifdef __BYTE_ORDER
#    define BYTE_ORDER __BYTE_ORDER
#   else
#    error "Cannot get BYTE_ORDER macro symbol"
#   endif
#  endif
# endif
# ifndef FLOAT_WORD_ORDER
#  ifdef _FLOAT_WORD_ORDER
#   define FLOAT_WORD_ORDER _FLOAT_WORD_ORDER
#  else
#   ifdef __FLOAT_WORD_ORDER
#    define FLOAT_WORD_ORDER __FLOAT_WORD_ORDER
#   endif
#  endif
# endif
#endif

#if (defined(LITTLE_ENDIAN) && !defined(BIG_ENDIAN))
# undef LITTLE_ENDIAN
# define LITTLE_ENDIAN 1234
# define BIG_ENDIAN    4321
# define BYTE_ORDER LITTLE_ENDIAN
#endif

#if (!defined(LITTLE_ENDIAN) && defined(BIG_ENDIAN))
# undef BIG_ENDIAN
# define LITTLE_ENDIAN 1234
# define BIG_ENDIAN    4321
# define BYTE_ORDER BIG_ENDIAN
#endif

#ifndef FLOAT_WORD_ORDER
# define FLOAT_WORD_ORDER BYTE_ORDER
#endif

/* */

#include <iraf/spp.h>

#if BYTE_ORDER == LITTLE_ENDIAN
#define BYTE_SWAP YES
#else
#define BYTE_SWAP NO
#endif

#if FLOAT_WORD_ORDER == LITTLE_ENDIAN
#define IEEE_SWAP YES
#else
#define IEEE_SWAP NO
#endif

#endif	/* ! _IRAF_ENDIAN_H */
