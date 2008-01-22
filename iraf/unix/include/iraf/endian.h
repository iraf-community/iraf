#ifndef	_IRAF_ENDIAN_H
#define	_IRAF_ENDIAN_H

#ifdef LINUX
# include <endian.h>
#endif
#if defined(BSD) || defined(MACOSX)
# include <machine/endian.h>
#endif
#ifdef CYGWIN
# include <sys/param.h>
#endif
#ifdef SOLARIS
# include <sys/isa_defs.h>
#endif

#ifndef __BYTE_ORDER
# error "Cannot get __BYTE_ORDER macro symbol"
#endif
#ifndef __FLOAT_WORD_ORDER
# error "Cannot get __FLOAT_WORD_ORDER macro symbol"
#endif

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
