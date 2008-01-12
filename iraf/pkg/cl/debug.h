#ifndef _DEBUG_H
#define _DEBUG_H

#define import_stdio
#include <iraf.h>

/* debug.c */
extern void d_trace ( int );
extern void d_fmtmsg ( FILE *, const char *, const char *, int );
extern int d_instr ( FILE *, const char *, int );
extern void d_stack ( int, int );
extern void d_f ( void );	/* debugging functions */
extern void d_l ( void );
extern void d_d ( void );
extern void d_off ( void );
extern void d_on ( void );
extern void d_p ( void );
extern void d_t ( void );

#endif	/* _DEBUG_H */
