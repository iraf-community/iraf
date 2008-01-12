#ifndef _CLREADLINE_H
#define _CLREADLINE_H

extern char *cl_readline( const char * );
extern void cl_add_history( const char * );
extern int cl_read_history( const char * );
extern int cl_write_history( const char * );
extern void cl_rl_free( void * );

#ifndef _CLREADLINE_C

#define readline cl_readline
#define add_history cl_add_history
#define read_history cl_read_history
#define write_history cl_write_history
#define rl_free cl_rl_free

#endif	/* _CLREADLINE_C */

#endif	/* _CLREADLINE_H */
