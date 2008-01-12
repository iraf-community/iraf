/*
 * GNU readline support for the IRAF C codes
 */
#define _CLREADLINE_C

#ifndef NOREADLINE

#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>

#include "clreadline.h"

char *cl_readline( const char *prompt )
{
	return readline(prompt);
}

void cl_add_history( const char *buf )
{
	add_history(buf);
}

int cl_read_history( const char *filename )
{
	return read_history(filename);
}

int cl_write_history( const char *filename )
{
	return write_history(filename);
}

void cl_rl_free( void *ptr )
{
	free(ptr);
}

#endif	/* NOREADLINE */
