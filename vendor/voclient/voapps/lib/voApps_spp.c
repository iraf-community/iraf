/**
 *  VOTAPP_SPP.C -- SPP Interface routines to applications code.
 *
 *  @file       votApp_spp.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      SPP Interface routines to applications code.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include <unistd.h>
#include <ctype.h>
#include <errno.h>

/*
#include "votParseP.h"
*/
#include "votParse.h"


/* SPP Type definitions.
*/
#define XCHAR		short
#define PKCHAR		char
#define XINT		int
#define XEOS		0


/*  SPP Interface Definitions.
 *
 *  SPP compilers on various platforms may append one or more trailing
 *  underscores to symbol names, we'll use macros for the interface names
 *  and use defines to see what the symbol name is.
*/
#ifdef _NO_US_

#define VX_VODATA   		vxvoda
#define VX_VODIRECTORY 		vxvody
#define VX_VODSS 		vxvods
#define VX_VOSESAME   		vxvose

#define VX_VOCOPY   		vxvocy
#define VX_VOGET   		vxvogt
#define VX_VOINFO   		vxvoio

#else

#define VX_VODATA   		vxvoda_
#define VX_VODIRECTORY 		vxvody_
#define VX_VODSS 		vxvods_
#define VX_VOSESAME   		vxvose_

#define VX_VOCOPY   		vxvocy_
#define VX_VOGET   		vxvogt_
#define VX_VOINFO   		vxvoio_

#endif

typedef  void  (*PFV)();
typedef  int   (*PFI)();



/** 
 *  Local interface declarations.
 */
static PKCHAR *spp2c (XCHAR *instr,  int maxch);
static int    spplen (XCHAR *str);
static void   func_exec (PFV func, char *name, int *argc, XCHAR *firstArg, 
 			va_list argp);



/*****************************************************************************
 *  Application Interfaces
 ****************************************************************************/

#define	MAX_ARGS	64

void  vodata (int argc, char **argv);
void  vodirectory (int argc, char **argv);
void  vodss (int argc, char **argv);
void  vosesame (int argc, char **argv);

void  votcopy (int argc, char **argv);
void  votget (int argc, char **argv);
void  votinfo (int argc, char **argv);

void  voc_debug (void);



/** 
 *  FUNC_EXEC -- Execute a VOClient function from the SPP binding.
 */
static void
func_exec (PFV func, char *name, int *argc, XCHAR *firstArg, va_list argp)
{
    int      i, _argc = *argc;
    char   *_argv[MAX_ARGS];
    XCHAR   *arg;

    if (firstArg == NULL)		/* must pass in at least one arg      */
	return;


    /*  Process the argument list.
     */
    _argc    = *argc + 1;
    _argv[0] = strdup (name);
    _argv[1] = spp2c (firstArg, spplen (firstArg));

    for (i=2; i < _argc && (arg=(XCHAR *)va_arg(argp,XCHAR *)) != NULL; i++)
	if (arg)
	    _argv[i] = spp2c (arg, spplen (arg));

    /*  Debug output.
     */
    if (access ("/tmp/VOC_DEBUG", F_OK) == 0) {
        for (i=0; i < _argc; i++)
	    fprintf (stderr, "%s ", _argv[i]);
	fprintf (stderr, "\n");
    }


    (*func) (_argc, _argv); 		/* call the task 	      */

    for (i=0; i < *argc; i++) 		/*  free the arg pointers     */
        if (_argv[i]) 
	    free ((char *) _argv[i]);

    return;
}


/****************************************************************************
 *  Task wrappers
 ****************************************************************************/

/** 
 *  VX_VOCOPY -- Application interface to the VOCOPY task.
 */
void VX_VOCOPY (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (votcopy, "votcopy", argc, firstArg, argp);
    va_end (argp);
}


/** 
 *  VX_VODATA -- Application interface to the VODATA task.
 */
void VX_VODATA (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (vodata, "vodata", argc, firstArg, argp);
    va_end (argp);
}


/** 
 *  VX_VODIRECTORY -- Application interface to the VODIRECTORY task.
 */
void VX_VODIRECTORY (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (vodirectory, "vodirectory", argc, firstArg, argp);
    va_end (argp);
}


/** 
 *  VX_VODSS -- Application interface to the VODSS task.
 */
void VX_VODSS (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (vodss, "vodss", argc, firstArg, argp);
    va_end (argp);
}


/** 
 *  VX_VOGET -- Application interface to the VOGET task.
 */
void VX_VOGET (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (votget, "votget", argc, firstArg, argp);
    va_end (argp);
}

/** 
 *  VX_VOINFO -- Application interface to the VOTINFO task.
 */
void VX_VOINFO (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (votinfo, "votinfo", argc, firstArg, argp);
    va_end (argp);
}


/** 
 *  VX_VOSESAME -- Application interface to the VOSESAME task.
 */
void VX_VOSESAME (int *argc, XCHAR *firstArg, ...)
{
    va_list  argp;

    va_start (argp, firstArg);
    func_exec (vosesame, "vosesame", argc, firstArg, argp);
    va_end (argp);
}



/****************************************************************************
 *  Private utility procedures
 ****************************************************************************/

/**
 *  SPP2C -- Convert an SPP string to a host C string.
 */
static char *
spp2c (XCHAR *instr, int maxch)
{
    XCHAR  *ip = instr;
    char   *outstr = (char *) calloc (1, maxch+1);
    char   *op = (char *) outstr;
    int      n = maxch;

    while ((*op++ = (char)*ip++) != (char)XEOS && --n >= 0)
        ;
    *--op = (char) XEOS;

    return (outstr);
}


/**
 *  SPPLEN -- Get the length of an SPP string.
 */
static int
spplen (XCHAR *str)
{
    int len = 0;

    for (len=0; str[len] != (XCHAR) XEOS; len++)
	;
    return (len);
}

void voc_debug () {  int junk; junk = 1; }
