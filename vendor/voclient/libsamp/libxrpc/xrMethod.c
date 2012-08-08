/**
 *
 *  XRMETHOD.C
 *
 *  These are procedures used to implement the service methods,
 *  e.g. to get input parameters and create result values.
 *
 *
 *  Public procedures:
 *
 *	         xr_initCaller (void *data)		// not used
 *	        xr_closeCaller (void *data)		// not used
 *
 *	    xr_getIntFromParam (void *data, int index) 	// Input Parameters
 *	 xr_getDoubleFromParam (void *data, int index)
 *	 xr_getStringFromParam (void *data, int index)
 *	   xr_getBoolFromParam (void *data, int index)
 *     xr_getDatetimeFromParam (void *data, int index)
 *   s = xr_getStructFromParam (void *data, int index)
 *    a = xr_getArrayFromParam (void *data, int index)
 *
 *	     xr_setIntInResult (void *data, int val)	// Scalar Results
 *	  xr_setDoubleInResult (void *data, double val)
 *	    xr_setBoolInResult (void *data, int bval)
 *	  xr_setStringInResult (void *data, char *val)
 *      xr_setDatetimeInResult (void *data, char *date)
 *	  xr_setStructInResult (void *data, int snum)
 *	   xr_setArrayInResult (void *data, int anum)
 *
 *	        xr_setShutdown (void *data, int val)	// Shutdown Value
 *
 *  Private procedures:
 *
 *       env = xr_callerGetEnv (void *data)		// not used
 *     name = xr_callerGetName (void *data)		// not used
 *     host = xr_callerGetHost (void *data)		// not used
 *   param = xr_callerGetParam (void *data)		// not used
 *     info = xr_callerGetInfo (void *data)		// not used
 *
 *    	     val = xr_getParam (Caller *c, int index)	// not used
 *
 *
 *
 *  @brief      Procedures used to implement service methods.
 *
 *  @file       xrMethod.c
 *  @author     Mike Fitzpatrick
 *  @date       6/10/09
 */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "xrpcP.h"


int	xr_errstat	= 0;

pthread_mutex_t method_mutex = PTHREAD_MUTEX_INITIALIZER;


/* Private procedures. */
#ifdef DEBUG_PROCS

static xmlrpc_env   *xr_callerGetEnv (void *data);
static char         *xr_callerGetName (void *data);
static char         *xr_callerGetHost (void *data);
static xmlrpc_value *xr_callerGetParam (void *data);
static void         *xr_callerGetInfo (void *data);
static xmlrpc_value *xr_getParam (Caller *c, int index);

#endif

static xmlrpc_value *xr_getArrValue (xmlrpc_value *val, int index);




/****************************************************************************
**  Get parameters from caller.  These procedures are used in the method
**  implementation to get the parameters passed from the caller.
*****************************************************************************
*/

int
xr_getIntFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    int ival;


    xmlrpc_read_int (c->env, val, &ival);
    xmlrpc_DECREF (val);

    return ( ((xr_errstat=c->env->fault_occurred) ? (int)NULL : ival) );
}

double
xr_getDoubleFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    double dval;

    xmlrpc_read_double (c->env, val, &dval);
    xmlrpc_DECREF (val);

    return ( ((xr_errstat=c->env->fault_occurred) ? (double)0.0 : dval) );
}

char *
xr_getStringFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    const char  *s = (char *) NULL;
    size_t   len;


#ifdef OLD_SFP
    xmlrpc_read_string_lp (c->env, val, &len, &s);
/*  xmlrpc_read_string (c->env, val, &s); */
    xmlrpc_DECREF (val);

    return (((xr_errstat=c->env->fault_occurred) ? (char *)NULL : (char *)s));

#else
    int lock;
    char *str = (char *) NULL;

    lock = pthread_mutex_lock (&method_mutex);
    xmlrpc_read_string (c->env, val, (const char **)&s);
    if (s) {
        len = strlen (s);
        str = calloc (1, (len + 1));
    	strcpy (str, s);
    	free ((void *) s);
    }
    xmlrpc_DECREF (val);
    lock = pthread_mutex_unlock (&method_mutex);
 
    return ((char *) str);
#endif
}

int
xr_getBoolFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    xmlrpc_bool bval;

    xmlrpc_read_bool (c->env, val, &bval);
    xmlrpc_DECREF (val);

    return ( ((xr_errstat=c->env->fault_occurred) ? (int)NULL : bval) );
}

char *
xr_getDatetimeFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    const char  *s;

    xmlrpc_read_datetime_str (c->env, val, &s);
    xmlrpc_DECREF (val);

    return (((xr_errstat=c->env->fault_occurred) ? (char *)NULL : (char *)s));
}

int
xr_getStructFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    int strct;


    /* Get the struct parameter and save it to a local Struct.
    */
    strct = xr_newStruct ();
    xr_setSParam (strct, val);

    return (strct);
}

int
xr_getArrayFromParam (void *data, int index)
{
    CallerP c = (Caller *) data;
    xmlrpc_value *val = xr_getArrValue (c->param, index);
    int arry;


    /* Get the struct parameter and save it to a local Struct.
    */
    arry = xr_newArray ();
    xr_setAElement (arry, val);

    return (arry);
}




/****************************************************************************
**  Set result for the caller.  These procedures are used in the method
**  implementation to create a result object and append values.  The 
**  server then unpacks the result array when building the array for
**  the return.
*****************************************************************************
*/
void
xr_setIntInResult (void *data, int val)
{
    CallerP c = (Caller *) data;

    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
        c->result = xmlrpc_int_new (c->env, val);
}

void
xr_setDoubleInResult (void *data, double val)
{
    CallerP c = (Caller *) data;

    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xmlrpc_double_new (c->env, val);
}

void
xr_setBoolInResult (void *data, int val)
{
    CallerP c = (Caller *) data;

    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xmlrpc_bool_new (c->env, (xmlrpc_bool)val);
}

void
xr_setStringInResult (void *data, char *val)
{
    CallerP c = (Caller *) data;

    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xmlrpc_string_new (c->env, (const char *)val);
}

void
xr_setDatetimeInResult (void *data, char *val)
{
    CallerP c = (Caller *) data;

    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xmlrpc_datetime_new_str (c->env, (const char *)val);
}

void
xr_setStructInResult (void *data, int snum)
{
    CallerP c = (Caller *) data;
    extern  int res_snum;

    res_snum = snum;
    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xr_getSParam (snum);
}

void
xr_setArrayInResult (void *data, int anum)
{
    CallerP c = (Caller *) data;
    extern  int res_anum;

    res_anum = anum;
    if (c->result)
	xmlrpc_DECREF (c->result);
    if (!xr_errstat)
	c->result = xr_getAElement (anum);
}


void
xr_setShutdown (void *data, int val)
{
    CallerP c = (Caller *) data;

    c->rpc_shutdown = val;
}

/****************************************************************************
**  Private procedures.
****************************************************************************/

/*
**  Query the Caller struct values directly.  These are simple utility
**  procedures that may be removed later if not found to be useful.
*/

#ifdef DEBUG_PROCS

static xmlrpc_env *
xr_callerGetEnv (void *data)
{
    CallerP c = (Caller *) data;
    return ((xmlrpc_env *) c->env);
}

static char *
xr_callerGetName (void *data)
{
    CallerP c = (Caller *) data;
    return ((char *) c->name);
}

static char *
xr_callerGetHost (void *data)
{
    CallerP c = (Caller *) data;
    return ((char *) c->host);
}

static xmlrpc_value *
xr_callerGetParam (void *data)
{
    CallerP c = (Caller *) data;
    return ((xmlrpc_value *) c->param);
}

static void *
xr_callerGetInfo (void *data)
{
    CallerP c = (Caller *) data;
    return ((void *) c->info);
}

static xmlrpc_value *
xr_getParam (Caller *c, int index)
{
    xmlrpc_value *v;

    xmlrpc_array_read_item (c->env, c->param, index, &v);
    return (v);
}

#endif

/* Utility routine to get the value of an array by its index.
*/
static xmlrpc_value *
xr_getArrValue (xmlrpc_value *val, int index)
{
    xmlrpc_value *v;
    xmlrpc_env   env;

    if (xmlrpc_value_type (val) == XMLRPC_TYPE_ARRAY) {
        xmlrpc_array_read_item (&env, val, index, &v);
        return (v);
    } else {
	xmlrpc_INCREF(val);
        return (val);
    }
}
