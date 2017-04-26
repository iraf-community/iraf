/**
 *  XRVALUES.C
 *
 *  Methods to handle XML-RPC values.
 *
 *                 xr_initValues  ()
 *               v = xr_newValue  (int type, void *v)
 *               v = xr_tmpValue  (int type, void *v)
 *                  xr_freeValue  (int index)
 *
 *        snum = xr_appendStruct  (int snum, char *key, int value)
 *         anum = xr_appendArray  (int anum, char *key, int value)
 *
 *             xr_getStructValue  (int snum, char *key, void *value)
 *              xr_getArrayValue  (int anum, int index, void *value)
 *
 *
 *  @brief      Methods to handle XML-RPC values.
 *
 *  @file       xrValues.c
 *  @author     Mike Fitzpatrick
 *  @date       6/10/09
 */


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "xrpcP.h"



#define MAX_XVALUES	128

#define V_FIRST		5
#define V_LAST		MAX_XVALUES

#define V_TMPVAL	0			/* temp value		*/
#define V_CPARAM	1			/* client-side params	*/
#define V_CRESULT	2			/* client-side results	*/
#define V_MPARAM	3			/* method-side params	*/
#define V_MRESULT	4			/* method-side results	*/


typedef struct {
    xmlrpc_value  *val;                         /* xmlrpc value         */
    int    in_use;

} XValue, *XValueP;


int     nx_values       = -1;
XValue  xValues[MAX_XVALUES];

xmlrpc_env env;                                 /* local env            */


static  xmlrpc_value *xr_gval (int type, void *value);
static  int xr_sval (int type, xmlrpc_value *v, void *uval);





/* XR_INTIVALUES -- Globally initialize the XValues array.
*/
void
xr_initValues ()
{
    memset (&xValues[0], 0, MAX_XVALUES * sizeof(XValue) );
    nx_values = -1;
}


/* XR_NEWVALUE -- 'Allocate' a new dynamic value to be used.
*/
int
xr_newValue (int type, void *v)
{
    int    i;
    int    *iv  = (int *) v;
    double *dv  = (double *) v;
    const char   *str = (const char *) v;
    XValue *xv;


    if (nx_values < 0)           /* initialize the value array */
        memset (&xValues[0], 0, MAX_XVALUES * sizeof(XValue) );

    nx_values++;
    for (i=V_FIRST; i < V_LAST; i++) {
        xv = &xValues[i];
        if (! xv->in_use) {

    	    if ( (xv->val = xr_gval (type, v)) ) {
        	xv->in_use = TRUE;
        	return (i); 
	    } else
		break;
        }
    }

    return (-1);

}


/*  XR_TMPVAL -- Return a temp xmlrc_value.  This is distinguished from other
**  XValues by the fact that this is a static value that might be overwritten
**  by some later call.  This will always be the xmlrpc_value in the zero
**  position of the XValues array, any procedure that requests a new value
**  on this XValue position will destroy the value before a new one is
**  allocated.
*/
int
xr_tmpValue (int type, void *v)
{
    int    i;
    int    *iv  = (int *) v;
    double *dv  = (double *) v;
    const char   *str = (const char *) v;
    XValue *xv;

    xv = &xValues[V_TMPVAL];

    if (! xv->in_use) 
	xmlrpc_DECREF(xv->val);

    if ( (xv->val = xr_gval (type, v)) ) {
        xv->in_use = TRUE;
        return (V_TMPVAL); 
    } else
	return (-1);

}



/* XR_FREEVALUE -- 'Deallocate' a new value in use.
*/
void
xr_freeValue (int index)
{
    XValue *xv = &xValues[index];
            
    if (xv->in_use) {
        xmlrpc_DECREF (xv->val);
        memset (&xValues[index], 0, sizeof(XValue) );
        nx_values--;
    }
}


/*  Compound objects.
**
*/
int
xr_appendStruct (int snum, char *key, int value)
{
    XValue *x1 = &xValues[snum];	/* struct to be appended	*/
    XValue *x2 = &xValues[value];	/* value to be appended		*/

    xmlrpc_struct_set_value (&env, x1->val, key, x2->val);
}


int
xr_appendArray (int anum, char *key, int value)
{
    XValue *x1 = &xValues[anum];	/* array to be appended	*/
    XValue *x2 = &xValues[value];	/* value to be appended		*/

    xmlrpc_array_append_item (&env, x1->val, x2->val);
}




/*  Get methods.
*/

void
xr_getStructValue (int snum, char *key, void *value)
{
    XValue *xv = &xValues[snum];		/* struct object	*/
    xmlrpc_value *s = xv->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;

    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);

	value = (void *)v;
    }
}


void
xr_getArrayValue (int anum, int index, void *value)
{

    XValue *xv = &xValues[anum];		/* array object		*/
    xmlrpc_value *tmp;

    xmlrpc_array_read_item (&env, xv->val, index, &tmp);

    /*  FIXME  */

/*
    switch (type) {
    case TY_INT: 	
	
        xmlrpc_read_int (&env, tmp, &ival);
	break;
    }
*/
}



/************************************************************************
**  PRIVATE PROCEDURES
**
*/


/*  GVAL -- Given a user-defined value, convert it to an xmlrpc_value.
*/
static xmlrpc_value *
xr_gval (int type, void *v)
{
    int    *iv  = (int *) v;
    double *dv  = (double *) v;
    const char   *str = (const char *) v;

    switch (type) {
    case TY_INT: 	return ( xmlrpc_int_new (&env, *iv) );
    case TY_DOUBLE: 	return ( xmlrpc_double_new (&env, *dv) );
    case TY_BOOL: 	return ( xmlrpc_bool_new (&env, (xmlrpc_bool)*iv) );
    case TY_STRING: 	return ( xmlrpc_string_new (&env, str) );
    case TY_DATETIME: 	return ( xmlrpc_datetime_new_str (&env, str) );
    case TY_ARRAY: 	return ( xmlrpc_array_new (&env) );
    case TY_STRUCT: 	return ( xmlrpc_struct_new (&env) );
    }
    return (xmlrpc_value *) NULL;
}


/*  SVAL -- Scan a user-type value from an xmlrpc_value.  Note we cannot
**  use this for compound objects such as arrays and structs, we assume
**  the caller has already allocated the uval pointer.
*/
static int
xr_sval (int type, xmlrpc_value *v, void *uval)
{
    size_t len;
    int    *iv  = (int *) uval;
    double *dv  = (double *) uval;
    const char   *str = (const char *) uval;
    xmlrpc_bool  bv;


    switch (type) {
    case TY_INT:
	xmlrpc_read_int (&env, v, iv);
	uval = (void *)&iv;
	break;
    case TY_DOUBLE:
	xmlrpc_read_double (&env, v, dv);
	uval = (void *)&dv;
	break;
    case TY_BOOL:
	xmlrpc_read_bool (&env, v, &bv);
	uval = (void *)&bv;
	break;
    case TY_STRING:
	xmlrpc_read_string_lp (&env, v, &len, &str);
	uval = (void *)&str;
	break;
    case TY_DATETIME:
	xmlrpc_read_datetime_str (&env, v, &str);
	uval = (void *)&str;
	break;
    }

    return ( ((env.fault_occurred) ? ERR : OK) );
}


