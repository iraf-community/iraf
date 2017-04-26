/**
 *  XRARRAY.c -- Methods used to manage Arrays.
 * 
 *  Methods used to manage Arrays.
 *
 *        anum = xr_newArray ()
 *              xr_freeArray (int anum)
 *         len = xr_arrayLen (int anum)
 *  
 *          xr_setIntInArray (int anum, int value)
 *       xr_setDoubleInArray (int anum, double value)
 *         xr_setBoolInArray (int anum, int value)
 *       xr_setStringInArray (int anum, char *value)
 *     xr_setDatetimeInArray (int anum, char *value)
 *       xr_setStructInArray (int anum, int svalue)
 *        xr_setArrayInArray (int anum, int avalue)
 *  
 *        xr_getIntFromArray (int anum, int index, int *value)
 *     xr_getDoubleFromArray (int anum, int index, double *value)
 *       xr_getBoolFromArray (int anum, int index, int *value)
 *     xr_getStringFromArray (int anum, int index, char **value)
 *   xr_getDatetimeFromArray (int anum, int index, char **value)
 *     xr_getStructFromArray (int anum, int index, int *value)
 *      xr_getArrayFromArray (int anum, int index, int *value)
 *  
 *
 *  @brief      Methods used to manage Arrays.
 *
 *  @file       xrArray.c
 *  @author     Mike Fitzpatrick
 *  @date       6/10/09
 */
/*****************************************************************************/


#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>

#include <xmlrpc-c/base.h>
#include <xmlrpc-c/client.h>
#include <xmlrpc-c/server.h>
#include <xmlrpc-c/server_abyss.h>

#include "xrpcP.h"


#define SZ_NAME		64


typedef struct {
    xmlrpc_value  *val;				/* struct value		*/
    int	   in_use;
} AElement, *AElementP;


int	  narrays		= -1;
AElement  aElements[MAX_ARRAYS];
xmlrpc_env env;					/* local env		*/

extern int client_verbose;




/**
 *  XR_NEWARRAY -- Create a new Array object.
 *
 *  @brief  Create a new Array object.
 *  @fn     int xr_newArray (void)
 *
 *  @return             status code (-1 on error)
 */
int
xr_newArray ()
{
    int  i;
    AElement *a;


    if (narrays < 0) 		/* initialize the array 	*/
	memset (&aElements[0], 0, MAX_ARRAYS * sizeof(AElement) );

    narrays++;
    for (i=0; i < MAX_ARRAYS; i++) {
	a = &aElements[i];
	if (! a->in_use) {
	    a->val = (xmlrpc_value *) xmlrpc_array_new (&env);
	    a->in_use = TRUE;
	    return (i);	
	}
    }

    return (-1);
}


/**
 *  XR_FREEARRAY -- Free an Array object.
 *
 *  @brief  Free an Array object.
 *  @fn     void xr_freeArray (int anum)
 *
 *  @param  anum	array number
 *  @return             nothing
 */
void
xr_freeArray (int anum)
{
    AElement *a = &aElements[anum];
    /*
    xmlrpc_value *v = (xmlrpc_value *) NULL;
    int i, nelem = xmlrpc_array_size (&env, a->val);
    */

    if (a->val) {
        /*  Release references to the values in the array.
        for (i=0; i < nelem; i++) {
            xmlrpc_array_read_item (&env, a->val, (unsigned int) i, &v);
            xmlrpc_DECREF(v);
        }
        */
    }
    xmlrpc_DECREF (a->val); 		/*  free the array */
    a->in_use = FALSE;

    /*
    memset (&aElements[anum], 0, sizeof(AElement) );
    */
    narrays--;
}


/**
 *  XR_ARRAYLEN -- Return length of an array.
 *
 *  @brief  Return the length of an array
 *  @fn     len = xr_arrayLen (int anum)
 *
 *  @param  anum	array number
 *  @return             length of array
 */
int
xr_arrayLen (int anum)
{
    AElement *a = &aElements[anum];
	    
    return (xmlrpc_array_size (&env, a->val));
}




/**************************************************************************
**  Procedures used to set values in an array.
*/

/**
 *  XR_SETINTINARRAY -- Set an int in an array.
 *
 *  @fn     void xr_setIntInArray (int anum, int value)
 */
void
xr_setIntInArray (int anum, int value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v = xmlrpc_int_new (&env, value);

    xmlrpc_array_append_item (&env, a->val, v);
    xmlrpc_DECREF(v);
}


/**
 *  XR_SETDOUBLEINARRAY -- Set a double in an array.
 *
 *  @fn     void xr_setDoubleInArray (int anum, double value)
 */
void
xr_setDoubleInArray (int anum, double value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v = xmlrpc_double_new (&env, value);

    xmlrpc_array_append_item (&env, a->val, v);
    xmlrpc_DECREF(v);
}


/**
 *  XR_SETBOOLINARRAY -- Set a bool in an array.
 *
 *  @fn     void xr_setBoolInArray (int anum, int value)
 */
void
xr_setBoolInArray (int anum, int value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v = xmlrpc_bool_new (&env, (xmlrpc_bool) value);

    xmlrpc_array_append_item (&env, a->val, v);
    xmlrpc_DECREF(v);
}


/**
 *  XR_SETSTRINGINARRAY -- Set a string in an array.
 *
 *  @fn     void xr_setStringInArray (int anum, char *value)
 */
void
xr_setStringInArray (int anum, char *value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v = xmlrpc_string_new (&env, value);

    xmlrpc_array_append_item (&env, a->val, v);
    xmlrpc_DECREF(v);
}


/**
 *  XR_SETDATETIMEaINARRAY -- Set a datetime in an array.
 *
 *  @fn     void xr_setDatetimeInArray (int anum, char *value)
 */
void
xr_setDatetimeInArray (int anum, char *value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v = xmlrpc_string_new (&env, (const char *)value);

    xmlrpc_array_append_item (&env, a->val, v);
    xmlrpc_DECREF(v);
}


/**
 *  XR_SETSTRUCTINARRAY -- Set a struct in an array.
 *
 *  @fn     void xr_setStructInArray (int anum, int value)
 */
void
xr_setStructInArray (int anum, int value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;
    
    v = xr_getSParam (value);
    xmlrpc_array_append_item (&env, a->val, v);
/*  xmlrpc_DECREF(v); */
}


/**
 *  XR_SETARRAYINARRAY -- Set an array in an array.
 *
 *  @fn     void xr_setArrayInArray (int anum, int value)
 */
void
xr_setArrayInArray (int anum, int value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;
    
    v = xr_getAElement (value);
    xmlrpc_array_append_item (&env, a->val, v);
/*  xmlrpc_DECREF(v); */
}





/**************************************************************************
**  Procedures used to extract values from a struct.
*/

/**
 *  XR_GETINTFROMARRAY -- Get an int from an array.
 *
 *  @fn     void xr_getIntFromArray (int anum, int index, int *value)
 */
void
xr_getIntFromArray (int anum, int index, int *ival)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;

    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);
    xmlrpc_read_int (&env, v, ival);
    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETDOUBLEFROMARRAY -- Get a double from an array.
 *
 *  @fn     void xr_getDoubleFromArray (int anum, int index, double *dval)
 */
void
xr_getDoubleFromArray (int anum, int index, double *dval)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;

    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);
    xmlrpc_read_double (&env, v, dval);
    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETBOOLFROMARRAY -- Get a bool from an Array.
 *
 *  @fn     void xr_getBoolFromArray (int anum, int index, int *bval)
 */
void
xr_getBoolFromArray (int anum, int index, int *bval)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;

    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);
    xmlrpc_read_bool (&env, v, (xmlrpc_bool *)bval);
    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETSTRINGFROMARRAY -- Get a String from an Array.
 *
 *  @fn     void xr_getStringFromArray (int anum, int index, char **value)
 */
void
xr_getStringFromArray (int anum, int index, char **value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;
    size_t   len;

    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);
    xmlrpc_read_string_lp (&env, v, &len, (const char **) value);
    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETDATETIMEFROMARRAY -- Get a Datetime from an Array.
 *
 *  @fn     void xr_getDatetimeFromArray (int anum, int index, char **value)
 */
void
xr_getDatetimeFromArray (int anum, int index, char **value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v;

    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);
    xmlrpc_read_datetime_str (&env, v, (const char **) value);
    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETSTRUCTFROMARRAY -- Get a Struct from an Array.
 *
 *  @fn     void xr_getStructFromArray (int anum, int index, int *value)
 */
void
xr_getStructFromArray (int anum, int index, int *value)
{
    AElement *a = &aElements[anum];
    xmlrpc_value *v, *sp;
    register  int i;


    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);

    *value = -1;
    for (i=0; i < narrays; i++) {
	sp = xr_getSParam (i);
	if (sp == v) {
	    *value = i;
	    break;
	}
    }

    /* If not found, create the struct.
     */
    if (*value < 0) {
	int  snum = xr_newStruct();
	xr_setSParam (snum, v);
	*value = snum;
    }

    if (client_verbose)
	warn_on_error (&env);
}


/**
 *  XR_GETARRAYFROMARRAY -- Get an Array from an Array.
 *
 *  @fn     void xr_getArrayFromArray (int anum, int index, int *value)
 */
void
xr_getArrayFromArray (int anum, int index, int *value)
{
    AElement *a = &aElements[anum];
    AElement *ap = &aElements[0];
    xmlrpc_value *v;
    register  int i;


    xmlrpc_env_init (&env);
    xmlrpc_array_read_item (&env, a->val, (unsigned int) index, &v);

    *value = -1;
    for (i=0; i < narrays; i++) {
	ap = &aElements[i];
	if (ap->val == v) {
	    *value = i;
	    break;
	}
    }

    if (*value < 0) {
	int anum = xr_newArray ();
	xr_setAElement (anum, v);
	*value = anum;
    }

    if (client_verbose)
	warn_on_error (&env);
}



/* Set/Get array elements.
*/

xmlrpc_value *
xr_getAElement (int anum)
{
    if (anum < 0) {
	fprintf (stderr, "xr_getAElement: invalid anum = %d\n", anum);
	exit (1);
    } else {
        AElement *a = &aElements[anum];
        return (a->val);
    }
}


void
xr_setAElement (int anum, xmlrpc_value *v)
{
    if (anum < 0) {
	fprintf (stderr, "xr_setAElement: invalid anum = %d\n", anum);
	exit (1);
    } else {
        AElement *a = &aElements[anum];
	a->val = v;
    }
}

