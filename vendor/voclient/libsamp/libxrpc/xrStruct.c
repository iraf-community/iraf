/**
 *
 *  XRSTRUCT.C
 *
 *  Methods used to implement the Struct object.
 *
 *
 *         snum = xr_newStruct ()
 *               xr_freeStruct (int snum)
 *
 *       nelem = xr_structSize (int snum)
 *       key = xr_getStructKey (int snum, int index)
 *       key = xr_getStructVal (int snum, int index)
 *
 *           xr_setIntInStruct (int snum, char *key, int value)
 *        xr_setDoubleInStruct (int snum, char *key, double value)
 *          xr_setBoolInStruct (int snum, char *key, int value)
 *        xr_setStringInStruct (int snum, char *key, char *value)
 *      xr_setDatetimeInStruct (int snum, char *key, char *value)
 *        xr_setStructInStruct (int snum, char *key, int value)
 *         xr_setArrayInStruct (int snum, char *key, int value)
 *
 *         xr_getIntFromStruct (int snum, char *key, int *value)
 *      xr_getDoubleFromStruct (int snum, char *key, double *value)
 *        xr_getBoolFromStruct (int snum, char *key, int *value)
 *      xr_getStringFromStruct (int snum, char *key, char **value)
 *    xr_getDatetimeFromStruct (int snum, char *key, char **value)
 *      xr_getStructFromStruct (int snum, char *key, int *value)
 *       xr_getArrayFromStruct (int snum, char *key, int *value)
 *
 *
 *  @brief      Methods used to manage Struct objects.
 *
 *  @file       xrStruct.c
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


#define SZ_NAME		64


typedef struct {
    xmlrpc_value  *val;				/* struct value		*/

    int	   in_use;

} PStruct, *PStructP;


int	nstructs	= -1;
PStruct	sParams[MAX_STRUCTS];

xmlrpc_env env;					/* local env		*/



/**
 *  XR_NEWSTRUCT -- Create a new Struct type value.
 */
int
xr_newStruct ()
{
    int  i;
    PStruct *p;


    if (nstructs < 0) 		/* initialize the Struct array */
	memset (&sParams[0], 0, MAX_STRUCTS * sizeof(PStruct) );

    nstructs++;
    xmlrpc_env_init (&env);

    for (i=0; i < MAX_STRUCTS; i++) {
	p = &sParams[i];
	if (! p->in_use) {
	    if (p->val)
    	        xmlrpc_DECREF (p->val);
    	    memset (p, 0, sizeof(PStruct) );

	    p->val = xmlrpc_struct_new (&env);
	    p->in_use = TRUE;
	    return (i);	
	}
    }

    return (-1);
}

void
xr_freeStruct (int snum)
{
    PStruct *p = &sParams[snum];
	    
    /*
    */
    if (p && p->val)
        xmlrpc_DECREF (p->val);
    memset (&sParams[snum], 0, sizeof(PStruct) );

    p->in_use = 0;
    nstructs--;
}


int
xr_structSize (int snum)
{
    PStruct *p = &sParams[snum];
    int  nelem = 0;

    nelem = xmlrpc_struct_size (&env, p->val);
    return (nelem);
}


char *
xr_getStructKey (int snum, int index)
{
    PStruct *p = &sParams[snum];
    static char  buf[SZ_LINE], *key = buf;
    const char *str = (char *) NULL;
    xmlrpc_value *k, *v;
    size_t len;


    xmlrpc_env_init (&env);
    memset (buf, 0, SZ_LINE);
    xmlrpc_struct_read_member (&env, p->val, index, &k, &v);

    xmlrpc_read_string_lp (&env, k, &len, &str);
    if (str) {
        strcpy (key, str);
        free ((void *) str);
    } else
        strcpy (key, "{ }");

    xmlrpc_DECREF (k);
    xmlrpc_env_clean (&env);

    return (key);
}


char *
xr_getStructVal (int snum, int index)
{
    PStruct *p = &sParams[snum];
    static char  buf[SZ_LINE], *value = buf;
    const char *str = (char *) NULL;
    xmlrpc_value *k, *v;
    size_t len;


    xmlrpc_env_init (&env);
    memset (buf, 0, SZ_LINE);
    xmlrpc_struct_read_member (&env, p->val, index, &k, &v);

    xmlrpc_read_string_lp (&env, v, &len, &str);
    if (env.fault_occurred) {
        /* Value was not a string.... */
        strcpy (value, "{ }");
        if (str)
            free ((void *) str);
    } else {
        if (str) {
            strcpy (value, str);
            free ((void *) str);
        } else
            strcpy (value, " ");
    }

    xmlrpc_DECREF (v);
    xmlrpc_env_clean (&env);

    return (value);
}



/* ******************************************************************** */


void
xr_setIntInStruct (int snum, char *key, int value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *v = xmlrpc_int_new (&env, value);

    xmlrpc_struct_set_value (&env, p->val, key, v);
    xmlrpc_DECREF(v);
}

void
xr_setDoubleInStruct (int snum, char *key, double value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *v = xmlrpc_double_new (&env, value);

    xmlrpc_struct_set_value (&env, p->val, key, v);
    xmlrpc_DECREF(v);
}

void
xr_setBoolInStruct (int snum, char *key, int value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *v = xmlrpc_bool_new (&env, (xmlrpc_bool) value);

    xmlrpc_struct_set_value (&env, p->val, key, v);
    xmlrpc_DECREF(v);
}

void
xr_setStringInStruct (int snum, char *key, char *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *v = xmlrpc_string_new (&env, value);

    xmlrpc_struct_set_value (&env, p->val, key, v);
    xmlrpc_DECREF(v);
}

void
xr_setDatetimeInStruct (int snum, char *key, char *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *v = xmlrpc_string_new (&env, (const char *)value);

    xmlrpc_struct_set_value (&env, p->val, key, v);
    xmlrpc_DECREF(v);
}

void
xr_setStructInStruct (int snum, char *key, int value)
{
    PStruct *p = &sParams[snum];
    PStruct *n = &sParams[value];

    xmlrpc_struct_set_value (&env, p->val, key, n->val);
}

void
xr_setArrayInStruct (int snum, char *key, int value)
{
    PStruct *p = &sParams[snum];

    xmlrpc_struct_set_value (&env, p->val, key, xr_getAElement (value) );
}



/**************************************************************************
**  Procedures used to extract values from a struct.
*/

void
xr_getIntFromStruct (int snum, char *key, int *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;

    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);
	xmlrpc_read_int (&env, v, value);
    }
}

void
xr_getDoubleFromStruct (int snum, char *key, double *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;

    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);
	xmlrpc_read_double (&env, v, value);
    }
}

void
xr_getBoolFromStruct (int snum, char *key, int *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;

    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);
	xmlrpc_read_bool (&env, v, (xmlrpc_bool *)value);
    }
}

void
xr_getStringFromStruct (int snum, char *key, char **value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;
    const char *str = (char *) NULL;


    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);
	xmlrpc_read_string (&env, v, &str);
	strcpy (*value, str);
        free ((void *) str);
    }
}

void
xr_getDatetimeFromStruct (int snum, char *key, char **value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;
    const char *str = (char *) NULL;


    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);
	xmlrpc_read_datetime_str (&env, v, &str);
	strcpy (*value, str);
        free ((void *) str);
    }
}

void
xr_getStructFromStruct (int snum, char *key, int *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;


    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);

	*value = xr_newStruct ();
	xr_setSParam (*value, v);
    }
}

void
xr_getArrayFromStruct (int snum, char *key, int *value)
{
    PStruct *p = &sParams[snum];
    xmlrpc_value *s = p->val;
    xmlrpc_value *v = (xmlrpc_value *) NULL;


    if (xmlrpc_struct_has_key (&env, s, (const char *) key)) {
        xmlrpc_struct_find_value (&env, s, (const char *)key, &v);

	*value = xr_newArray ();
	xr_setAElement (*value, v);
    }
}



/* Set/Get params.
*/

xmlrpc_value *
xr_getSParam (int snum)
{
    PStruct *p = &sParams[snum];
    return (p->val);
}

void
xr_setSParam (int snum, xmlrpc_value *v)
{
    PStruct *p = &sParams[snum];
    p->val = v;
}

