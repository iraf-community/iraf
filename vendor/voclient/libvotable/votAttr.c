/**
 *  VOTATTRIBUTE.C -- (Private) Methods to manage XML attributes.
 *
 *  @file       votAttribute.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief  	(Private) Methods to manage XML attributes.
 */

#define _GNU_SOURCE
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>

#include "votParseP.h"


/** 
 *  vot_attrSet -- Set/Create an attributes (private method).
 *
 *  @brief  Set/Create an attributes (private method)
 *  @fn	    status = vot_attrSet (AttrBlock *ablock, char *name, char *value)
 *
 *  @param  ablock 	An AttrBlock to insert these attributes.
 *  @param  name 	A string that hold the name of an attribute.
 *  @param  value 	A string that hold the value of an attribute.
 *  @return 		The status of the request.  1 Success, 0=FAIL.
 *
 *  @warning If an attribute has no name/value, this will not create it.
 */
int 
vot_attrSet (AttrBlock *ablock, char *name, char *value)
{
    char  *name_m = NULL;
    int   value_found = 0, value_existing = 0, i, len;
    AttrList *attr = (AttrList *) ablock->attributes;

    
    if (name == NULL)
        return (0);
    
    /* Convert to an upper-case name.
     */
    name_m = calloc (1, (len = strlen(name)));
    for (i=0; i < len; i++)
        /*name_m[i] = toupper (name[i]); */
        name_m[i] = name[i];
    
    /* Check for namespace qualifiers on the attribute.
     */
    if ((name_m[0] && strchr(name_m, (int)':')) || strcmp("xmlns", name_m) == 0)
        value_found = 1;

    /* Check for an 'xtype' attribute in the v1.1+ spec.
     */
    if (name_m[0] && strcmp("xtype", name_m) == 0)
        value_found = 1;

    if (ablock->req && strcasestr (ablock->req, name_m) != NULL)
	value_found = 1;
    else if (ablock->opt && strcasestr (ablock->opt, name_m) != NULL)
	value_found = 1;

    if (!value_found) {
	fprintf (stderr, "Error: '%s' not a valid Attribute.\n", name);
        return (0);

    } else {
	while (attr != NULL) {
            if (name_m[0] && strcasecmp (attr->name, name_m) == 0) {
                strcpy (attr->value, value);
                value_existing = 1;
            }
            attr = attr->next;
        }

        if (!value_existing) {
            attr = (AttrList *) calloc (1, sizeof(AttrList));
            if (ablock->attributes == NULL) {
		attr->next = NULL;
                strcpy (attr->value, value);
                strcpy (attr->name, name_m);
	    } else {
                attr = (AttrList *) calloc (1, sizeof(AttrList));
                attr->next = ablock->attributes;
                strcpy (attr->value, value);
                strcpy (attr->name, name_m);
	    }
	    ablock->attributes = attr;
        }
    }	
    
    if (name_m != NULL)
        free (name_m);
    
    return (1);
}


/** 
 *  vot_attrGet -- Get an attribute's value (private method).
 *
 *  @brief  Get an attribute's value (private method)
 *  @fn	    char *vot_attrGet (AttrBlock *ablock, char *name)
 *
 *  @param  *ablock 	An AttrBlock to insert these attributes
 *  @param  *name 	A string that hold the name of an attribute
 *  @return 		Value of the attribute or NULL
 */
char *
vot_attrGet (AttrBlock *ablock, char *name)
{
    char *value;
    AttrList *attr = ablock->attributes;
    
    while (attr != NULL) {
        if (strcasecmp (attr->name, name) == 0) {
            value = (char *) calloc (SZ_ATTRNAME, sizeof(char));
            
            strncpy (value, attr->value, SZ_ATTRNAME);
	    if (value && value[0])
                return (value);
	    else 
		return (NULL);
        }
        attr = attr->next;
    }
    
    return (NULL);
}


/** 
 *  vot_attrXML -- Get the attributes for an XML tag (private method).
 *
 *  @brief  Get the attributes for an XML tag (private method)
 *  @fn	    char *vot_attrXML (AttrBlock *ablock)
 *
 *  @param *ablock 	An AttrBlock to insert these attributes
 *  @return 		A string containing the attributes for an XML tag
 */
char * 
vot_attrXML (AttrBlock *ablock)
{
    char  *out = (char *) calloc (SZ_XMLTAG, sizeof (char));
    AttrList *attr = ablock->attributes;
    
    while (attr != NULL) {

        /* Privately used attribute.  It is not valid. */
        if (strcasecmp (attr->name, "NCOLS") != 0 &&
            strcasecmp (attr->name, "NROWS") != 0) {
		    
		if ((attr->value && attr->value[0]) ||
		    (strcasecmp (attr->name, "value") == 0)) {
                        strcat (out, " ");
                        strcat (out, attr->name);
                        strcat (out, "=\"");
                        strcat (out, attr->value);
                        strcat (out, "\"");
		}
        }
        
        attr = attr->next;
    }
    
    return (out);
}
