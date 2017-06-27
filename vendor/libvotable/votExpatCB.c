/**
 *  VOTEXPATCB.C -- (Private) Expat XML Parser callback methods.
 *
 *  @file       votExpatCB.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      (Private) Expat parser XML callback methods
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <expat.h>
#include <unistd.h>
#include <ctype.h>

#include "votParseP.h"
#include "votParse.h"


extern Stack   *element_stack;

static void     vot_compileTable (Element *tdata);


/** 
 *  vot_startElement -- CB whenever a start tag is seen (private method)
 *  
 *  @brief  CB whenever a start tag is seen (private method)
 *  @fn     vot_startElement (void *user, const char *name, const char **atts)
 *
 *  @param  user 	User data (not used)
 *  @param  name 	The name in the XML tag.
 *  @param  atts 	An array of attributes.
 *  @return 		nothing
 */

void 
vot_startElement (void *user, const char *name, const char **atts)
{
    Element *me, *cur;
    int  att, type, cols, rows;
    char name_str[SZ_ATTRNAME], value[SZ_ATTRNAME], tempstr[SZ_ATTRNAME];

    
    memset (name_str, 0, SZ_ATTRNAME);
    strncpy (name_str, name, (SZ_ATTRNAME - 1));
    
    type = vot_eType (name_str);

    /* Check or deprecated elements.
     */
    if (type == TY_DEFINITIONS)
        votEmsg ("<DEFINITIONS> element was deprecated in v1.1\n");
    if (type == TY_COOSYS)
	votEmsg ("<COOSYS> element was deprecated in v1.2\n");

    if (type != -1) {
        if ((me = vot_newElem (type)) == (Element *) NULL)
	    fprintf (stderr, "Cannot create new element for <%s>\n", name_str);
        
        if (!vot_isEmpty (element_stack)) {
            cur = votPeek (element_stack);
            
            if (cur->child)
                cur->last_child->next = me;
            else
                cur->child = me;
            
            cur->last_child = me;
            
            vot_setHandle (me);
            
            /* Gets the attributes. 
	     */
            for (att=0; atts[att]; att+=2)
                vot_attrSet (me->attr, (char *)atts[att], (char *)atts[att+1]);
            
            if (me->type == TY_TABLE) {
                cols = rows = 0;
                strcpy (tempstr, "NCOLS");
                sprintf (value, "%i", cols);
                vot_attrSet (me->attr, tempstr, value);
                
                strcpy (tempstr, "NROWS");
                sprintf (value, "%i", rows);
                vot_attrSet (me->attr, tempstr, value);
            }
            me->parent = cur;
            
            votPush (element_stack, me);

        } else
            fprintf (stderr, "ERROR: No Root node!\n");
    }
}


/** 
 *  vot_endElement -- CB whenever an end tag is seen (private method)
 *
 *  @brief  CB whenever an end tag is seen (private method)
 *  @fn     vot_endElement (void *user, const char *name)
 *
 *  @param  user	User data (not used)
 *  @param  name 	The name in the XML tag
 *  @return 		nothing
 */
void 
vot_endElement (void *user, const char *name)
{
    static int  cols = 0, rows = 0;
    Element *cur, *parent;
    int  type;
    char name_str[SZ_ATTRNAME];
    char value[SZ_ATTRNAME];
    char tempstr[SZ_ATTRNAME];
    

    memset (value, 0, SZ_ATTRNAME);
    memset (tempstr, 0, SZ_ATTRNAME);
    memset (name_str, 0, SZ_ATTRNAME);
    strncpy (name_str, name, (SZ_ATTRNAME - 1));
    
    if ( (type = vot_eType (name_str)) != -1) {
        /* BUILD TYPE */
        if (element_stack->head) {
            cur = votPop (element_stack);
            
            if (!vot_isEmpty (element_stack)) {
                parent = element_stack->head->element;
                
                if (parent->type == TY_TABLE) {
                    if (cur->type == TY_FIELD) {
                        strcpy (tempstr, "NCOLS");
                        cols = (atoi (vot_attrGet (parent->attr, tempstr)) + 1);
                        sprintf (value, "%i", cols);
                        vot_attrSet (parent->attr, tempstr, value);
                    }
                }
                
                if (parent->type == TY_TABLEDATA) {
                    if (cur->type == TY_TR) {
                        strcpy (tempstr, "NROWS");
                        rows = atoi (vot_attrGet (parent->parent->parent->attr,
			    tempstr)) + 1;
                        sprintf (value, "%i", rows);
                        vot_attrSet (parent->parent->parent->attr, 
			    tempstr, value);
                    }
                }
                
                if (cur->type == TY_TABLEDATA)
                    vot_compileTable (cur);
            }
            
            if (cur->type != type)
                fprintf (stderr, "ERROR: Malformed XML!!!!!\n%s not matched.", 
                        vot_elemName (cur));
   
        } else
            fprintf (stderr, "ERROR: No Root node!\n");
    }
}


/**
 *  vot_charData -- Handle non-element character strings (private method)
 *
 *  @brief  Handle non-element character strings (private method)
 *  @fn     vot_charData (void *user, const XML_Char *s, int len) 
 *
 *  @param  user	User data (not used)
 *  @param  s 		content string
 *  @param  len 	length of string
 *  @return 		nothing
 */
void
vot_charData (void *user, const XML_Char *s, int len) 
{
    Element  *cur;
    char     *ip = (char *) s;
    char     *rstr;
    int      clen = 0;
    

    cur = votPeek (element_stack);
    clen = (cur->content ? strlen (cur->content) : 0);

#ifdef STRIP_NL
    while (len && isspace (*ip)) 	/*  Strip newlines from content.  */
        ip++, len--;
#endif

    if (len > 0 && ip && *ip) {
        if (cur->content == NULL) {
            cur->content = (char *) calloc  ((len + 2), sizeof (char));
        } else {
            if ((rstr = (char *) realloc (cur->content, (clen + len + 2)) ))
                cur->content = rstr;
            else
                fprintf (stderr, "ERROR: Could not realloc charData space.\n");
        }
        strncat (cur->content, ip, len);
    }
}


/**
 *  vot_startCData -- Handle the start of CDATA strings (private method)
 *
 *  @brief  Handle the start of CDATA strings (private method)
 *  @fn     vot_startCData (void *user)
 *
 *  @param  user	User data (not used)
 *  @return 		nothing
 */
void
vot_startCData (void *user)
{
    Element  *cur = votPeek (element_stack);
    cur->isCData = 1;
}


/**
 *  vot_endCData -- Handle the end of CDATA strings (private method)
 *
 *  @brief  Handle the end of CDATA strings (private method)
 *  @fn     vot_endCData (void *user)
 *
 *  @param  user	User data (not used)
 *  @return 		nothing
 */
void
vot_endCData (void *user)
{
    ;
}




/****************************************************************************
 *  Private procedures.
 ****************************************************************************/

/** 
 *  vot_compileTable -- Compile a table of strings for easy access
 *
 *  @brief  Compile a table of strings for easy access (private method)
 *  @fn     vot_compileTable (Element *tdata)
 *
 *  @param  tdata 	TABLEDATA Element containing values to compile
 *  @return		nothing
 */
static void
vot_compileTable (Element *tdata)
{
    Element *r = NULL, *c = NULL;
    handle_t r_h, c_h, tdata_h;
    int   cols, rows, ncells, i, j;
    char **ip;
    

    if (tdata->type != TY_TABLEDATA) {
        votEmsg ("Arg must be a TABLEDATA element to compile.\n");
        return;
    }
    
    tdata_h = vot_lookupHandle (tdata);
    
    cols = vot_getNCols (tdata_h);
    rows = vot_getNRows (tdata_h);
    ncells  = rows * cols;
    
    if (ncells == 0)	/* e.g. a metadata votable return	*/
	return;

    if (tdata->data)
        free (tdata->data);
    tdata->data = (char **) calloc (ncells, sizeof (char *));

    ip = tdata->data;
    
    r_h = vot_getTR (vot_lookupHandle (tdata));
    r = vot_getElement (r_h);
    
    c_h = vot_getTD (vot_lookupHandle (r));
    c = vot_getElement (c_h);
    
    for (i = 1 ; r; i++) {
        for (j = 1; c; j++) {
            *ip++ = c->content;
            c = c->next;
        }
        r = r->next;
        
        if (r)
            c = r->child;
    }
}
