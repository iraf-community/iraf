/**
 *  VOTHANDLE.C -- (Private) Methods to manage interface handles.
 *
 *  @file       votHandle.c
 *  @author     Mike Fitzpatrick and Eric Timmermann
 *  @date       8/03/09
 *
 *  @brief      (Private) Methods to manage interface handles.
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include "votParseP.h"


static  Element   **handles;		/** A pointer to the handles	   */
static  handle_t  handleMax 	= 0;	/** max	available handles	   */
static  handle_t  handleCount 	= 0;	/** count of current used handles  */



/** 
 *  vot_handleCount -- Get the number of handle_t used (private method)
 *
 *  @brief  Get the number of handle_t used (private method)
 *  @fn     int vot_handleCount (void)
 *
 *  @return 		The number of handle_t types currently stored
 */
int vot_handleCount () { return (handleCount); }


/** 
 *  vot_lookupHandle -- Lookup the handle_t to an Element (private method)
 *
 *  @brief  Lookup the handle_t to an Element (private method)
 *  @fn     handle_t vot_lookupHandle (Element *elem)
 *
 *  @param  *elem 	A pointer to an Element
 *  @return 		A handle_t to the Element
 */
handle_t
vot_lookupHandle (Element *elem)
{
    unsigned int i = 0;
    
    if (elem == (Element *) NULL)
        return (0);
    
    for (i = 0; i < handleMax; i++) {
        if (handles[i] == elem)
            return ((i + 1));
    }
    
    return (vot_setHandle (elem));
}


/** 
 *  vot_setHandle -- Assign the Element a handle_t (private method)
 *
 *  @brief  Assign the Element a handle_t (private method)
 *  @fn     handle_t vot_setHandle (Element *elem)
 *
 *  @param  elem 	A pointer to an Element to be assigned a handle_t.
 *  @return 		A handle_t refering to elem
 */
handle_t
vot_setHandle (Element *elem)
{
    unsigned int i = 0;
    Element **old_handles;
    
    if (elem == NULL)
        return (0);
    
    if (handleCount == handleMax) {
        old_handles = handles;
        handles = (Element **) calloc ((handleMax + HANDLE_INCREMENT), 
	    sizeof(Element *));
        
        for (i = 0; i < handleMax; i++)
            handles[i] = old_handles[i];
        
        handleMax = handleMax + HANDLE_INCREMENT;
        free (old_handles);
    }
    
    for (i = 0; i < handleMax; i++) {
        if (handles[i] == NULL) {
            handles[i] = elem;
            handleCount++;
            return ((i + 1));
        }
    }
     
    /*  If we get this far, it's an error condition.
    */
    vot_handleError ("ERROR: Handle overflow.");
    return (0);
}


/** 
 *  vot_freeHandle -- Free a handle for use (private method)
 * 
 *  @brief  Free a handle for use (private method)
 *  @fn     vot_freeHandle (handle_t handle)
 *
 *  @param  handle 	A handle_t to the Element you wish to free
 *  @return 		nothing
 */
void
vot_freeHandle (handle_t handle)
{
    if (handle < handleMax) {
        handles[(handle - 1)] = NULL;
        handleCount--;

    } else
        vot_handleError ("ERROR: Handle overflow.");
}


/** 
 *  vot_getElement -- Get the Element refered to by handle_t (private method)
 *  
 *  @brief Get the Element refered to by handle_t (private method)
 *  @fn    Element *vot_getElement (handle_t handle)
 *
 *  @param  handle 	A handle_t to the Element.
 *  @return 		A pointer to the requested Element.
 */
Element *
vot_getElement (handle_t handle)
{

    if (handle == 0)
        vot_handleError ("ERROR: Handle NULL.");
        /*return (NULL);*/

    else if (handle > handleMax) 
        vot_handleError ("ERROR: Handle overflow.");
    
    else if (handle < handleMax) 
        return (Element *) handles[(handle - 1)];

    return (NULL);
}


/** 
 *  vot_handleCleanup -- Free all the handle nodes (private method)
 *
 *  @brief  Free all the handle nodes (private method)
 *  @fn     vot_handleCleanup (void)
 *
 *  @return 		nothing
 */
void
vot_handleCleanup (void)
{
    unsigned int i = 0;
    
    for (i = 0; i < handleMax; i++) {
        if (handles[i] != NULL)
            free (handles[i]);
    }
    handleMax   = 0;
    handleCount = 0;
    
    free (handles);
    handles = NULL;
    
    vot_newHandleTable ();	/*  FIXME ???? */
}


/** 
 *  vot_newHandleTable -- Initialize a handle table (private method)
 *
 *  @brief  Initialize a handle table (private method)
 *  @fn     vot_newHandleTable (void)
 *
 *  @return 		nothing
 */
void
vot_newHandleTable (void)
{
    if (handles == NULL)
        handles = (Element **) calloc (HANDLE_INCREMENT, sizeof(Element *));
}


/**
 *  vot_handleError -- Print an error message.
 *
 *  @brief  Print an error message.
 *  @fn     vot_handleError (char *msg)
 *
 *  @return 		nothing
 */
void
vot_handleError (char *msg)
{
    fprintf (stderr, "%s\n", msg);
}
