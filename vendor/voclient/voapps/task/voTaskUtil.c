/**
 *  VOTASKUTIL.C  -- Utility methods to convert pointers to user handles.
 *
 *  @brief      Utility methods to convert pointers to user handles.
 *
 *  @file       voTaskUtil.c
 *  @author     Mike Fitzpatrick
 *  @date       9/24/12
 */

#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <time.h>
#include <ctype.h>
#include "voTask.h"


#define	MAX_HANDLES	128


int     numHandles         = 0;
long    taskHandles[MAX_HANDLES];

/*  Public procedures
*/
handle_t  task_P2H (void *ptr);
void     *task_H2P (handle_t handle);
handle_t  task_newHandle (void *ptr);
void      task_freeHandle (handle_t handle);



/*  Utility routines for keep track of handles.
*/

/**
 *  TASK_NEWHANDLE -- Get an unused object handle.
 * 
 *  @brief  Get an unused object handle
 *  @fn	    handle_t task_newHandle (void *ptr)
 *
 *  @param  ptr		pointer to object to be stored
 *  @return		new object handle
 */
handle_t
task_newHandle (void *ptr)
{
    /* Initialize the handle-to-ptr converter the first time we're called,
    ** or whenever we've restarted.
    */
    if (numHandles == 0)
	memset (taskHandles, 0, sizeof (taskHandles));
    taskHandles[++numHandles] = (long) ptr;

    return (numHandles);
}


/**
 *  TASK_FREEHANDLE -- Free the handle for later re-use.
 *
 *  @brief  Free the handle for later re-use.
 *  @fn     task_freeHandle (handle_t handle)
 *
 *  @param  handle	object handle
 *  @return 		nothing
 */
void
task_freeHandle (handle_t handle)
{
    register int i, j;
    void *ptr = task_H2P (handle);


    if (handle <= 0) {
	fprintf (stderr, "Error: Attempt to free zero handle!\n");
	return;
    }

    for (i=1; i < MAX_HANDLES; i++) {
	if ((void *) ptr == (void *) taskHandles[i]) {
	    for (j=i+1; j < MAX_HANDLES; j++) {
		if (taskHandles[j])
		    taskHandles[i++] = taskHandles[j];
		else
		    break;
	    }
	    numHandles = ((numHandles-1) >= 0 ? (numHandles-1) : 0);
	    break;
 	}
    }
}


/**
 *  TASK_P2H -- Convert a pointer to a handle
 *
 *  @brief  Convert a pointer to a handle
 *  @fn     handle_t task_P2H (void *ptr)
 *
 *  @param  ptr		pointer to object
 *  @return 		handle to object, < 0 on error
 */
handle_t	
task_P2H (void *ptr)
{
    register int i;

    for (i=1; i < MAX_HANDLES; i++) {
	if ((void *) ptr == (void *) taskHandles[i])
	    return ((int) i);
    }

    return (-1);
}


/**
 *  TASK_H2P -- Convert a handle to a pointer
 *
 *  @brief  Convert a handle to a pointer
 *  @fn     void *task_H2P (int handle) 
 *
 *  @param  handle	object handle
 *  @return 		pointer to object or NULL
 */
void *
task_H2P (handle_t handle) 
{ 
    return ((void *) taskHandles[handle]); 
}
