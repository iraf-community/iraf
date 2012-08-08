/**
 *  VOAPPS.C -- Generic task main() for the VOClient Package applications.
 *
 *  @file       voApps.c
 *  @author     Mike Fitzpatrick
 *  @date       6/03/11
 *
 *  @brief      Generic task main() for the VOClient Package applications.
 */

#include <stdio.h>
#include <string.h>

#include "voApps.h"


/**
 *  Program main().
 */
int
main (int argc, char *argv[])
{
    Task  *app    = (Task *) NULL;
    char  *task   = (char *) NULL;
    int    status = 0;


    /*  Get the task name minus any leading path prefix.
     */
    task = argv[0] + strlen (argv[0]) - 1;
    while (task > argv[0]) {
	if ( *(task - 1) != '/')
	    task--;
	else
	    break;
    }

    /*  Loop through the application table.  If the names match, call
     *  the entry-point function.
     */
    for (app = voApps; app->taskName; app++) {
	if (strcmp (app->taskName, task) == 0)
	    return ( (status = (*app->func)(argc, argv) ));
    }

    return (status);
}
