/************************************************************************
 * Call the Sesame name resolver with the specified target.
 *
 * Usage:        resolver <target>
 *
 * Or call with no args for the built-in unit test.
 *
 *  M. Fitzpatrick, NOAO, July 2006
 */


#include <stdio.h>
#include <stdlib.h>
#include "VOClient.h"


char	*target  = "m31";

int main (int argc, char *argv[])
{
    Sesame  sr = (Sesame) NULL;

    /*  Process command line arguments. 
     */
    target = (argc <= 1 ? "m31" : argv[1]);

    /*  Now call the Resolver Service and summarize the results.   We'll 
     *  let the interface initialize the VO Client server and simply call
     *  the procedure we need.
     */
    sr = voc_nameResolver (target);

    printf ("target: %s   ra=%f  dec=%f  (%s)\n", 
	target, voc_resolverRA(sr), voc_resolverDEC(sr), voc_resolverPos(sr) );

    return (0);
}
