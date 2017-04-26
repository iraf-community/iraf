/*
 * appres.h : External defs for files needing the application
 *	resources.
 *
 * George Ferguson, ferguson@cs.rochester.edu, 12 Sep 1991.
 *
 */

#ifndef APP_RESOURCES_H
#define APP_RESOURCES_H

typedef struct {
    String	widgets;
    int		alternate;
    int		port;
    int		debug;
} AppResources;

extern AppResources appResources;

#endif /* APP_RESOURCES_H */
