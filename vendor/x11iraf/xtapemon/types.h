/*
 * types.h : Definitions of SearchType and SortType and external defs
 *	of their resource converters and the improved Widget converter.
 *
 * George Ferguson, ferguson@cs.rochester.edu, 4 Sep 1991.
 *
 */

#ifndef CONVERT_H
#define CONVERT_H

/* This puke is so the bloody converters work. Argh! */
#define GfDefault	'\000'
#define GfInvdate	'\001'

#define GfNDefault	"default"
#define GfNInvdate	"invdate"

extern void initConverters();
extern void cvtStringToWidget();

#endif /* CONVERT_H */
