
/*              Copyright 1993 Colorado State University     */
/*                      All Rights Reserved                  */


/*	flowd.c
 *	Global data file for flow routines in century.
 */

#define LENFST 500	/* Length of flowstack */

struct stack{
	float *from;	/* Source */
	float *to;	/* Destination */
	float when;	/* Time to flow */
	float amt;	/* Amount */
} flowstack[LENFST+1];

int nflows;		/* Number of flows */

