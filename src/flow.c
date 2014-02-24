
/*              Copyright 1993 Colorado State University     */
/*                      All Rights Reserved                  */

#include "flow.h"

/*	flow.c
 *      Schedules a flow by storing the arguments in a stack which
 *      will be accessed by the function 'flowup' which performs
 *      the actual flow for all stack elements where 'when' is
 *      less than or equal to time.
 *
 *	Called from the Fortran version, the underscore in flow_
 *	allows for correct referencing during linkage.  No 
 *	wrapper - this routine was written to be called directly
 *	from Fortran.
 */

void
flow_(float *from, float *to, float *when, float *howmuch)
{
	/* Increment the number of flows stored in the stack */

	nflows += 1;

	if (nflows > LENFST)
	{
	/* Stack Overflow */
		flow_err(1, *when);
		exit(1);
	}


	else
	/* Store the arguments in the stack */
	{
		flowstack[nflows].from = from;
		flowstack[nflows].to = to;
		flowstack[nflows].when = *when;
		flowstack[nflows].amt = *howmuch;
	}

	return;
}
