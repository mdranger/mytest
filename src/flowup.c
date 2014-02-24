
/*              Copyright 1993 Colorado State University     */
/*                      All Rights Reserved                  */

#include "flow.h"

/*	flowup.c
 *	Perform the flows for all stack elements whose 'when'
 *	value is less than or equal to time.
 *
 *	Called from the Fortran version, the underscore in flowup_
 *	allows for correct referencing during linkage.  No 
 *	wrapper - this routine was written to be called directly
 *	from Fortran.
 */

void
flowup_(float *time)
{
	int FlowsDone, FlowsNotDone, i;

	FlowsNotDone = 0;
	FlowsDone = 0;

	if (nflows <= 0.0)
		return;

	/* If there are any flows in the stack, determine which
	 * need to go now and do it.
	 */
	else
		for (i=1; i<=nflows; i++)
		{
			if (*time < flowstack[i].when)
			{
				FlowsNotDone +=1;
				/* This one doesn't need to be done yet; 
				 * move it down the stack if other flows
				 * have been performed already.
				 */
				if (FlowsDone > 0)
					flowstack[FlowsNotDone] = flowstack[i];
			}
			else
			{
				if (flowstack[i].amt != 0.0)
				{
					*(flowstack[i].from) -= flowstack[i].amt;
					*(flowstack[i].to) += flowstack[i].amt;
				}
				FlowsDone += 1;
			}

		}

	nflows = FlowsNotDone;
	return;
}
