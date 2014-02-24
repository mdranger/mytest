
/*              Copyright 1993 Colorado State University     */
/*                      All Rights Reserved                  */

#include "flow.h"

void
floclr_()
{
	int i;

	/* Initialize variables used in flow routines */

	nflows = 0;
	for (i=0; i < LENFST; i++)
	{
		flowstack[i].from = (float *) NULL;
		flowstack[i].to = (float *) NULL;
		flowstack[i].when = 0.0;
		flowstack[i].amt = 0.0;
	}

	return;
}
