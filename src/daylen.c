
/*              Copyright 1993 Colorado State University     */
/*                      All Rights Reserved                  */

#include "flow.h"
#define M_PI 3.1415926536 

float
daylen_(int *month, float *sitlat)
{

 /*  Author: Melannie Hartman	*/
 /*  Changed 1/96 for inclusion in Gridded Century  -*/
 /*  Compute daylength		*/
 /*  rlat:	latitude in radians	*/

     double adelt;
     double ahou;
     float  dylngth;	/* daylength in hours	*/
     double rlat;	/* latitude in radians	*/
     double temp1, temp2;
     double jday[] = {	1.	/* Jan 1 julian day	*/,
			32.	/* Feb 1 julian day	*/,
			61.	/* Mar 1 julian day	*/,
			92.	/* Apr 1 julian day	*/,
			122.	/* May 1 julian day	*/,
			153.	/* Jun 1 julian day	*/,
			183.	/* Jul 1 julian day 	*/,
			214.	/* Aug 1 julian day	*/,
			245.	/* Sep 1 julian day	*/,
			275.	/* Oct 1 julian day	*/,
			306.	/* Nov 1 julian day	*/,
			337.	/* Dec 1 julian day	*/};

 /* NOTE: JDAY isn't exact, but should be close enough. */

 /* Convert sitlat which is in degrees to radians	*/
     rlat = (*sitlat * M_PI / 180.0);

 /* Passing month in from FORTRAN - subtract 1 for zero based */
     temp1 = 2.0 * M_PI * (jday[*month-1] - 77.0) / 365.0;
     adelt = 0.4014 * sin((double)temp1);
     temp1 = 1.0 - pow((-tan(rlat) * (adelt)), 2.0);
     if (temp1 < 0.0) temp1 = 0.0;
     temp1 = sqrt(temp1);
     temp2 = -tan(rlat) * tan(adelt);
     ahou = atan2(temp1,temp2);
     dylngth = (float)((ahou / M_PI) * 24.0);
     return dylngth;

}



