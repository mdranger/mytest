
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      real function line(x, x1, y1, x2, y2)
      real x, x1, y1, x2, y2

c****	This function is the generic equation of a line from
c	two points.
c	CALLED FROM:	co2eff.f

      line = (y2 - y1) / (x2 - x1) * (x - x2) + y2

	return
	end
