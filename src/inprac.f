
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine inprac

c...Initialize annual production accumulators.                                

      include 'const.inc'
      include 'dovars.inc'
      include 'timvar.inc'
      include 'plot1.inc'
      include 'plot2.inc'
      include 'plot3.inc'

c...Local variables
      integer i, j

c...In the CROP system or SAVANNA, if it is the first month
c     of the growing season reset the accumulators.
      if (dofrst .or. doplnt) then
c...Aboveground carbon production                                             
         agcisa(UNLABL) = 0.0
         agcisa(LABELD) = 0.0
         agcacc = 0.0
         ptagc = 0.0
c...Belowground carbon production                                             
         bgcisa(UNLABL) = 0.0
         bgcisa(LABELD) = 0.0
         bgcacc = 0.0
         ptbgc = 0.0
      endif
                                                                        x

c...In the FOREST system or SAVANNA, if it is the first month
c     of the growing season reset the accumulators.
c... liu, 12oct2004, commented out if-then
c      if (dofone) then
c...Total forest carbon
         fcacc = 0

c...Leaf carbon production
         alvcis(UNLABL) = 0.0
         alvcis(LABELD) = 0.0
         rlvacc = 0.0

c...Fine root carbon production
         afrcis(UNLABL) = 0.0
         afrcis(LABELD) = 0.0
         frtacc = 0.0

c...Fine branch carbon production
         afbcis(UNLABL) = 0.0
         afbcis(LABELD) = 0.0
         fbracc = 0.0

c...Large wood carbon production
         alwcis(UNLABL) = 0.0
         alwcis(LABELD) = 0.0
         rlwacc = 0.0

c...Coarse root carbon production
         acrcis(UNLABL) = 0.0
         acrcis(LABELD) = 0.0
         crtacc = 0.0
c...Liu, 12oct2004, commented out if-then
c      endif

c...N, P, and S uptake by plants                                              
      if (month .eq. 1) then
         do 20 i = 1, MAXIEL
           eupacc(i) = 0.0
           eupaga(i) = 0.0
           eupbga(i) = 0.0
	   do 10 j = 1,5
             eupprt(j,i) = 0.0
 10        continue
 20      continue
      endif

      return                                                                    
      end                                                                       
