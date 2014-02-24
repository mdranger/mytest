
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...FRESPR.F

       real function frespr(temp,nitrog)

       real      temp, nitrog


!...This function calculates the maintenance respiration per month
!     for the forest materials (fine root, coarse root, fine branch,
!     large wood, and leaf) using an equation derived by Mike Ryan.
!     Maintenance respiration and tissue N content are strongly
!     correlated.

!...Ref:  A simple method for estimating gross carbon budget
c           for vegetation in forest ecosystems.  (Appendix A)
c           Michael G. Ryan
c           Tree Physiology, February 1, 1991
c      
c      temp   :  tave - for leaf and stem, stemp - for root
c      nitrog :  rleave(1), rlwode(1), fbrche(1), croote(1), or froote(1)
c

       frespr =  ((.0106 / 4.0) * (12. / 14.) * 24 * 30 *
     $           exp((alog(2.) / 10.) * temp) * nitrog) * 1.1

       return
       end
