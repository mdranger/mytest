
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


c**** Character values needed in common block:
c     cmdary    array holding command for each event in the block
c     curcrp    the current crop
c     curtre    the current tree
c     initcp	the initial crop
c     initre	the initial tree
c     typary    array holding command type for each event in block
c     wlabel	string of weather labels "prec", "tmin", "tmax"
c     wthnam    if wthr = F, the weather file name
c     wthr      the weather choice
c     flaglc    flag to indicate the change of land cover, 05/23/08
c     prelc     previous year lc, can be crop or tree 05/23/08
c               used with cur crp to set the value of flaglc

      common/chrvar/cmdary(5000), curcrp, curtre, initcp, initre, 
     $              typary(5000), wlabel(3), wthnam, wthr
     $              , libpath, pathlen, pretre

      character*100 libpath
      character*20 cmdary, wthnam, wthr
      character*5 curcrp, curtre, initcp, initre, typary, pretre
      character*5 wlabel
!      character*4 wlabel
      integer     pathlen

      save /chrvar/
