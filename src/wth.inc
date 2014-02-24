
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


c.... maxtt:  the month when max air temperature occurs
c	meant: mean air temperature = 0.5 * (max + min)
c	amp: amplitude of the air (surface) temperature

      common/wth/prcurr(12),prcnxt(12),tmn2m(12),tmx2m(12), 
     - maxtt, meant, amp
      real prcurr, prcnxt, tmn2m, tmx2m, maxtt, meant, amp

      save /wth/
