
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


c**** time is in a separate common block for compatability with TIME0
c
      common /zztim/ time, title_flag, ftitle_flag
      real time
      integer title_flag
c    Added by zp, flag for forest monthly output
      integer ftitle_flag

      save /zztim/
