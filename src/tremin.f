c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...TREMIN.F

      subroutine tremin(tomatch,curtrm)

      character*5 tomatch
      character*5 curtrm

c...Read in the new tree removal type

      include 'forrem.inc'
      include 'const.inc'
      include 'chrvar.inc'
      include 'param.inc'

c...Local variables
      integer   i, TREMLNS, found
      real      temp
      character fromdat*5, name*20, string*80


c...Number of lines to read for each tree removal type
      parameter (TREMLNS = 20)

      character*100 fixfile

      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+9) = '\trem.100'

c      write(*,*) fixfile

      open(unit=11,file=fixfile,status='OLD')


c      open(unit=11, file='trem.100',status='OLD')
      rewind(11)
20    continue
      read(11, 100, end=200) fromdat
c          print *, tomatch, fromdat      
c      call ckdata(tomatch,fromdat,name)
      if (tomatch .ne. fromdat) then 
c       if (found .eq. 1) then
        do 25 i = 1, TREMLNS 
          read(11, *) temp, name
25      continue
        goto 20
      else
        read(11, *) temp, name
        evntyp = int(temp)
        call ckdata('schedl','evntyp',name)
        print *, "Trem", name
        read(11, *) remf(LEAF), name 
        if (remf(leaf) > 0.90) then
                prdx(3) = 0.01
        endif
        call ckdata('schedl','remf',name)
        read(11, *) remf(FROOT), name 
        call ckdata('schedl','remf',name)
        read(11, *) remf(FBRCH), name 
        call ckdata('schedl','remf',name)
        read(11, *) remf(LWOOD), name 
        call ckdata('schedl','remf',name)
        read(11, *) remf(CROOT), name 
        call ckdata('schedl','remf',name)
        read(11, *) fd(1), name 
        call ckdata('schedl','fd',name)
        read(11, *) fd(2), name 
        call ckdata('schedl','fd',name)
        read(11, *) retf(1,1), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(1,2), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(1,3), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(1,4), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(2,1), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(2,2), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(2,3), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(2,4), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(3,1), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(3,2), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(3,3), name 
        call ckdata('schedl','retf',name)
        read(11, *) retf(3,4), name 
        call ckdata('schedl','retf',name)
        close(11)
        curtrm = tomatch 
      endif


      return

100   format(a5)

200   continue
      call message('   Error reading in values from the trem.100 file.')
      string = '   Looking for type: ' // tomatch
      call message(string)
      STOP

      end
