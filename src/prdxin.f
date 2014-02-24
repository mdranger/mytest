c new subroutine created to read a prdx scalar from
c input file prdxin.100

      subroutine prdxin(curyr)

      integer curyr
      

c...Read in the new fert type
c
      include 'const.inc'
      include 'chrvar.inc'
      include 'param.inc'

c...Local variables
      integer   inyear
      real      temp
      character fromdat*5, name*20
      character*100 fixfile

c...open the file under the 100files directory
      fixfile = ' '

      fixfile(1:pathlen) = libpath
      fixfile(pathlen+1:pathlen+11) = '\prdxin.100'

      open(unit=11,file=fixfile,status='OLD')

      rewind(11)
20    continue
      read(11, 100, end=200) inyear
      if (curyr .ne. inyear) then
          read(11, *) temp, name
        goto 20
      else
c...find the right time and read in the scalar     
        read(11, *) prdx_scalar, name 
 
        call ckdata('schedl','prdxin',name)
        close(11)
!  curfert = tomatch 
      endif


      return

100   format(I3)

200   continue
      call message(' Error reading in values from the prdxin.100 file.')
      print *,'   Looking for prdx scalar: ', curyr
      STOP


      end
