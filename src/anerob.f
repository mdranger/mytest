
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


       real function anerob(aneref, drain, rprpet, pet, tave, micosm,
     $ somtc, cropdrain)

        real     aneref(3), drain, rprpet, pet, tave, cropdrain
        integer  micosm


c...This function calculates the impact of soil anerobic conditions
c     on decomposition.  It returns a multiplier 'anerob' whose value
c     is 0-1.
c...by using anerob and soil organic carbon level, 
c...If a microcosm is being simulated, return the value for aneref(3)
c     which is set by the user.

c...Called From:CYCLE
c     aneref(1) - ratio RAIN/PET with maximum impact
c     aneref(2) - ratio RAIN/PET with minimum impact
c     aneref(3) - minimum impact
c     drain     - percentage of excess water lost by drainage
c     newrat    - local var calculated new (RAIN+IRRACT+AVH2O(3))/PET ratio
c     pet       - potential evapotranspiration
c     rprpet    - actual (RAIN+IRRACT+AVH2O(3))/PET ratio
c     cropdrain - this value is set for > 0 when a management on the crop land
c               - when it is set to 0, use the original formula
 
c...Local variables
      real      newrat, slope, xh2o



      anerob = 1.0

c...Check if simulating a microcosm
      if (micosm .eq. 1) then
        anerob = aneref(3)
        return
      endif


c...Determine if RAIN/PET ratio is greater than the ratio with
c     maximum impact.
      if ((rprpet .gt. aneref(1)) .and. (tave .gt. 2.0)) then
        xh2o = (rprpet - aneref(1)) * pet * (1.0 - drain)
        if (xh2o .gt. 0) then
          newrat = aneref(1) + (xh2o / (0.0001+pet))
          slope = (1.0 - aneref(3)) / (aneref(1) - aneref(2))
          anerob = 1.0 + slope * (newrat - aneref(1))
        endif

        if (anerob .lt. aneref(3)) then
          anerob = aneref(3)
        endif
      endif
        
c... turn off original cals for anerob shown above 04/10/2007
c... add cropdrain variables for crops
      if ( cropdrain .eq. 0.0 ) then
          anerob =  (drain + 0.45 )/1.5
          anerob = 0.4 * anerob + 0.6 * min(1.0,0.0+exp(-2.5*
     $    (somtc-3000)/50000))     
      else
          anerob = cropdrain + 0.3
      endif

c...make sure the output is between 0.0 and 1.0
      if ( anerob .gt. 1.0 ) then
          anerob = 1.0
      endif
      
      
      return
      end
