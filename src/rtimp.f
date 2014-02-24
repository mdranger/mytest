
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...RTIMP.F

	real function rtimp(riint, rictrl, rootc)

	real      riint, rictrl, rootc

c...This function calculates and returns a value between 0-1 which is the impact
c     of root biomass on available nutrients.  It is used in the calculation of
c     total plant production in RESTRP and SRESTRP.


c...Called From:       GROWTH
c                      SIMSOM
c                      TREES

        rtimp = (1.0 - riint * exp(-rictrl * rootc * 2.5))

        return
        end
