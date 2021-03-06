      DOUBLE PRECISION FUNCTION SCALE(NN,PT,ETA,INFIN,SMALNO,BASE)      SCAL4160
C RETURNS A SCALE FACTOR TO MULTIPLY THE COEFFICIENTS OF THE
C POLYNOMIAL. THE SCALING IS DONE TO AVOID OVERFLOW AND TO AVOID
C UNDETECTED UNDERFLOW INTERFERING WITH THE CONVERGENCE
C CRITERION.  THE FACTOR IS A POWER OF THE BASE.
C PT - MODULUS OF COEFFICIENTS OF P
C ETA,INFIN,SMALNO,BASE - CONSTANTS DESCRIBING THE
C FLOATING POINT ARITHMETIC.
      DOUBLE PRECISION PT(NN),ETA,INFIN,SMALNO,BASE,HI,LO,
     *    MAX,MIN,X,SC,DSQURT,DLOG
C FIND LARGEST AND SMALLEST MODULI OF COEFFICIENTS.
      HI = DSQRT(INFIN)
      LO = SMALNO/ETA
      MAX = 0.0D0
      MIN = INFIN
      DO 10 I = 1,NN
          X = PT(I)
          IF (X .GT. MAX) MAX = X
          IF (X .NE. 0.0D0 .AND. X.LT.MIN) MIN = X
   10 CONTINUE
C SCALE ONLY IF THERE ARE VERY LARGE OR VERY SMALL COMPONENTS.
      SCALE = 1.0D0
      IF (MIN .GE. LO .AND. MAX .LE. HI) RETURN
      X = LO/MIN
      IF (X .GT. 1.0D0) GO TO 20
          SC = 1.0D0/(DSQRT(MAX)*DSQRT(MIN))
          GO TO 30
   20 SC = X
      IF (INFIN/SC .GT. MAX) SC = 1.0D0
   30 L = DLOG(SC)/DLOG(BASE) + .500
      SCALE = BASE**L
      RETURN
      END
