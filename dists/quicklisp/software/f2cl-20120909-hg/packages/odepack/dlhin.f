*DECK DLHIN
      SUBROUTINE DLHIN (NEQ, N, T0, Y0, YDOT, F, TOUT, UROUND,
     1   EWT, ITOL, ATOL, Y, TEMP, H0, NITER, IER)
      EXTERNAL F
      DOUBLE PRECISION T0, Y0, YDOT, TOUT, UROUND, EWT, ATOL, Y,
     1   TEMP, H0
      INTEGER NEQ, N, ITOL, NITER, IER
      DIMENSION NEQ(*), Y0(*), YDOT(*), EWT(*), ATOL(*), Y(*), TEMP(*)
C-----------------------------------------------------------------------
C Call sequence input -- NEQ, N, T0, Y0, YDOT, F, TOUT, UROUND,
C                        EWT, ITOL, ATOL, Y, TEMP
C Call sequence output -- H0, NITER, IER
C Common block variables accessed -- None
C
C Subroutines called by DLHIN: F, DCOPY
C Function routines called by DLHIN: DVNORM
C-----------------------------------------------------------------------
C This routine computes the step size, H0, to be attempted on the
C first step, when the user has not supplied a value for this.
C
C First we check that TOUT - T0 differs significantly from zero.  Then
C an iteration is done to approximate the initial second derivative
C and this is used to define H from WRMS-norm(H**2 * yddot / 2) = 1.
C A bias factor of 1/2 is applied to the resulting h.
C The sign of H0 is inferred from the initial values of TOUT and T0.
C
C Communication with DLHIN is done with the following variables:
C
C NEQ    = NEQ array of solver, passed to F.
C N      = size of ODE system, input.
C T0     = initial value of independent variable, input.
C Y0     = vector of initial conditions, input.
C YDOT   = vector of initial first derivatives, input.
C F      = name of subroutine for right-hand side f(t,y), input.
C TOUT   = first output value of independent variable
C UROUND = machine unit roundoff
C EWT, ITOL, ATOL = error weights and tolerance parameters
C                   as described in the driver routine, input.
C Y, TEMP = work arrays of length N.
C H0     = step size to be attempted, output.
C NITER  = number of iterations (and of f evaluations) to compute H0,
C          output.
C IER    = the error flag, returned with the value
C          IER = 0  if no trouble occurred, or
C          IER = -1 if TOUT and t0 are considered too close to proceed.
C-----------------------------------------------------------------------
C
C Type declarations for local variables --------------------------------
C
      DOUBLE PRECISION AFI, ATOLI, DELYI, HALF, HG, HLB, HNEW, HRAT,
     1     HUB, HUN, PT1, T1, TDIST, TROUND, TWO, DVNORM, YDDNRM
      INTEGER I, ITER
C-----------------------------------------------------------------------
C The following Fortran-77 declaration is to cause the values of the
C listed (local) variables to be saved between calls to this integrator.
C-----------------------------------------------------------------------
      SAVE HALF, HUN, PT1, TWO
      DATA HALF /0.5D0/, HUN /100.0D0/, PT1 /0.1D0/, TWO /2.0D0/
C
      NITER = 0
      TDIST = ABS(TOUT - T0)
      TROUND = UROUND*MAX(ABS(T0),ABS(TOUT))
      IF (TDIST .LT. TWO*TROUND) GO TO 100
C
C Set a lower bound on H based on the roundoff level in T0 and TOUT. ---
      HLB = HUN*TROUND
C Set an upper bound on H based on TOUT-T0 and the initial Y and YDOT. -
      HUB = PT1*TDIST
      ATOLI = ATOL(1)
      DO 10 I = 1,N
        IF (ITOL .EQ. 2 .OR. ITOL .EQ. 4) ATOLI = ATOL(I)
        DELYI = PT1*ABS(Y0(I)) + ATOLI
        AFI = ABS(YDOT(I))
        IF (AFI*HUB .GT. DELYI) HUB = DELYI/AFI
 10     CONTINUE
C
C Set initial guess for H as geometric mean of upper and lower bounds. -
      ITER = 0
      HG = SQRT(HLB*HUB)
C If the bounds have crossed, exit with the mean value. ----------------
      IF (HUB .LT. HLB) THEN
        H0 = HG
        GO TO 90
      ENDIF
C
C Looping point for iteration. -----------------------------------------
 50   CONTINUE
C Estimate the second derivative as a difference quotient in f. --------
      T1 = T0 + HG
      DO 60 I = 1,N
 60     Y(I) = Y0(I) + HG*YDOT(I)
      CALL F (NEQ, T1, Y, TEMP)
      DO 70 I = 1,N
 70     TEMP(I) = (TEMP(I) - YDOT(I))/HG
      YDDNRM = DVNORM (N, TEMP, EWT)
C Get the corresponding new value of H. --------------------------------
      IF (YDDNRM*HUB*HUB .GT. TWO) THEN
        HNEW = SQRT(TWO/YDDNRM)
      ELSE
        HNEW = SQRT(HG*HUB)
      ENDIF
      ITER = ITER + 1
C-----------------------------------------------------------------------
C Test the stopping conditions.
C Stop if the new and previous H values differ by a factor of .lt. 2.
C Stop if four iterations have been done.  Also, stop with previous H
C if hnew/hg .gt. 2 after first iteration, as this probably means that
C the second derivative value is bad because of cancellation error.
C-----------------------------------------------------------------------
      IF (ITER .GE. 4) GO TO 80
      HRAT = HNEW/HG
      IF ( (HRAT .GT. HALF) .AND. (HRAT .LT. TWO) ) GO TO 80
      IF ( (ITER .GE. 2) .AND. (HNEW .GT. TWO*HG) ) THEN
        HNEW = HG
        GO TO 80
      ENDIF
      HG = HNEW
      GO TO 50
C
C Iteration done.  Apply bounds, bias factor, and sign. ----------------
 80   H0 = HNEW*HALF
      IF (H0 .LT. HLB) H0 = HLB
      IF (H0 .GT. HUB) H0 = HUB
 90   H0 = SIGN(H0, TOUT - T0)
C Restore Y array from Y0, then exit. ----------------------------------
      CALL DCOPY (N, Y0, 1, Y, 1)
      NITER = ITER
      IER = 0
      RETURN
C Error return for TOUT - T0 too small. --------------------------------
 100  IER = -1
      RETURN
C----------------------- End of Subroutine DLHIN -----------------------
      END
