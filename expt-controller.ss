; Calculates b^n - In this case 3^10. Changes initial-set instructions to compute 
; various exponents.
(controller
   (initial-set b (const 3))
   (initial-set n (const 10))

   (assign result (const 1))

   expt-begin
   (test (op =) (reg n) (const 0))
   (branch (label expt-done))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label expt-resume))
   (save continue)
   (goto (label expt-begin))
   
   expt-resume
   (restore n)
   (assign result (op *) (reg result) (reg b))
   
   expt-done
   (restore continue)
   (goto (reg continue)))
