(controller
   (initial-set n (const 10))
   (assign result (const 1))

   factorial-begin
   (test (op =) (reg n) (const 1))
   (branch (label factorial-done))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label factorial-resume))
   (save continue)
   (goto (label factorial-begin))

   factorial-resume
   (restore n)
   (assign result (op *) (reg n) (reg result))

   factorial-done
   ; Goes back to caller (external to this controller at final step).
   (restore continue)
   (goto (reg continue)))
