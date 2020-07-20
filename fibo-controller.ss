(controller
   (initial-set n (const 15))
   fibonacci-begin
   (assign result (reg n))
   (test (op <) (reg n) (const 2))
   (branch (label fibonacci-done))
   (assign result (const 0))
   (save n)
   (assign n (op -) (reg n) (const 1))
   (assign continue (label fibonacci-resume-1))
   (save continue)
   (goto (label fibonacci-begin))

   fibonacci-resume-1
   (restore n)
   (save result)
   (save n)
   (assign n (op -) (reg n) (const 2))
   (assign continue (label fibonacci-resume-2))
   (save continue)
   (goto (label fibonacci-begin))

   fibonacci-resume-2
   (restore n)
   (restore temp)
   (assign result (op +) (reg temp) (reg result))

   fibonacci-done
   (restore continue)
   (goto (reg continue)))
