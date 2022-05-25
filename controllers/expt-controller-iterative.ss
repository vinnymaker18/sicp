; This is an iterative process and doesn't need to use the stack - no
; save/restore instructions.
(controller
  (initial-set b (const 3))
  (initial-set n (const 10))

  (assign result (const 1))
  (assign counter (reg n))

  expt-begin
  (test (op =) (reg counter) (const 0))
  (branch (label expt-done))
  (assign counter (op -) (reg counter) (const 1))
  (assign result (op *) (reg result) (reg b))
  (goto (label expt-begin))

  expt-done
  (restore continue)
  (goto (reg continue)))
