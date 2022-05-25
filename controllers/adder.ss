(controller
  (assign result (op +) (op read) (op read))
  (restore continue)
  (goto (reg continue)))
