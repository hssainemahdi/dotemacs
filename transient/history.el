((magit-branch nil)
 (magit-commit nil
               ("--no-verify")
               ("--signoff"))
 (magit-pull
  ("--rebase")
  nil)
 (magit-push
  ("--force"))
 (magit-rebase
  ("--autostash" "--interactive")
  ("--autostash")
  nil)
 (magit-stash nil
              ("--include-untracked"))
 (magit-status-jump nil))
