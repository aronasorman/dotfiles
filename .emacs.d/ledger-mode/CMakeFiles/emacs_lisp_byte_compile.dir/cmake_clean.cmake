FILE(REMOVE_RECURSE
  "CMakeFiles/emacs_lisp_byte_compile"
  "ledger-commodities.elc"
  "ledger-complete.elc"
  "ledger-exec.elc"
  "ledger-fonts.elc"
  "ledger-init.elc"
  "ledger-mode.elc"
  "ledger-occur.elc"
  "ledger-post.elc"
  "ledger-reconcile.elc"
  "ledger-regex.elc"
  "ledger-report.elc"
  "ledger-schedule.elc"
  "ledger-sort.elc"
  "ledger-state.elc"
  "ledger-test.elc"
  "ledger-texi.elc"
  "ledger-xact.elc"
)

# Per-language clean rules from dependency scanning.
FOREACH(lang)
  INCLUDE(CMakeFiles/emacs_lisp_byte_compile.dir/cmake_clean_${lang}.cmake OPTIONAL)
ENDFOREACH(lang)
