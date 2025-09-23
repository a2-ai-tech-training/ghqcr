.catch <- function(cnd) {
  rlang::catch_cnd(
    {
      if (rlang::is_condition(cnd)) {
        cnd[["message"]] <- cnd[["value"]]
        rlang::cnd_signal(cnd)
      }
      cnd
    },
    "extendr_err"
  )
  cnd
}
