import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter


val x = None

val y = x.orElse(Some("hi"))

val filtered = x.filter(s => s == "blah")