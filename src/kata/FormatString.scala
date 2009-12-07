package kata

// Adds Python-like formatting operator to strings
class FormatString(string: String) {
  def %(values: Any*) = string.format(values: _*)
}

object FormatString {
  implicit def implicitFormatString(s: String) = new FormatString(s)
}
