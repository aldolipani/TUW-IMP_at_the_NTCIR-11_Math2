package at.ac.tuwien.utility

/**
 * Created by aldo on 03/02/15.
 */
object MathTemplate {
  val GREEN_GRADING = Array[String]("#edf8fb", "#b2e2e2", "#66c2a4", "#238b45")
  val RED_GRADING = Array[String]("#e34a33", "#fdbb84", "#fee8c8")
  val TITLE = "${TITLE}"
  val QUERY = "${QUERY}"
  val CONTENT = "${CONTENT}"
  val R = "${R}"
  val TR1 = "${TR1}"
  val TR2 = "${TR2}"
  val TR3 = "${TR3}"
  val TR4 = "${TR4}"
  val R1 = "${R1}"
  val R2 = "${R2}"
  val R3 = "${R3}"
  val R4 = "${R4}"

  val STYLE_HEADING = "${STYLE_HEADING}"

  val VOID = "${}"
  val LIST = "<ul class=\"list-group\">${}</ul>"
  val ITEM = "<li class=\"list-group-item\">${}</li>"
  val ROW = "<div class=\"row\">${}</div>"
  val COL = "<div class=\"col-md-6\">${}</div>"
  val PANEL = "<div class=\"panel panel-default\">" +
    "<div class=\"panel-heading\" style=\"${STYLE_HEADING}\" >${TITLE}</div>" +
    "<div class=\"panel-body\">${CONTENT}</div>" +
    "</div>"

  val TABLE = "<table class=\"table table-condensed\">${}</table>"

}
