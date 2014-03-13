
class RyansConverter {

  def apply(v: Int): String = convert(v.toString)

  object Sections extends Enumeration {
    val base, thousand, million, billion = Value
  }


  def convert(value: String) = {
    def validate(something: String):String = {
      if (something.length % 3 == 0)
        something
      else
        validate("0" + something)
    }
    val validated = validate(value)
    val segmentCount = validated.length / 3

    var output: String = ""
    for (segment <- 1 to segmentCount) {
      val (substringStart, substringEnd) = ((segment -1) * 3, segment * 3)
      val numericString = NumberConverter.BaseConverter.convert(validated.substring(substringStart, substringEnd)).trim
      val segmentIdentifier = if (segment != segmentCount) Sections(segmentCount - segment) else ""
      output += s" $numericString $segmentIdentifier"
    }
    output.trim
  }

}
