val s = "А роза упала на лапу Азора"
s.split(" ").map(_.length).sum
s.length - s.count(_ == ' ')
s.count(_.isLetterOrDigit)
