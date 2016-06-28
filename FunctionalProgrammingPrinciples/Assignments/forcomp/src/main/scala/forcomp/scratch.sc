import forcomp.Anagrams

val s = "sous"

val g = s.groupBy(c => c).toList

Anagrams.wordOccurrences("sous")