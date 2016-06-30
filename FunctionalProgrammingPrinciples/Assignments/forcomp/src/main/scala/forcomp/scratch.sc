import forcomp.Anagrams

val s = "sous"

val g = s.groupBy(c => c).toList

Anagrams.wordOccurrences("sous")

val m = Map(('a', 1), ('b',2))

val x = m - 'a'