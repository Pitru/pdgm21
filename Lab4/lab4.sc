import scala.annotation.tailrec

//used for tail recursion functions
//complexity  O(n) where n is list.length
//memory (n^2)/2 + 1 because n, n-1, n-2, ... 1 and
def reverseList[A](list: List[A]): List[A] ={
  @tailrec
  def innerReverse[A](list: List[A], reversedList: List[A]): List[A] ={
    list match
      case h1:: t1 => innerReverse(t1, h1 :: reversedList)
      case Nil => reversedList
  }
  innerReverse(list, List())
}


def containsPhrase(inputString: String, frase: String): Boolean={
  if frase.isBlank then true
  else if inputString.isBlank then false
  else if inputString.head == frase.head then
    containsPhrase(inputString.tail, frase.tail)
  else containsPhrase(inputString.tail, frase)
}

def containsPhrases(inputString: String, phrases: List[String]): Boolean={
  if phrases == Nil then false
  else if phrases.head.isBlank then false
  else containsPhrase(inputString, phrases.head) || containsPhrases(inputString, phrases.tail)
}
//complexity
//O(n^4), where n is max(functions below V )
// list.length * frases.length * list[number].length * frases[number].length

def find(list: List[String], phrases: List[String]): List[String] ={
  list match
    case h::t => if containsPhrases(h, phrases) then h :: find(t, phrases)
                          else find(t, phrases)
    case Nil => Nil
}

def findTail(list: List[String], phrases: List[String]): List[String]= {
  @tailrec
  def iterlist(list: List[String], phrases: List[String], result: List[String]): List[String]={
    list match
      case h::t => if containsPhrases(h, phrases) then iterlist(t, phrases, h :: result)
        else iterlist(t, phrases, result)
      case Nil => result
  }
  reverseList(iterlist(list, phrases, List()))
}

find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))
findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))
find(List("abc","def"),List(""))



//complexity O(n) where n is first.length + second.length
//linear complexity
//memory
//doesn't make copies, memory needed for arguments and result
//so (first.length + second.length + third.length)*2
// multiply by 2 because remember result

//tail recursion reduce stack depth
//complexity in tail recursion function is O(2n) still linear
//because it needs to be iterate once to reverse
// memory


def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  (first, second) match
    case (h1::t1, l2) => h1 :: joinLists(t1, l2, third)
    case (Nil, h2::t2) => h2 :: joinLists(Nil, t2, third)
    case (Nil, Nil) => third
}

def joinListsTail[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  @tailrec
  def iter[A](first: List[A], second: List[A], third: List[A], finalList: List[A]): List[A] ={
    (first, second, third) match
      case (h1::t1, l2, l3) => iter(t1, l2, l3, h1:: finalList)
      case (Nil, h2::t2, l3) => iter(Nil, t2, l3, h2 :: finalList)
      case (Nil, Nil, h3::t3) => iter(Nil, Nil, t3, h3 :: finalList)
      case(Nil, Nil, Nil) => finalList
  }
  reverseList(iter(first, second, third, List()))
}

val a = List(1,2,3,4,5,6)
val b = List(7,8,9)
val c = List(10,11,12)
joinLists(a,b,c)
joinListsTail(a,b,c)