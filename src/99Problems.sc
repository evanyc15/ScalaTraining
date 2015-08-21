object _99Problems {
  /* P01 Find the last element of a list
 * ------------------------------------ */
  def last[A](arg: List[A]): A = {
    arg match {
      case head :: Nil => head // Double colon splits list to head :: tail. If tail is nil, then we at end of list
      case _ :: tail => last(tail)
      case _ => throw new NoSuchElementException
    }
  }

  /* P02 Find the last but one element of a list
   * Ex: List(1,1,2,3,5,8) = 5
   * ------------------------------------ */
  def lastbutOne[A](arg: List[A]): A = {
    arg match {
      case head :: (h :: Nil) => head // Get 2nd to last element with the following
      case _ :: tail => lastbutOne(tail)
      case _ => throw new NoSuchElementException
    }
  }

  /* P03 Find the Kth element of a list (list starts at index 0)
   * Ex: nth(2, List(1,1,2,3,5,8)) = 2
   * ------------------------------------ */
  // Using builtin functions
  def nthBuiltIn[A](nth: Int, list: List[A]): A = {
    if (nth >= 0) {
      return list(nth)
    } else {
      throw new NoSuchElementException
    }
  }

  // Using recursion
  def nthRecursion[A](nth: Int, list: List[A]): A = {
    (nth, list) match {
      case (0, h :: _) => h
      case (nth, _ :: tail) => nthRecursion(nth - 1, tail) // This is similar to tail recursion.
      case (_, Nil) => throw new NoSuchElementException
    }
  }

  /* P04 Find the number of elements of a list
   * ------------------------------------ */
  // Using builtin functions
  def lengthList[A](list: List[A]): Int = {
    return list.length
  }

  // Using recursion
  def lengthListRecursion[A](list: List[A]): Int = {
    def lengthListRecursionInner(acc: Int, list: List[A]): Int = {
      (acc, list) match {
        case (_, h :: Nil) => return acc + 1
        case (_, _ :: tail) => lengthListRecursionInner(acc + 1, tail) //acc is accumulator to count how many elements while tail-recursing
        case (_, Nil) => throw new NoSuchElementException
      }
    }
    lengthListRecursionInner(0, list)
  }

  /* P05 Reverse a list
   * ------------------------------------ */
  // Using builtin functions
  def reverseList[A](list: List[A]): List[A] = {
    return list.reverse
  }

  // Using recursion
  def reverseListRecursion[A](list: List[A]): List[A] = {
    def reverseListRecursionInner[A](accList: List[A], list: List[A]): List[A] = {
      list match {
        case Nil => accList // Return list
        case h :: tail => reverseListRecursionInner(h :: accList, tail) // Reverses list by taking head of list and putting it to back of result list=
      }
    }
    reverseListRecursionInner(Nil, list)
  }

  /* P06 Find out whether a list is a palindrome.
   * ------------------------------------ */
  // Using builtin functions
  def isPalindrome[A](list: List[A]): Boolean = {
    val reversed = list.reverse
    return list.equals(reversed)
  }

  // Using recursion (using our reverseListRecursion function defined)
  def isPalindromeRecursion[A](list: List[A]): Boolean = {
    val reversed = reverseListRecursion(list)
    return list.equals(reversed)
  }

  /* P07 Flatten a nested list structure
   * Ex: flatten(List(List(1,1), 2, List(3, List(5, 8)))) = List(1,1,2,3,5,8)
   * ------------------------------------ */
  // Using builtin functions
  def flattenList(list: List[Any]): List[Any] = {
    list flatMap {
      //flatMap takes a function and applies to each element of a list and flattens result list
      case ls: List[_] => flattenList(ls) // If case is a list (var name:type) then recursively flatten list
      case e => List(e) // If not a list then put into list (will be flattened by flatMap later)
    }
  }

  /* P08 Eliminate consecutive duplicates of list elements
   * If a list contains repeated elements they should be replaced with a single copy of the element.
   * The order of the elements should not be changed.
   * Ex: compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = List('a, 'b, 'c, 'd, 'e)
   * ------------------------------------ */
  def eliminateDuplicates[A](list: List[A]): List[A] = {
    list match {
      case Nil => Nil
      case h :: tail => h :: eliminateDuplicates(tail.dropWhile(_ == h)) //Take the head and append tail without duplicates (dropWhile drops elements up to certain condition)
    }
  }

  /* P09 Pack consecutive duplciates of list elements into sublists
   * Ex: pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a,)) = List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a,'a))
   * ------------------------------------ */
  def packConsecuDupes[A](list: List[A]): List[List[A]] = {
    if (list.isEmpty) List(List()) // Base checking case for if list empty
    else {
      val (packed, next) = list span {
        _ == list.head
      } // span combines takeWhile and dropWhile: xs span p equals (xs takeWhile p, xs dropWhile p)
      if (next == Nil) List(packed) // We've reached the end of the list (no duplicates are there)
      else packed :: packConsecuDupes(next) // Else continue to look for consecutive duplicates
    }
  }

  /* P10 Run-length encoding of a list.
   * Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive
   * duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
   * Ex: encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   * ------------------------------------ */
  def encode[A](list: List[A]): List[(Int, A)] = {
    packConsecuDupes(list) map {
      // Map takes a list and applies a function to each element of the list
      e => (e.length, e.head)
    }
  }

  /* P11 Modified run-length encoding
   * Modify the result of problem P10 in such a way that if an element has not duplicates it is simply copied into the result list.
   * Only elements with duplicates are transferred as (N, E) terms.
   * Ex: encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))
   * ------------------------------------ */
  def encodeModified[A](list: List[A]): List[Any] = {
    encode(list) map {
      e => if (e._1 == 1) e._2 // Take each element of the list (e) which will be (N, E) pairs and look at N by using e._1. If it is 1, take e._2 and put back in list or else keep e
      else e
    }
  }

  /* P12 Decode a run-length encoded list
   * Given a run-length code list generated as specified in problem P10, construct its uncompressed version
   * Ex: decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) = List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
   * ------------------------------------ */
  def decode[A](list: List[(Int, A)]): List[A] = {
    list flatMap {
      // Remember flatMap takes a function and applies to each element (e) of a list and flattens result list
      e => List.fill(e._1)(e._2) //List.fill(n)(y) Creates a list of n elements using value y. Thus, in our case (N,E) pairs in the list gets E expanded N times.
    }
  }

  /* P13 Run-length encoding of a list (direct solution)
   * Implement the so-called run-length encoding data compression method directly. I.e. don't use other methods you've written
   * (like P09's pack); do all work directly.
   * Ex: encodeDirect(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) = List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))
   * ------------------------------------ */
  def encodeDirect[A](list: List[A]): List[(Int, A)] = {
    if (list.isEmpty) Nil // Base checking case for if list empty
    else {
      val (packed, next) = list span {
        _ == list.head
      } // span combines takeWhile and dropWhile: xs span p equals (xs takeWhile p, xs dropWhile p)
      (packed.length, packed.head) :: encodeDirect(next) // Create (N, E) at the head
    }
  }

  /* P14 Duplicate the elements of a list
   * Ex: duplicate(List('a, 'b, 'c, 'c, 'd)) = List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd)
   * ------------------------------------ */
  def duplicateList[A](list: List[A]): List[A] = {
    list flatMap {
      e => List(e, e)
    }
  }

  /* P15 Duplicate the elements of a list a given number of times.
   * Ex: duplicateN(3, List('a, 'b, 'c, 'c, 'd)) = List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd)
   * ------------------------------------ */
  def duplicateListN[A](num: Int, list: List[A]): List[A] = {
    list flatMap {
      e => List.fill(num)(e)
    }
  }

  /* P16 Drop every Nth element from a list
   * Ex: drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)
   * ------------------------------------ */
  def dropRecursive[A](num: Int, list: List[A]): List[A] = {
    def dropRecursiveInner[A](decNum: Int, innerList: List[A]): List[A] = {
      (decNum, innerList) match {
        case (_, Nil) => Nil // If at the end of the list
        case (1, _ :: tail) => dropRecursiveInner(num, tail) // Decrement from Nth down to 1. When 1, then reset counter to original N and continue by skipping the current element.
        case (_, h :: tail) => h :: dropRecursiveInner(decNum - 1, tail) // If N is still being decremented, keep the head
      }
    }
    dropRecursiveInner(num, list)
  }

  // Tail recursion
  def dropTailRecursive[A](num: Int, list: List[A]): List[A] = {
    def dropTailRecursiveInner[A](decNum: Int, innerList: List[A], resultList: List[A]): List[A] = {
      (decNum, innerList) match {
        case (_, Nil) => resultList.reverse // Reverse the list because we prepend values onto the resultList for tail recursion so we need to flip it before returning as an answer
        case (1, _ :: tail) => dropTailRecursiveInner(num, tail, resultList)
        case (_, h :: tail) => dropTailRecursiveInner(decNum - 1, tail, h :: resultList) // Tail recursion by using resultList and prepending new values on
      }
    }
    dropTailRecursiveInner(num, list, Nil)
  }

  /* P17 Split a list into two parts
   * The length of the first part is given. Use a Tuple for your result.
   * Ex: split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
   * ------------------------------------ */
  // Using builtin functions
  def splitBuiltIn[A](num: Int, list: List[A]): (List[A], List[A]) = {
    return list.splitAt(num)
  }

  // Using recursion
  def splitRecursion[A](num: Int, list: List[A]): (List[A], List[A]) = {
    def splitRecursionInner[A](decNum: Int, curList: List[A], splitList: List[A]): (List[A], List[A]) = {
      (decNum, curList) match {
        case (_, Nil) => (splitList.reverse, Nil) // End of list, reverse the splitList (result) because we are prepending the head for Tail Recursion
        case (0, restList) => (splitList.reverse, restList) // When the num counter (Nth split) has been reached, we need to now create a list of the splitList and the rest of the list
        case (count, h :: tail) => splitRecursionInner(count - 1, tail, h :: splitList) // num counter has not been reached yet so keep going. Prepend head (h) to splitList to create first list subset
      }
    }
    splitRecursionInner(num, list, Nil)
  }

  /* P18 Extract a slice from a list
   * Given two indices, I and K, the slice is the list containing the elements from and including the Ith element up to but not including
   * the Kth element of the original list. Start counting the elements with 0.
   * Ex: slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = List('d, 'e, 'f, 'g)
   * ------------------------------------ */
  // Using builtin functions
  def sliceBuiltIn[A](ith: Int, kth: Int, list: List[A]): List[A] = {
    return list.slice(ith, kth)
  }

  // Using Tail recursion
  def sliceRecursion[A](ith: Int, kth: Int, list: List[A]): List[A] = {
    def sliceRecursionInner[A](count: Int, innerList: List[A], resultList: List[A]): List[A] = {
      (count, innerList) match {
        case (_, Nil) => resultList.reverse // Check for Nil for end of list. Reverse resultList to compensate for Tail Recursion's prepending of head values to result list
        case (c, h :: tail) if kth <= c => resultList.reverse // If we are hitting the end of the subset to be sliced (end <= counter)
        case (c, h :: tail) if ith <= c => sliceRecursionInner(c + 1, tail, h :: resultList) // If we are hitting the beginning of the subset to be sliced (start <= counter), begin creating the result list of sliced values
        case (c, _ :: tail) => sliceRecursionInner(c + 1, tail, resultList) // Else keep going
      }
    }
    sliceRecursionInner(0, list, Nil)
  }

  /* P19 Rotate a list N places to the left.
   * Ex: rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c)
   * Ex2: rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) = List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i)
   * ------------------------------------ */
  def rotateLeft[A](num: Int, list: List[A]): List[A] = {
    val nBounded = if (list.isEmpty) 0 else num % list.length //Check if list is empty and setup if we are rotating left or right. Positive = left, negative = right.
    if (nBounded < 0) rotateLeft(nBounded + list.length, list) // This is rotate right. To rotate right, we rotate left by nBounded + list.length to emulate rotating right n times.
    else (list drop nBounded) ::: (list take nBounded) // ::: = concatenate lists. List w/ first elements up to nBounded dropped ::: list with elements up to nBounded kept
  }

  /* P20 Remove the Kth element from a list
   * Ex: removeAt(1, List('a, 'b, 'c, 'd)) = (List('a, 'c, 'd),'b)
   * ------------------------------------ */
  def removeAt[A](kth: Int, list: List[A]): (List[A], A) = {
    list.splitAt(kth) match {
      case (Nil, _) if kth < 0 => throw new NoSuchElementException // If the first list is Nil and the kth index is less than 0, throw error
      case (pre, e :: post) => (pre ::: post, e) // List was successfully split at kth index. e represents the kth element. Take it out and return result format
      case (pre, Nil) => throw new NoSuchElementException // If kth element is past the end/length of the list, throw error
    }
  }

  /* P21 Insert an element at a given position into a list.
   * Ex: insertAt('new, 1, List('a, 'b, 'c, 'd)) = List('a, 'new, 'b, 'c, 'd)
   * ------------------------------------ */
  def insertAt[A](data: Any, index: Int, list: List[A]): List[Any] = {
    list.splitAt(index) match {
      case (Nil, _) if index < 0 => throw new NoSuchElementException // If the first list is Nil and the kth index is less than 0, throw error
      case (pre, Nil) => throw new NoSuchElementException // If index is past the end/length of the list, throw error
      case (pre, post) => (pre ::: (data :: post)) // List was successfully split and data is inserted at index
    }
  }

  /* P22 Create a list containing all integers within a given range
   * Ex: range(4, 9)
   * ------------------------------------ */
  def intRange[A](start: Int, end: Int): List[Int] = {
    def intRangeRecurs(endCount: Int, resultList: List[Int]): List[Int] = {
      if (endCount < start) resultList // If the end of the parameter range is less than the start of the parameter range then we are done and return the result.
      else intRangeRecurs(endCount - 1, endCount :: resultList) // Else continue and count down from end -> start. Remember this is tail recursion and we prepend here to the head so going backwards is better.
    }
    intRangeRecurs(end, Nil)
  }

  /* P23 Extract a given number of randomly selected elements from a list.
   * Ex: randomSelect(3, List('a, 'b, 'c, 'd, 'f, 'g, 'h)) = List[Symbol] = List('e, 'd, 'a)
   * ------------------------------------ */
  def randomExtract[A](num: Int, list: List[A]): List[A] = {
    def randomExtractRecurs[A](num: Int, list: List[A], rand: util.Random): List[A] = {
      if (num < 0) Nil //Check if num is negative
      else {
        val (rest, e) = removeAt(rand.nextInt(list.length), list) // Extract an element out of the list by using randomly generated index and Problem #20's created function removeAt
        e :: randomExtractRecurs(num - 1, rest, rand) // Creating list of extracted (num) elements. Pass rand to recursive call so that we don't have to create new rand object every time.
      }
    }
    randomExtractRecurs(num, list, new util.Random)
  }

  /* P24 Lotto: Draw N different random numbers from the set 1..M
   * Ex: lotto(6, 49) = List(23, 1, 17, 33, 21, 37)
   * ------------------------------------ */
  def lotto(num: Int, range: Int): List[Int] = {
    def lottoRecurs(num: Int, range: Int, rand: util.Random): List[Int] = {
      if (num < 0) Nil
      else {
        rand.nextInt(range) :: lottoRecurs(num - 1, range, rand) // Get random number from 0 to range and create list from it recursively
      }
    }
    lottoRecurs(num, range, new util.Random)
  }

  // HAVING PROBLEMS WITH THIS ONE!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!**********************
  /* P25 Generate a random permutation of the elements of a list
   * Hint: Use the solution of problem P23
   * Ex: randomPermute(List('a, 'b, 'c, 'd, 'e, 'f)) = List('b, 'a, 'd, 'c, 'e, 'f)
   * ------------------------------------ */
  def randomPermute[A](list: List[A]): List[A] = {
    randomExtract(list.length, list)
  }

  /* P26 Generate the combinations of K distinct objects chosen from the N elements of a list.
   * In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12, 3) = 220 possibilities (C(N, K)) denotes the well-known binomial coefficient).
   * For pure mathematicians, this result may be great. But we want to really generate all the possibilites.
   * Ex: combinations(3, List('a, 'b, 'c, 'd, 'e, 'f)) = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a, 'b, 'e), ...\
   * ------------------------------------ */
//  def combinations[A](num: Int, list: List[A]): List[Any] = {
//
//
//    /* P27 Group the elements of a set into disjoint subsets
//     * a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 person? Write a function that generates all the possibilities.
//     * Ex: group3(List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) = List(List(List(Aldo, Beat), List(Carla, David, Evi), List(Flip, Gary, Hugo, Ida)), ...
//     * b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
//     * Ex: group(List(2, 2, 5), List("Aldo", "Beat", "Carla", "David", "Evi", "Flip", "Gary", "Hugo", "Ida")) = List(List(List(Aldo, Beat), List(Carla, David), List(Evi, Flip, Gary, Hugo, Ida)), ...
//     * ------------------------------------ */
//
//    /* P28 Sorting a list of lists according to length of sublists
//     * a) We suppose that a list contains elements that are lists thenselves. The objective is to sort the elements of the list according to their length frequency; i.e. in the default, sorting is done ascendingly,
//     * lists with rare lengths are placed, others with a more frequence length come later.
//     * Ex: lsortFreq(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) = List(List('i, 'j, 'k, 'l), List('o), List('a, 'b, 'c), List('f, 'g, 'h), List('d, 'e), List('d, 'e), List('m, 'n))
//     * ------------------------------------ */
//
//    /* P31 Determine whether a given integer number is prime
//     * Ex: 7.isPrime = true
//     * ------------------------------------ */
//
//
//  }

}