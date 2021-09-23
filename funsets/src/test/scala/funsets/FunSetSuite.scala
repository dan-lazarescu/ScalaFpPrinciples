package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton s1")
      assert(!contains(s1, 2), "Singleton s1 negative")
      assert(contains(s2, 2), "Singleton s2")
      assert(!contains(s2, 1), "Singleton s2 negative")
      assert(contains(s3, 3), "Singleton s3")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersect contains all common elements from both sets") {
    new TestSets:
      val s = union(s1, s2)
      val p = intersect(s, s1)
      val r = intersect(s, s2)
      assert(contains(p, 1), "Intersection between s & s1")
      assert(!contains(p, 2), "Intersection between s & s1 - negative")
      assert(contains(r, 2), "Intersection between s & s2")
      assert(!contains(r, 1), "Intersection between s & s2 - negative")
  }

  test("difference contains all elements from set1 that are not in set 2") {
    new TestSets:
      val s = union(s1, s2)
      val p = diff(s, s1)
      val r = diff(s, s2)
      assert(contains(p, 2), "Difference between s & s1")
      assert(!contains(p, 1), "Difference between s & s1 - negative")
      assert(contains(r, 1), "Difference between s & s2")
      assert(!contains(r, 2), "Difference between s & s2 - negative")
  }

  test("filter contains all elements from set that are accepted by a predicate") {
    new TestSets:
      val s = union(union(s1, s2), s3)
      val p = filter(s, s1)
      val r = filter(s, x => x > 1)
//      assert(contains(p, 2), "Difference between s & s1")
//      assert(!contains(p, 1), "Difference between s & s1 - negative")
      assert(!contains(r, 1), "After filter elements <=1 from s - verify 1")
      assert(contains(r, 2), "After filter elements <=1 from s - verify 2")
      assert(contains(r, 3), "After filter elements <=1 from s - verify 3")
  }

  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
