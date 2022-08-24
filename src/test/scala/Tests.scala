package com.tkroman.kpi.y2022.l1

import munit.FunSuite
import MyList.*
import Tree.*
import RecEntry.*

class Tests extends FunSuite {
  test("traverse empty list") {
    val expected = Some(MyNil)
    val actual = traverse(MyNil)(Some(_))
    assertEquals(actual, expected)
  }
  test("traverse list with None on all") {
    val expected = None
    val actual = traverse(MyList(5, 6, 8, 7, 13))(_ => None)
    assertEquals(actual, expected)
  }
  test("traverse list with None on 8") {
    val expected = None
    val actual = traverse(MyList(5, 6, 8, 7, 13))(x => if x == 8 then None else Some(x))
    assertEquals(actual, expected)
  }
  test("traverse list with None on negatives") {
    val expected = Some(MyList(5, 6, 8, 7, 13))
    val actual = traverse(MyList(5, 6, 8, 7, 13))(x => if x < 0 then None else Some(x))
    assertEquals(actual, expected)
  }
  test("traverse list with Some(_)") {
    val expected = Some(MyList(5, 6, 8, 7, 13))
    val actual = traverse(MyList(5, 6, 8, 7, 13))(Some(_))
    assertEquals(actual, expected)
  }
  test("traverse tree with _ => MyNil") {
    val expected = MyNil
    val actual = traverse(
      Branch(Branch(Leaf(14), Branch(Leaf(2), Leaf(-15))), Branch(Leaf(3), Leaf(-25)))
    )(_ => MyNil)
    assertEquals(actual, expected)
  }
  test("traverse Leaf") {
    val expected = MyList(Leaf(-8), Leaf(8), Leaf(13))
    val actual = traverse(Leaf(8))(x => MyList(-x, x, x + 5))
    assertEquals(actual, expected)
  }
  test("traverse tree") {
    val expected = MyList(
      Branch(Branch(Leaf(-14), Branch(Leaf(-2), Leaf(15))), Branch(Leaf(-3), Leaf(25))),
      Branch(Branch(Leaf(14), Branch(Leaf(2), Leaf(-15))), Branch(Leaf(3), Leaf(-25))),
      Branch(Branch(Leaf(19), Branch(Leaf(7), Leaf(-10))), Branch(Leaf(8), Leaf(-20)))
    )
    val actual = traverse(
      Branch(Branch(Leaf(14), Branch(Leaf(2), Leaf(-15))), Branch(Leaf(3), Leaf(-25)))
    )(x => MyList(-x, x, x + 5))
    assertEquals(actual, expected)
  }
  test("flatten nothing") {
    val expected = MyNil
    val actual = flatten(MyNil)
    assertEquals(actual, expected)
  }
  test("flatten empty list") {
    val expected = MyNil
    val actual = flatten(MyList(MyNil))
    assertEquals(actual, expected)
  }
  test("flatten one list") {
    val expected = MyList(82, 38, 23)
    val actual = flatten(MyList(MyList(82, 38, 23)))
    assertEquals(actual, expected)
  }
  test("flatten list with empty lists") {
    val expected = MyList(82, 38, 23)
    val actual = flatten(MyList(MyNil, MyList(82, 38, 23), MyNil))
    assertEquals(actual, expected)
  }
  test("flatten several lists") {
    val expected = MyList(93, -28, 32, 12, -12, 35, 76, -95, 80)
    val actual = flatten(MyList(MyList(93, -28, 32), MyList(12, -12, 35), MyList(76, -95, 80)))
    assertEquals(actual, expected)
  }
  test("unnest empty list") {
    val expected = MyNil
    val actual = unnest(MyNil)
    assertEquals(actual, expected)
  }
  test("unnest flats") {
    val expected = MyList(-47, -41, -9, -49, 91)
    val actual = unnest(MyList(Flat(-47), Flat(-41), Flat(-9), Flat(-49), Flat(91)))
    assertEquals(actual, expected)
  }
  test("unnest nested") {
    val expected = MyList(22, 43, 67, 4, 51)
    val actual = unnest(MyList(Flat(22), Nested(MyList(Flat(43), Flat(67), Flat(4))), Flat(51)))
    assertEquals(actual, expected)
  }
}
