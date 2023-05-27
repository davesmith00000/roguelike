package roguelike.util

import indigo.shared.datatypes.Point

class IndicesTests extends munit.FunSuite {

  test("convert point to index") {
    assertEquals(Indices.pointToIndex(Point(1, 1), 3), 4)
  }

  test("convert index to Point") {
    assertEquals(Indices.indexToPoint(4, 3), Point(1, 1))
  }

  test("convert index to point and back again") {
    val w = 14
    val i = 31
    val p = Indices.indexToPoint(i, w)
    val j = Indices.pointToIndex(p, w)

    assertEquals(i, j)
  }

}
