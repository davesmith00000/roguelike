package dungeongen.v2

import indigo.Batch
import indigo.Vertex

class MeshTests extends munit.FunSuite {

  /*
  0---2
  |  /|
  | / |
  |/  |
  1---3
   */
  val quadMesh =
    Mesh(
      vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
      vertexNext = 4,
      edges =
        Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0), 3 -> Edge(1, 3), 4 -> Edge(3, 2)),
      edgeNext = 5,
      tris = Batch(0 -> Tri(0, 1, 2), 1 -> Tri(4, 5, 1)),
      triNext = 2
    )

  test("addVertex") {
    val actual =
      Mesh.empty.addVertex(Vertex(1, 0))

    val expected =
      Mesh.empty.copy(
        vertices = Batch(0 -> Vertex(1, 0)),
        vertexNext = 1
      )

    assertEquals(actual, expected)
  }

  /*
  0---2
  |  /
  | /
  |/
  1
   */
  test("removeVertex") {
    val actual =
      quadMesh.removeVertex(Vertex(1, 1))

    val expected =
      Mesh(
        vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0)),
        vertexNext = 4,
        edges = Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0)),
        edgeNext = 5,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  /*
  0---2
  |  /
  | /
  |/
  1
   */
  test("removeVertexAt (3)") {
    val actual =
      quadMesh.removeVertexAt(3)

    val expected =
      Mesh(
        vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0)),
        vertexNext = 4,
        edges = Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0)),
        edgeNext = 5,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  /*
      2
     /|
    / |
   /  |
  1---3
   */
  test("removeVertexAt (0)") {
    val actual =
      quadMesh.removeVertexAt(0)

    val expected =
      Mesh(
        vertices = Batch(1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(1 -> Edge(1, 2), 3 -> Edge(1, 3), 4 -> Edge(3, 2)),
        edgeNext = 5,
        tris = Batch(1 -> Tri(4, 5, 1)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  test("addEdge") {
    assert(1 == 2)
  }

  test("addEdge - fail - missing vertex") {
    assert(1 == 2)
  }

  test("removeEdge") {
    assert(1 == 2)
  }

  test("removeEdgeAt") {
    assert(1 == 2)
  }

  test("addTri") {
    assert(1 == 2)
  }

  test("removeTri") {
    assert(1 == 2)
  }

  test("removeTriAt") {
    assert(1 == 2)
  }

  test("addTriangle") {
    assert(1 == 2)
  }

  test("toTriangles") {
    assert(1 == 2)
  }

  test("combine / |+|") {
    assert(1 == 2)
  }

  test("prune") {
    assert(1 == 2)
  }

  test("weld") {
    assert(1 == 2)
  }

  test("fromTriangle") {
    assert(1 == 2)
  }

  test("fromTriangles") {
    assert(1 == 2)
  }

}
