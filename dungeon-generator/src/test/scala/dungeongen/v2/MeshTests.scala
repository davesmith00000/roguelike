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
      vertices = Batch(Vertex(0, 0), Vertex(0, 1), Vertex(1, 0), Vertex(1, 1)),
      edges = Batch(Edge(0, 1), Edge(1, 2), Edge(2, 0), Edge(1, 3), Edge(3, 2)),
      tris = Batch(Tri(0, 1, 2), Tri(4, 5, 1))
    )

  test("addVertex") {
    val actual =
      Mesh.empty.addVertex(Vertex(1, 0))

    val expected =
      Mesh(
        vertices = Batch(Vertex(1, 0)),
        edges = Batch(),
        tris = Batch()
      )

    assertEquals(actual, expected)
  }

  test("removeVertex") {
    assert(1 == 2)
  }

  test("removeVertexAt (3)") {
    val actual =
      quadMesh.removeVertexAt(3)

    val expected =
      Mesh(
        vertices = Batch(Vertex(0, 0), Vertex(0, 1), Vertex(1, 0)),
        edges = Batch(Edge(0, 1), Edge(1, 2), Edge(2, 0)),
        tris = Batch(Tri(0, 1, 2))
      )

    assertEquals(actual, expected)
  }

  /* New mesh
      1
     /|
    / |
   /  |
  0---2
   */
  test("removeVertexAt (0)".only) {
    val actual =
      quadMesh.removeVertexAt(0)

    val expected =
      Mesh(
        vertices = Batch(Vertex(0, 1), Vertex(1, 0), Vertex(1, 1)),
        edges = Batch(Edge(0, 1), Edge(0, 2), Edge(2, 1)),
        tris = Batch(Tri(0, 1, 2))
      )

    assertEquals(actual, expected)
  }

  test("addEdge") {
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
