package dungeongen.v2

import indigo.Batch
import indigo.Vertex

class MeshTests extends munit.FunSuite {

  /*
  0-2-2
  |  /|
  0 1 4
  |/  |
  1-3-3
   */
  val quadMesh =
    Mesh(
      vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
      vertexNext = 4,
      edges =
        Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0), 3 -> Edge(1, 3), 4 -> Edge(3, 2)),
      edgeNext = 5,
      tris = Batch(0 -> Tri(0, 1, 2), 1 -> Tri(4, 3, 1)),
      triNext = 2
    )

  test("addVertex".only) {
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
  test("removeVertex".only) {
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
  test("removeVertexAt (3)".only) {
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
  test("removeVertexAt (0)".only) {
    val actual =
      quadMesh.removeVertexAt(0)

    val expected =
      Mesh(
        vertices = Batch(1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(1 -> Edge(1, 2), 3 -> Edge(1, 3), 4 -> Edge(3, 2)),
        edgeNext = 5,
        tris = Batch(1 -> Tri(4, 3, 1)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  test("addEdge".only) {
    val actual =
      Mesh.empty
        .addVertex(Vertex(0, 0))
        .addVertex(Vertex(1, 0))
        .addEdge(Edge(0, 1))

    val expected =
      Mesh.empty.copy(
        vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(1, 0)),
        vertexNext = 2,
        edges = Batch(0 -> Edge(0, 1)),
        edgeNext = 1
      )

    assertEquals(actual, expected)
  }

  test("addEdge - fail - missing vertex".only) {
    val actual =
      Mesh.empty
        .addVertex(Vertex(0, 0))
        .addVertex(Vertex(1, 0))
        .addEdge(Edge(0, 2))

    val expected =
      Mesh.empty.copy(
        vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(1, 0)),
        vertexNext = 2,
        edges = Batch(),
        edgeNext = 0
      )

    assertEquals(actual, expected)
  }

  /*
  0---2
  |  /|
  | / x <-- edge 4
  |/ x|
  1---3
   */
  test("removeEdge".only) {
    val actual =
      quadMesh.removeEdge(Edge(3, 2))

    val expected =
      Mesh(
        vertices =
          Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0), 3 -> Edge(1, 3)),
        edgeNext = 5,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  /*
  0---2
  |  /|
  | / x <-- edge 4
  |/ x|
  1---3
   */
  test("removeEdgeAt".only) {
    val actual =
      quadMesh.removeEdgeAt(4)

    val expected =
      Mesh(
        vertices =
          Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0), 3 -> Edge(1, 3)),
        edgeNext = 5,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  test("addTri".only) {
    val actual =
      Mesh.empty
        .addVertex(Vertex(0, 0))
        .addVertex(Vertex(1, 0))
        .addVertex(Vertex(0, 1))
        .addEdge(Edge(0, 1))
        .addEdge(Edge(1, 2))
        .addEdge(Edge(2, 0))
        .addTri(Tri(0, 1, 2))

    val expected =
      Mesh.empty.copy(
        vertices = Batch(0 -> Vertex(0, 0), 1 -> Vertex(1, 0), 2 -> Vertex(0, 1)),
        vertexNext = 3,
        edges = Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0)),
        edgeNext = 3,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 1
      )

    assertEquals(actual, expected)
  }

  /*
  0-2-2
  |0 /|
  0 1 4
  |/ 1|
  1-3-3
   */
  test("removeTri") {
    val actual =
      quadMesh.removeTri(Tri(4, 3, 1))

    val expected =
      Mesh(
        vertices =
          Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(
          0 -> Edge(0, 1),
          1 -> Edge(1, 2),
          2 -> Edge(2, 0),
          3 -> Edge(1, 3),
          4 -> Edge(3, 2)
        ),
        edgeNext = 5,
        tris = Batch(0 -> Tri(0, 1, 2)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  /*
  0-2-2
  |0 /|
  0 1 4
  |/ 1|
  1-3-3
   */
  test("removeTriAt".only) {
    val actual =
      quadMesh.removeTriAt(0)

    val expected =
      Mesh(
        vertices =
          Batch(0 -> Vertex(0, 0), 1 -> Vertex(0, 1), 2 -> Vertex(1, 0), 3 -> Vertex(1, 1)),
        vertexNext = 4,
        edges = Batch(
          0 -> Edge(0, 1),
          1 -> Edge(1, 2),
          2 -> Edge(2, 0),
          3 -> Edge(1, 3),
          4 -> Edge(3, 2)
        ),
        edgeNext = 5,
        tris = Batch(1 -> Tri(4, 3, 1)),
        triNext = 2
      )

    assertEquals(actual, expected)
  }

  test("addTriangle") {
    assert(1 == 2)
  }

  test("toTriangles") {
    assert(1 == 2)
  }

  test("offsetBy".only) {
    val actual =
      Mesh.offsetIndexesBy(10, 20, 30)(
        Mesh.empty
          .addVertex(Vertex(0, 0))
          .addVertex(Vertex(1, 0))
          .addVertex(Vertex(0, 1))
          .addEdge(Edge(0, 1))
          .addEdge(Edge(1, 2))
          .addEdge(Edge(2, 0))
          .addTri(Tri(0, 1, 2))
      )

    val expected =
      Mesh.empty.copy(
        vertices = Batch(10 -> Vertex(0, 0), 11 -> Vertex(1, 0), 12 -> Vertex(0, 1)),
        vertexNext = 13,
        edges = Batch(20 -> Edge(10, 11), 21 -> Edge(11, 12), 22 -> Edge(12, 10)),
        edgeNext = 23,
        tris = Batch(30 -> Tri(20, 21, 22)),
        triNext = 31
      )

    assertEquals(actual, expected)
  }

  test("combine / |+|".only) {
    val actual =
      Mesh.combine(
        Mesh.empty
          .addVertex(Vertex(0, 0))
          .addVertex(Vertex(1, 0))
          .addVertex(Vertex(0, 1))
          .addEdge(Edge(0, 1))
          .addEdge(Edge(1, 2))
          .addEdge(Edge(2, 0))
          .addTri(Tri(0, 1, 2)),
        Mesh.empty
          .addVertex(Vertex(1, 0))
          .addVertex(Vertex(0, 1))
          .addVertex(Vertex(1, 1))
          .addEdge(Edge(0, 1))
          .addEdge(Edge(1, 2))
          .addEdge(Edge(2, 0))
          .addTri(Tri(0, 1, 2))
      )

    val expected =
      Mesh.empty.copy(
        vertices = Batch(
          0 -> Vertex(0, 0),
          1 -> Vertex(1, 0),
          2 -> Vertex(0, 1),
          3 -> Vertex(1, 0),
          4 -> Vertex(0, 1),
          5 -> Vertex(1, 1)
        ),
        vertexNext = 6,
        edges = Batch(
          0 -> Edge(0, 1),
          1 -> Edge(1, 2),
          2 -> Edge(2, 0),
          3 -> Edge(3, 4),
          4 -> Edge(4, 5),
          5 -> Edge(5, 3)
        ),
        edgeNext = 6,
        tris = Batch(
          0 -> Tri(0, 1, 2),
          1 -> Tri(3, 4, 5)
        ),
        triNext = 2
      )

    assertEquals(actual, expected)
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
