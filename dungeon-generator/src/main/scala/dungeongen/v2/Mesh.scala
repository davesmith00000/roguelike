package dungeongen.v2

import indigo.*
import indigo.syntax.*

import scala.annotation.tailrec

/** Naive implementation of a data structure for storing and modifying tri[angle]-based mesh data.
  */
final case class Mesh(
    vertices: Batch[(Int, Vertex)],
    vertexNext: Int,
    edges: Batch[(Int, Edge)],
    edgeNext: Int,
    tris: Batch[(Int, Tri)],
    triNext: Int
):

  /** Adds a vertex */
  def addVertex(vertex: Vertex): Mesh =
    this.copy(
      vertices = vertices :+ (vertexNext -> vertex),
      vertexNext = vertexNext + 1
    )

  /** Removes any vertices that match, and any edges, and then any tris connected to them. */
  def removeVertex(vertex: Vertex): Mesh =
    vertices.filter(_._2 ~== vertex).foldLeft(this) { case (acc, (i, _)) => acc.removeVertexAt(i) }

  /** Removes the vertex with the specified index, and any edges and tris attached to it. */
  def removeVertexAt(index: Int): Mesh =
    val keep = vertices.filterNot(p => p._1 == index)
    val (badEdges, goodEdges) = edges.partition { case (_, e) =>
      e.vertexA == index || e.vertexB == index
    }
    val badEdgeIndices = badEdges.map(_._1)
    val t = tris.filterNot(t =>
      badEdgeIndices.contains(t._2.edgeA) ||
        badEdgeIndices.contains(t._2.edgeB) ||
        badEdgeIndices.contains(t._2.edgeC)
    )

    this.copy(
      vertices = keep,
      edges = goodEdges,
      tris = t
    )

  /** Adds an edge using existing vertices. Checks the vertices are present, but does not ensure
    * they are valid.
    */
  def addEdge(edge: Edge): Mesh =
    if vertices.exists(p => p._1 == edge.vertexA) &&
      vertices.exists(p => p._1 == edge.vertexB)
    then
      this.copy(
        edges = edges :+ edgeNext -> edge,
        edgeNext = edgeNext + 1
      )
    else this

  /** Removes any edges that match, and any tris using them. Does not remove any other/connecting
    * edges.
    */
  def removeEdge(edge: Edge): Mesh =
    edges.filter(_._2 == edge).foldLeft(this) { case (acc, (i, _)) => acc.removeEdgeAt(i) }

  /** Removes the edge at the specified index, and any tris using it. Does not remove any
    * other/connecting edges.
    */
  def removeEdgeAt(index: Int): Mesh =
    this.copy(
      edges = edges.filterNot(e => e._1 == index),
      tris = tris.filterNot(t => t._2.edgeA == index || t._2.edgeB == index || t._2.edgeC == index)
    )

  /** Adds a triangle using existing edges. Checks the edges are present, but does not ensure they
    * are valid.
    */
  def addTri(tri: Tri): Mesh =
    if edges.exists(p => p._1 == tri.edgeA) &&
      edges.exists(p => p._1 == tri.edgeB) &&
      edges.exists(p => p._1 == tri.edgeC)
    then
      this.copy(
        tris = tris :+ triNext -> tri,
        triNext = triNext + 1
      )
    else this

  /** Removes any tris that match, leaves edges and vertices intact. */
  def removeTri(tri: Tri): Mesh =
    this.copy(tris = tris.filterNot(_._2 == tri))

  /** Removes the tri at the specified index, leaves edges and vertices intact. */
  def removeTriAt(index: Int): Mesh =
    this.copy(tris = tris.filterNot(_._1 == index))

  /** Adds a Triangle's data to this mesh. Does _not_ attempt to optimise the mesh, this can be done
    * intentionally by calling the `weld`, `prune`, or `optimise` methods.
    */
  def addTriangle(triangle: Triangle): Mesh =
    this |+| Mesh.fromTriangle(triangle)

  /** Returns the mesh as individual Triangles. The assumption is made that the edges do form a
    * valid trangle, no validation occurs. If the triangle is degenerate, you will get unexpected
    * results.
    */
  def toTriangles: Batch[Triangle] =
    tris.flatMap { case (_, t) =>
      val vSet = t.indices.map(i => edges(i)).flatMap(_._2.indices).toSet
      if vSet.size == 3 then
        Batch.fromOption(Triangle.fromVertices(vSet.toList.map(i => vertices(i)._2)))
      else Batch.empty
    }

  def toLineSegments: Batch[LineSegment] =
    edges.map { case (_, e) =>
      LineSegment(vertices(e.vertexA)._2, vertices(e.vertexB)._2)
    }

  /** Combines two meshes together, offsetting the indices of the second mesh by the first. Does not
    * attempt to optimise the meshes in any way.
    */
  def |+|(other: Mesh): Mesh =
    Mesh.combine(this, other)

  /** Prune to remove any vertices and edges that are not part of a Tri. */
  def prune: Mesh =
    // First remove any edges not associate with a tri
    val es = edges.filter(e =>
      tris.exists(t => t._2.edgeA == e._1 || t._2.edgeB == e._1 || t._2.edgeC == e._1)
    )

    // Then remove any vertices not associated with an edge
    val vs = vertices.filter(v => es.exists(e => e._2.vertexA == v._1 || e._2.vertexB == v._1))

    this.copy(
      vertices = vs,
      edges = es
    )

  /** Weld matching Vertices and Edges, and remove the duplicates. */
  def weld: Mesh =
    import Batch.==:

    // First weld the vertices, and update the edges as you go.
    @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
    @tailrec
    def weldVerts(
        remaining: Batch[(Int, Vertex)],
        edgesAcc: Batch[(Int, Edge)],
        vertsAcc: Batch[(Int, Vertex)]
    ): (Batch[(Int, Vertex)], Batch[(Int, Edge)]) =
      remaining match
        case Batch() =>
          (vertsAcc, edgesAcc)

        case v ==: vs =>
          vertsAcc.find(_._2 ~== v._2) match
            case None =>
              // If the 'found so far' doesn't include this vertex, add it and move on.
              weldVerts(vs, edgesAcc, vertsAcc :+ v)

            case Some((replacementIndex, _)) =>
              // This vertex needs to be welded with one of the ones accumulated already.
              // This is the same as forgetting the vertex, and pointing the matching one.

              val updatedEdges =
                edgesAcc.map { case (idx, e @ Edge(a, b)) =>
                  idx -> Edge(
                    if a == v._1 then replacementIndex else a,
                    if b == v._1 then replacementIndex else b
                  )
                }

              weldVerts(vs, updatedEdges, vertsAcc)

        case _ =>
          // Not a real case, Batch pattern matching isn't great.
          throw new Exception(
            "Batch pattern matching failed unexpectedly during Mesh.weld of vertices. Shouldn't have got here."
          )

    val (vs, es) = weldVerts(vertices, edges, Batch.empty)

    // Then weld the edges, and update the tris as you go
    @SuppressWarnings(Array("scalafix:DisableSyntax.throw"))
    @tailrec
    def weldEdges(
        remaining: Batch[(Int, Edge)],
        trisAcc: Batch[(Int, Tri)],
        edgesAcc: Batch[(Int, Edge)]
    ): (Batch[(Int, Edge)], Batch[(Int, Tri)]) =
      remaining match
        case Batch() =>
          (edgesAcc, trisAcc)

        case e ==: es =>
          edgesAcc.find(_._2 ~== e._2) match
            case None =>
              // If the 'found so far' doesn't include this edge, add it and move on.
              weldEdges(es, trisAcc, edgesAcc :+ e)

            case Some((replacementIndex, _)) =>
              // This edge needs to be welded with one of the ones accumulated already.
              // This is the same as forgetting the edge, and pointing the matching one.
              val updatedTries =
                trisAcc.map { case (idx, t @ Tri(a, b, c)) =>
                  idx -> Tri(
                    if a == e._1 then replacementIndex else a,
                    if b == e._1 then replacementIndex else b,
                    if c == e._1 then replacementIndex else c
                  )
                }

              weldEdges(es, updatedTries, edgesAcc)

        case _ =>
          // Not a real case, Batch pattern matching isn't great.
          throw new Exception(
            "Batch pattern matching failed unexpectedly during Mesh.weld of edges. Shouldn't have got here."
          )

    val (es2, ts) = weldEdges(es, tris, Batch.empty)

    this.copy(
      vertices = vs,
      edges = es2,
      tris = ts
    )

  /** Weld matching Vertices and Edges, and then prune to remove any vertices and edges that are not
    * part of a Tri.
    */
  def optimise: Mesh =
    weld.prune

  // TODO?
  // def transformVertex? (translate / rotate / scale)
  // def transformMesh? (translate / rotate / scale)

object Mesh:

  /** Provides an empty Mesh */
  def empty: Mesh =
    Mesh(Batch.empty, 0, Batch.empty, 0, Batch.empty, 0)

  def fromTriangles(triangles: Batch[Triangle]): Mesh =
    triangles.map(fromTriangle).foldLeft(Mesh.empty)(_ |+| _).optimise

  def fromTriangle(triangle: Triangle): Mesh =
    Mesh(
      triangle.vertices.zipWithIndex.map(p => (p._2, p._1)),
      3,
      Batch(0 -> Edge(0, 1), 1 -> Edge(1, 2), 2 -> Edge(2, 0)),
      3,
      Batch(0 -> Tri(0, 1, 2)),
      1
    )

  /** Shifts the indices of the stored data by the given offsets. */
  private[v2] def offsetIndexesBy(vertexOffset: Int, edgeOffset: Int, triOffset: Int)(
      mesh: Mesh
  ): Mesh =
    Mesh(
      vertices = mesh.vertices.map(p => (p._1 + vertexOffset, p._2)),
      vertexNext = mesh.vertexNext + vertexOffset,
      edges = mesh.edges.map { p =>
        (p._1 + edgeOffset, Edge(p._2.vertexA + vertexOffset, p._2.vertexB + vertexOffset))
      },
      edgeNext = mesh.edgeNext + edgeOffset,
      tris = mesh.tris.map { p =>
        (
          p._1 + triOffset,
          Tri(p._2.edgeA + edgeOffset, p._2.edgeB + edgeOffset, p._2.edgeC + edgeOffset)
        )
      },
      triNext = mesh.triNext + triOffset
    )

  /** Combines two meshes together, offsetting the indices of the second mesh by the first. Does not
    * attempt to optimise the meshes in any way.
    */
  def combine(a: Mesh, b: Mesh): Mesh =
    val bb = offsetIndexesBy(a.vertexNext, a.edgeNext, a.triNext)(b)
    Mesh(
      vertices = a.vertices ++ bb.vertices,
      vertexNext = bb.vertexNext,
      edges = a.edges ++ bb.edges,
      edgeNext = bb.edgeNext,
      tris = a.tris ++ bb.tris,
      triNext = bb.triNext
    )

final case class Edge(vertexA: Int, vertexB: Int):
  def indices: Batch[Int] =
    Batch(vertexA, vertexB)

  /** Approx. equals of an edge means that if you reverse one of the edges, they end up being the
    * same.
    */
  def ~==(other: Edge): Boolean =
    (this.vertexA == other.vertexA && this.vertexB == other.vertexB) ||
      (this.vertexA == other.vertexB && this.vertexB == other.vertexA)

object Edge:

  def fromIndices(indices: List[Int]): Option[Edge] =
    if indices.length == 2 then Option(Edge(indices(0), indices(1)))
    else None

  def fromIndices(indices: Int*): Option[Edge] =
    fromIndices(indices.toList)

final case class Tri(edgeA: Int, edgeB: Int, edgeC: Int):
  def indices: Batch[Int] =
    Batch(edgeA, edgeB, edgeC)

object Tri:

  def fromIndices(indices: List[Int]): Option[Tri] =
    if indices.length == 3 then Option(Tri(indices(0), indices(1), indices(2)))
    else None

  def fromIndices(indices: Int*): Option[Tri] =
    fromIndices(indices.toList)

final case class Triangle(a: Vertex, b: Vertex, c: Vertex):
  val vertices: Batch[Vertex] = Batch(a, b, c)
  val closed: Batch[Vertex]   = Batch(a, b, c, a)

object Triangle:

  def fromVertices(vertices: List[Vertex]): Option[Triangle] =
    if vertices.length == 3 then Option(Triangle(vertices(0), vertices(1), vertices(2)))
    else None

  def fromVertices(vertices: Vertex*): Option[Triangle] =
    fromVertices(vertices.toList)

  // A triangle big enough to comfortably contain a point cloud
  // Some fudged numbers here.
  def encompassing(pointCloud: Batch[Vertex]): Triangle =
    encompassing(BoundingBox.fromVertexCloud(pointCloud).expand(20))

  def encompassing(b: BoundingBox): Triangle =
    val t = Vertex(b.center.x, b.center.y - b.height)
    val l = Vertex(b.center.x - b.width, b.bottom)
    val r = Vertex(b.center.x + b.width, b.bottom)

    Triangle(t, l, r)
