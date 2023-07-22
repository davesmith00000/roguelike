package dungeongen.v2

import indigo.*
import indigo.syntax.*

final case class Mesh(vertices: Batch[Vertex], edges: Batch[Edge], tris: Batch[Tri]):

  /** Adds a vertex */
  def addVertex(vertex: Vertex): Mesh =
    this.copy(vertices = vertices :+ vertex)

  /** Removes any vertices that match, and any edges, and then any tris connected to them. */
  def removeVertex(vertex: Vertex): Mesh =
    val delete = vertices.zipWithIndex.collect { case (v, i) if v ~== vertex => i }
    delete.foldLeft(this) { case (acc, i) => acc.removeVertexAt(i) }

  /** Removes the vertex at the specified index, and any edges and tris attached to it. */
  def removeVertexAt(index: Int): Mesh =
    if index >= 0 && index < vertices.length then
      val (v1, v2) = vertices.splitAt(index)
      val (badEdges, goodEdges) = edges.zipWithIndex.partition { case (e, _) =>
        e.vertexA == index || e.vertexB == index
      }
      val badEdgeIndices = badEdges.map(_._2)
      val t = tris.filterNot(t =>
        badEdgeIndices.contains(t.edgeA) ||
          badEdgeIndices.contains(t.edgeB) ||
          badEdgeIndices.contains(t.edgeC)
      )

      this.copy(
        vertices = v1 ++ v2.drop(1),
        edges = goodEdges.map(_._1),
        tris = t
      )
    else this

  /** Adds an edge using existing vertices. Checks the vertices are present, but does not ensure
    * they are valid.
    */
  def addEdge(edge: Edge): Mesh =
    if edge.vertexA >= 0 && edge.vertexA < vertices.length &&
      edge.vertexB >= 0 && edge.vertexB < vertices.length
    then this.copy(edges = edges :+ edge)
    else this

  /** Removes any edges that match, and any tris connected to them. */
  def removeEdge(edge: Edge): Mesh =
    val delete = edges.zipWithIndex.collect { case (e, i) if e == edge => i }
    delete.foldLeft(this) { case (acc, i) => acc.removeEdgeAt(i) }

  /** Removes the edge at the specified index, and any tris attached to it. */
  def removeEdgeAt(index: Int): Mesh =
    if index >= 0 && index < edges.length then
      val (e1, e2) = edges.splitAt(index)
      val t        = tris.filterNot(t => t.edgeA == index || t.edgeB == index || t.edgeC == index)

      this.copy(
        edges = e1.dropRight(1) ++ e2,
        tris = t
      )
    else this

  /** Adds a triangle using existing edges. Checks the edges are present, but does not ensure they
    * are valid.
    */
  def addTri(tri: Tri): Mesh =
    if tri.edgeA >= 0 && tri.edgeA < edges.length &&
      tri.edgeB >= 0 && tri.edgeB < edges.length &&
      tri.edgeC >= 0 && tri.edgeC < edges.length
    then this.copy(tris = tris :+ tri)
    else this

  /** Removes any tris that match. */
  def removeTri(tri: Tri): Mesh =
    this.copy(tris = tris.filterNot(_ == tri))

  /** Removes the tri at the specified index. */
  def removeTriAt(index: Int): Mesh =
    if index >= 0 && index < tris.length then
      val (t1, t2) = tris.splitAt(index)

      this.copy(
        tris = t1.dropRight(1) ++ t2
      )
    else this

  // def addTriangle(triangle: Triangle): Triangulation =
  //   val indexed = vertices.zipWithIndex

  //   val toUse =
  //     Batch(
  //       (triangle.a, indexed.find(p => p._1 ~== triangle.a)),
  //       (triangle.b, indexed.find(p => p._1 ~== triangle.b)),
  //       (triangle.c, indexed.find(p => p._1 ~== triangle.c))
  //     )

  //   @tailrec
  //   def rec(
  //       remaining: List[(Vertex, Option[(Vertex, Int)])],
  //       accV: List[Vertex],
  //       accI: List[Int]
  //   ): Option[(List[Vertex], Tri)] =
  //     remaining match
  //       case Nil =>
  //         Tri.fromIndices(accI).map(is => (accV, is))

  //       case (v, None) :: next =>
  //         rec(next, accV :+ v, accI :+ (vertices.length + accV.length + 1))

  //       case (v, Some((_, i))) :: next =>
  //         rec(next, accV, accI :+ i)

  //   val tri: Option[(List[Vertex], Tri)] = rec(toUse.toList, Nil, Nil)

  //   tri match
  //     case None =>
  //       this

  //     case Some((newVerts, tri)) =>
  //       this.copy(
  //         vertices = vertices ++ newVerts.toBatch,
  //         triangles = triangles :+ tri
  //       )

  def toTriangles: Option[Batch[Triangle]] =
    try
      tris.map { t =>
        val vSet = t.indices.map(i => edges(i)).flatMap(_.indices).toSet
        if vSet.size == 3 then Triangle.fromVertices(vSet.toList.map(i => vertices(i)))
        else None
      }.sequence
    catch {
      case _ => None
    }

  def |+|(other: Mesh): Mesh =
    Mesh.combine(this, other)

  def prune: Mesh =
    Mesh.prune(this)

  def weld: Mesh =
    Mesh.weld(this)

object Mesh:

  def empty: Mesh =
    Mesh(Batch.empty, Batch.empty, Batch.empty)

  def fromTriangles(triangles: Batch[Triangle]): Mesh =
    triangles.map(fromTriangle).foldLeft(Mesh.empty)(_ |+| _)

  def fromTriangle(triangle: Triangle): Mesh =
    Mesh(
      triangle.vertices,
      Batch(Edge(0, 1), Edge(1, 2), Edge(2, 0)),
      Batch(Tri(0, 1, 2))
    )

  def combine(a: Mesh, b: Mesh): Mesh =
    // TODO: This is wrong, need to change the index offsets otherwise you get 0,1,2,0,1,2 instead of 0,1,2,3,4,5.
    Mesh(
      a.vertices ++ b.vertices,
      a.edges ++ b.edges,
      a.tris ++ b.tris
    )

  /** Removes edges and vertices that are not part of a Tri. */
  def prune(m: Mesh): Mesh =  
    m

  def weld(m: Mesh): Mesh =  
    m

final case class Edge(vertexA: Int, vertexB: Int):
  def indices: Batch[Int] =
    Batch(vertexA, vertexB)

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
