package dungeongen.v2

import indigo.*
import indigo.syntax.*

import scala.annotation.tailrec

// Bowyer-Watson Delaunay Triangulation
object BowyerWatson:

  def triangulation(pointList: Batch[Vertex]): Mesh =
    val superTriangle = Triangle.encompassing(pointList)

    val initialMesh = Mesh.empty
      .addTriangle(superTriangle)

    val superTriEdges = initialMesh.edges

    // Add all the points one at a time to the triangulation
    @tailrec
    def rec(remaining: List[Vertex], acc: Mesh): Mesh =
      remaining match
        case Nil =>
          acc

        case p :: ps =>
          // first find all the triangles that are no longer valid due to the insertion
          val (badTris, _) = findInvalidTris(p, acc)

          // find the boundary of the polygonal hole
          val (internalEdges, boundaryEdges) = findBoundaryEdges(badTris, acc)

          // remove them from the data structure
          val meshWithHole = internalEdges.foldLeft(acc) { case (a, e) => a.removeEdge(e) }

          // re-triangulate the polygonal hole
          val nextMesh =
            boundaryEdges
              .flatMap { e =>
                edgeToTriangle(e, acc, p)
              }
              .foldLeft(meshWithHole) { case (m, t) => m.addTriangle(t) }
              .optimise

          rec(ps, nextMesh)

    val meshWithSuperTri = rec(pointList.toList, initialMesh)

    // if triangle contains a vertex from original super-triangle remove triangle from triangulation
    val result =
      superTriEdges
        .map(_._1)
        .foldLeft(meshWithSuperTri) { case (acc, i) =>
          acc.removeEdgeAt(i)
        }

    result

  // Left is 'bad' i.e. triangles inside the circle. Right is 'good', triangles we can leave in place.
  def findInvalidTris(point: Vertex, mesh: Mesh): (Batch[Tri], Batch[Tri]) =
    mesh.tris.map(_._2).partition { t =>
      Mesh.toTriangle(t, mesh).map(t => t.withinCircumcirle(point)).getOrElse(false)
    }

  // Left is 'bad' i.e. edges shared between the given triangles (occurs more than once).
  // Right is 'good', edges on the boundary.
  def findBoundaryEdges(triangles: Batch[Tri], mesh: Mesh): (Batch[Edge], Batch[Edge]) =
    val allEdges =
      triangles.flatMap { t =>
        mesh.edges.map(_._2).filter(e => t.indices.contains(e))
      }

    allEdges.partition { e =>
      if allEdges.filter(_ ~== e).length > 1 then true
      else false
    }

  def edgeToTriangle(edge: Edge, mesh: Mesh, point: Vertex): Batch[Triangle] =
    Batch.fromOption(
      Triangle.fromVertices(
        point :: mesh.vertices.filter(v => edge.indices.contains(v._1)).map(_._2)
      )
    )
