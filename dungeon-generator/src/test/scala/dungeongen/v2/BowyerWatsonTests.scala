// package dungeongen.v2

// import indigo.*

// class BowyerWatsonTests extends munit.FunSuite:

//   test("one point") {

//     val superTriangle = Triangle(Vertex(0, 10), Vertex(10, 10), Vertex(5, 0))
//     val pt            = Vertex(5, 5)

//     val actual =
//       BowyerWatson.triangulation(Batch(pt), superTriangle)

//     val expected =
//       Mesh.empty
//         .addTriangle(superTriangle)
//         .addTriangle(Triangle(Vertex(0, 10), Vertex(10, 10), pt))
//         .addTriangle(Triangle(Vertex(10, 10), Vertex(5, 0), pt))
//         .addTriangle(Triangle(Vertex(5, 0), Vertex(0, 10), pt))
//         .optimise

//     assertEquals(actual, expected)

//   }

//   test("two points".only) {

//     println("----> " +  BoundingCircle.fromThreeVertices(Vertex(5,5),Vertex(10,10),Vertex(5,0)))
//     println("----> " +  BoundingCircle.fromThreeVertices(Vertex(5,5),Vertex(10,10),Vertex(5,0)).map(_.contains(Vertex(6, 4))))

//     val superTriangle = Triangle(Vertex(0, 10), Vertex(10, 10), Vertex(5, 0))
//     val pts           = Batch(Vertex(5, 5), Vertex(6, 4))

//     val actual =
//       BowyerWatson.triangulation(pts, superTriangle)

//     val expected =
//       Mesh.empty
//         .addTriangle(superTriangle)
//       // .addTriangle(Triangle(Vertex(0, 10), Vertex(10, 10), pt))
//       // .addTriangle(Triangle(Vertex(10, 10), Vertex(5, 0), pt))
//       // .addTriangle(Triangle(Vertex(5, 0), Vertex(0, 10), pt))
//       // .optimise

//     assertEquals(actual, expected)

//   }

//   test("three points") {

//     val superTriangle = Triangle(Vertex(0, 10), Vertex(10, 10), Vertex(5, 0))
//     val pts           = Batch(Vertex(5, 5), Vertex(6, 4), Vertex(8, 9))

//     val actual =
//       BowyerWatson.triangulation(pts, superTriangle)

//     val expected =
//       Mesh.empty
//         .addTriangle(superTriangle)
//       // .addTriangle(Triangle(Vertex(0, 10), Vertex(10, 10), pt))
//       // .addTriangle(Triangle(Vertex(10, 10), Vertex(5, 0), pt))
//       // .addTriangle(Triangle(Vertex(5, 0), Vertex(0, 10), pt))
//       // .optimise

//     assertEquals(actual, expected)

//   }
