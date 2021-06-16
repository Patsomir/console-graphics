package math

import scala.math.{ sin, cos, tan, toRadians }
import graphics.Primitive
import graphics.Vertex
import graphics.Point
import graphics.Line
import graphics.Triangle

object Transformations {
    import MetricVectorSpace.ops._

    implicit class LinearTransformation(matrix: Matrix4x4) {
        def apply(vec: Vector3): Vector3 = transform(matrix)(vec)
        def apply(vec: Vector4): Vector4 = matrix * vec
    }

    def perspectiveMatrix(viewAngle: Float, aspectRatio: Float, nearPlane: Float, farPlane: Float): Matrix4x4 = {
        val fov = 1.0f / tan(toRadians(viewAngle) / 2).toFloat
        Matrix4x4.fromCols(
            Vector4(fov / aspectRatio, 0, 0, 0),
            Vector4(0, fov, 0, 0),
            Vector4(0, 0, (farPlane + nearPlane) / (nearPlane - farPlane), -1),
            Vector4(0, 0, 2.0f * nearPlane * farPlane / (nearPlane - farPlane), 0)
        )
    }

    def viewMatrix(eye: Vector3, focus: Vector3, up: Vector3): Matrix4x4 = {
        val lookingDirection = (focus to eye).normalize
        val upVector = (up vectorProduct lookingDirection).normalize
        val sideVector = (lookingDirection vectorProduct upVector).normalize

        Matrix4x4.fromCols(
            Vector4(upVector.x, sideVector.x, lookingDirection.x, 0),
            Vector4(upVector.y, sideVector.y, lookingDirection.y, 0),
            Vector4(upVector.z, sideVector.z, lookingDirection.z, 0),
            Vector4(-(upVector dot eye), -(sideVector dot eye), -(lookingDirection dot eye), 1)
        )
    }

    def transform(matrix: Matrix4x4)(vec: Vector3): Vector3 = {
        val Vector4(x, y, z, t) = matrix * Vector4(vec.x, vec.y, vec.z, 1)
        Vector3(x / t, y / t, z / t)
    }

    def applyMatrix(matrix: Matrix4x4)(primitive: Primitive): Primitive = primitive match {
        case Vertex(point, color) => Vertex(transform(matrix)(point), color)
        case Line(start, end, color) => Line(transform(matrix)(start), transform(matrix)(end), color)
        case Triangle(a, b, c, color) => Triangle(transform(matrix)(a), transform(matrix)(b), transform(matrix)(c), color)
    }

    def translationMatrix(vec: Vector3): Matrix4x4 = Matrix4x4.fromCols(
        Vector4(1, 0, 0, 0),
        Vector4(0, 1, 0, 0),
        Vector4(0, 0, 1, 0),
        Vector4(vec.x, vec.y, vec.z, 1)
    )

    def scalingMatrix(xScaler: Float, yScaler: Float, zScaler: Float): Matrix4x4 = Matrix4x4.fromCols(
        Vector4(xScaler, 0, 0, 0),
        Vector4(0, yScaler, 0, 0),
        Vector4(0, 0, zScaler, 0),
        Vector4(0, 0, 0, 1)
    )

    def xRotationMatrix(angle: Float): Matrix4x4 = {
        val rads = toRadians(angle)
        val sinAngle = sin(rads).toFloat
        val cosAngle = cos(rads).toFloat
        Matrix4x4.fromCols(
            Vector4(1, 0, 0, 0),
            Vector4(0, cosAngle, sinAngle, 0),
            Vector4(0, -sinAngle, cosAngle, 0),
            Vector4(0, 0, 0, 1)
        )
    }

    def yRotationMatrix(angle: Float): Matrix4x4 = {
        val rads = toRadians(angle)
        val sinAngle = sin(rads).toFloat
        val cosAngle = cos(rads).toFloat
        Matrix4x4.fromCols(
            Vector4(cosAngle, 0, sinAngle, 0),
            Vector4(0, 1, 0, 0),
            Vector4(-sinAngle, 0, cosAngle, 0),
            Vector4(0, 0, 0, 1)
        )
    }

    def zRotationMatrix(angle: Float): Matrix4x4 = {
        val rads = toRadians(angle)
        val sinAngle = sin(rads).toFloat
        val cosAngle = cos(rads).toFloat
        Matrix4x4.fromCols(
            Vector4(cosAngle, sinAngle, 0, 0),
            Vector4(-sinAngle, cosAngle, 0, 0),
            Vector4(0, 0, 1, 0),
            Vector4(0, 0, 0, 1)
        )
    }
}