package io.unsecurity.hlinx

trait ParamConverter[A] {
  def convert(s: String): Either[String, A]
}

object ParamConverter {
  import scala.util.Try

  implicit val intConverter: ParamConverter[Int] = (s: String) =>
    Try(s.toInt).fold(
      t => Left(t.toString),
      i => Right(i)
  )

  implicit val stringConverter: ParamConverter[String] =
    (s: String) => Right(s)
}
