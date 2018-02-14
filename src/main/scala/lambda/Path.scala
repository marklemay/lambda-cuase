package lambda

object Path {

  sealed trait Path {

    //appending
    def \(other: Path): Path

    def <(other: Path): Boolean

    def comparable(other: Path): Boolean

    def subPaths: Set[Path]
  }

  case object Here extends Path {

    def \(other: Path) = other

    def comparable(other: Path): Boolean = true

    def <(other: Path): Boolean = other match {
      case Here => false
      case _    => true
    }

    def subPaths: Set[Path] = Set(Here)
  }

  object Lam {
    def apply(): Lam = Lam(Here)
  }
  case class Lam(path: Path) extends Path {

    def \(other: Path): Path = Lam(path \ other)

    def comparable(other: Path): Boolean = other match {
      case Lam(rest) => path.comparable(rest)
      case Here      => true
      case _         => false
    }
    def <(other: Path): Boolean = other match {
      case Lam(rest) => path < rest
      case Here      => false
      case _         => { require(false, "uncomparable paths"); false }
    }

    def subPaths: Set[Path] = path.subPaths.map(Lam() \ _) + Here
  }

  object AppF {
    def apply(): AppF = AppF(Here)
  }
  case class AppF(path: Path) extends Path {

    def \(other: Path): Path = AppF(path \ other)

    def comparable(other: Path): Boolean = other match {
      case AppF(rest) => path.comparable(rest)
      case Here       => true
      case _          => false
    }
    def <(other: Path): Boolean = other match {
      case AppF(rest) => path < rest
      case Here       => false
      case _          => { require(false, "uncomparable paths"); false }
    }

    def subPaths: Set[Path] = path.subPaths.map(AppF() \ _) + Here
  }

  object AppA {
    def apply(): AppA = AppA(Here)
  }
  case class AppA(path: Path) extends Path {

    def \(other: Path): Path = AppA(path \ other)

    def comparable(other: Path): Boolean = other match {
      case AppA(rest) => path.comparable(rest)
      case Here       => true
      case _          => false
    }
    def <(other: Path): Boolean = other match {
      case AppA(rest) => path < rest
      case Here       => false
      case _          => { require(false, "uncomparable paths"); false }
    }

    def subPaths: Set[Path] = path.subPaths.map(AppA() \ _) + Here
  }

}