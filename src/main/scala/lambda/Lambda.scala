package lambda

//the only members of a lambda expression
case class Lam(bod: LambdaExp) extends LambdaExp
case class Var(v: Int) extends LambdaExp {
  require(v >= 0)
}
case class App(f: LambdaExp, a: LambdaExp) extends LambdaExp
case class Hole(i: Int) extends LambdaExp

sealed trait LambdaExp {

  def leaves(prefix: Path.Path = Path.Here): Set[Path.Path] = this match {
    case Var(_)    => Set(prefix)
    case Hole(_)   => Set(prefix)
    case Lam(b)    => b.leaves(prefix).map(p => Path.Lam(p))
    case App(f, a) => f.leaves(prefix).map(p => Path.AppF(p)) ++ a.leaves(prefix).map(p => Path.AppA(p))
  }

  def replacePath(path: Path.Path, withThis: LambdaExp): LambdaExp = (path, this) match {
    case (Path.Here, _)               => withThis
    case (Path.Lam(rest), Lam(bod))   => Lam(bod.replacePath(rest, withThis))
    case (Path.AppF(rest), App(f, a)) => App(f.replacePath(rest, withThis), a)
    case (Path.AppA(rest), App(f, a)) => App(f, a.replacePath(rest, withThis))
    case _                            => { require(false, "incompatble path"); ??? }
  }

  def getPath(path: Path.Path): Option[LambdaExp] = (path, this) match {
    case (Path.Here, _)               => Some(this)
    case (Path.Lam(rest), Lam(bod))   => bod.getPath(rest)
    case (Path.AppF(rest), App(f, a)) => f.getPath(rest)
    case (Path.AppA(rest), App(f, a)) => a.getPath(rest)
    case _                            => None
  }

  def open: LambdaExp = {

    def opene(e: LambdaExp, depth: Int): LambdaExp = {
      require(depth >= 0)
      e match {
        case Var(i) if i >= depth => Var(i + 1) // "global"
        case Var(i) if i < depth  => Var(i) // "local"
        case App(f, a)            => App(opene(f, depth), opene(a, depth))
        case Lam(bod)             => Lam(opene(bod, depth + 1))
        case Hole(i)              => Hole(i)
      }
    }

    opene(this, 0)
  }

  def fill(withThis: LambdaExp, depth: Int = 0): LambdaExp = {
    require(depth >= 0)
    this match {
      case Var(i) if i == depth => withThis
      case Var(i) if i > depth  => Var(i - 1) // "global"
      case Var(i) if i < depth  => Var(i) // "local"
      case App(f, a)            => App(f.fill(withThis, depth), a.fill(withThis, depth))
      case Lam(bod)             => Lam(bod.fill(withThis.open, depth + 1))
      case Hole(i)              => Hole(i)
    }
  }

  def eval: LambdaExp =
    this match {
      case Var(i)            => Var(i)
      case App(Lam(fbod), a) => fbod.fill(a.eval)
      case App(Hole(i), a)   => Hole(i) // need this to conform to the requirement that everything matches up to holes
      case App(f, a)         => App(f.eval, a.eval)
      case Lam(bod)          => Lam(bod.eval)
      case Hole(i)           => Hole(i)
    }

  override def toString = {

    def fresh(ctx: List[String]): String = {
      var c = 'A'
      while (ctx.contains(c)) {
        c = (c.toInt + 1).toChar
      }
      c.toString()
    }

    def rec(e: LambdaExp, ctx: List[String]): String = e match {
      case Var(i)    => ctx(i)
      case Hole(i)   => s"$i?"
      case App(f, a) => s"(${rec(f, ctx)} ${rec(a, ctx)})"
      case Lam(bod) => {
        val v = fresh(ctx)
        s"λ$v. ${rec(bod, v :: ctx)}"
      }
    }

    rec(this, List())
  }

}
