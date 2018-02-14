package lambda

object Cuase {

  def directCuase(f: LambdaExp => LambdaExp)(in: LambdaExp)(outPath: Path.Path): Set[Path.Path] = {

    val allPaths = in.leaves().flatMap(_.subPaths)

    // find which parts of the input put a hole in the "right place"
    val cuases = for (
      path <- allPaths;
      inReplace = in.replacePath(path, Hole(1));
      out = f(inReplace);
      exp <- out.getPath(outPath);
      if exp.isInstanceOf[Hole] // exactly if there is a hole in the specified position
    ) yield path

    for (
      path <- cuases;
      if cuases.forall(other => if (other.comparable(path)) {
        path == other || other < path //only return the specific cuases
      } else { true })
    ) yield path
  }

  //more imperative  faster way
  // eat input in from the leaves
  //  keep irrelivent removals
  //  find the boundry such that any increase matters, and any decrease doesn't

  //  TODO: yeah perhaps the prov guys know some relivent literature

}