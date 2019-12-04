import ornicar.scalalib

package object poker
    extends scalalib.Validation
    with scalaz.syntax.ToValidationOps {

  type StackIndex = Int

}
