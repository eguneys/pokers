import ornicar.scalalib

package object poker
    extends scalalib.Validation
    with scalaz.syntax.std.ToBooleanOps
    with scalaz.syntax.ToValidationOps {

  type StackIndex = Int

  type HandValueMagic = Int

}
