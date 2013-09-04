package dsl.reactive.phantom

/** These types are kept abstract and serve only to guide LMS
  * when checking for subtype relations
  */

trait ReactiveEntity
trait DepHolder extends ReactiveEntity
trait AccessableDepHolder[+A] extends DepHolder
trait Dependent extends ReactiveEntity
trait Behavior[+A] extends Dependent with AccessableDepHolder[A]

abstract class ReactiveVar[A:Manifest] extends AccessableDepHolder[A]
abstract class Signal[+A:Manifest] extends Dependent with AccessableDepHolder[A]

abstract class Point(x: Int,y: Int)