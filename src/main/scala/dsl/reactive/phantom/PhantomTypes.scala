package dsl.reactive.phantom

trait ReactiveEntity
trait DepHolder extends ReactiveEntity
trait AccessableDepHolder[+A] extends DepHolder
trait Dependent extends ReactiveEntity
trait Behavior[+A] extends Dependent with AccessableDepHolder[A]

abstract class ReactiveVar[A:Manifest] extends AccessableDepHolder[A]
abstract class Signal[+A:Manifest] extends Dependent with AccessableDepHolder[A]

abstract class Point(x: Int,y: Int)