package com.tkroman.kpi.y2022.l1

import scala.annotation.tailrec

enum Tree[+A]:
  case Branch(l: Tree[A], r: Tree[A])
  case Leaf(a: A)

enum MyList[+A]:
  case MyCons(h: A, t: MyList[A])
  case MyNil

  override def toString: String =
    @scala.annotation.tailrec
    def go(sb: StringBuilder, as: MyList[A]): String = {
      as match {
        case MyNil => sb.append(']').result
        case MyCons(h, MyNil) => sb.append(h).append(']').result
        case MyCons(h, t) => go(sb.append(h).append(", "), t)
      }
    }
    go(new StringBuilder("["), this)

object MyList:
  def empty[A]: MyList[A] = MyNil
  def apply[A](xs: A*): MyList[A] = of(xs*)
  def of[A](xs: A*): MyList[A] =
    xs.foldRight(MyNil: MyList[A]) { case (x, acc) => MyCons(x, acc) }

enum RecEntry[A]:
  case Flat(a: A)
  case Nested(as: MyList[RecEntry[A]])

import Tree.*
import MyList.*
import RecEntry.*

def traverse[A, B](xs: MyList[A])(f: A => Option[B]): Option[MyList[B]] =
  xs match
    case MyNil => Some(MyNil)
    case MyCons(h, t) =>
      (f(h), traverse(t)(f)) match
        case (Some(hd), Some(tl)) => Some(MyCons(hd, tl))
        case _ => None

def traverse[A, B](xs: Tree[A])(f: A => MyList[B]): MyList[Tree[B]] =
  def merge(xs: MyList[Tree[B]], ys: MyList[Tree[B]]): MyList[Tree[B]] =
    (xs, ys) match
      case (MyCons(h1, t1), MyCons(h2, t2)) => MyCons(Branch(h1, h2), merge(t1, t2))
      case (_, _) => MyNil
  xs match
    case Leaf(a) => map(f(a))(Leaf(_))
    case Branch(l, r) => merge(traverse(l)(f), traverse(r)(f))

def foldRight[A, B](xs: MyList[A], z: B)(f: (A, B) => B): B =
  xs match
    case MyNil => z
    case MyCons(h, t) => f(h, foldRight(t, z)(f))

def map[A, B](xs: MyList[A])(f: A => B): MyList[B] =
  foldRight(xs, MyNil: MyList[B])((a, b) => MyCons(f(a), b))

def flatten[A](xs: MyList[MyList[A]]): MyList[A] =
  foldRight(xs, MyNil: MyList[A])((a, b) => foldRight(a, b)(MyCons(_, _)))

def unnest[A](xs: MyList[RecEntry[A]]): MyList[A] =
  def go(r: RecEntry[A]): MyList[A] =
    r match
      case Flat(a) => MyCons(a, MyNil)
      case Nested(as) =>
        flatten(map(as)(go))
  go(Nested(xs))

@main def run() =
  println("Hello")
