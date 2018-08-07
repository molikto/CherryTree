package web

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

package object interop {


  @js.native
  @JSImport("commonmark", JSImport.Namespace, "CommonMark")
  object CommonMark extends js.Object  {

    @js.native
    class Parser(opt: js.Object) extends js.Object {

      def parse(str: String): Node = js.native

    }

    @js.native
    trait Node extends js.Object {
      def firstChild: Node = js.native
      def `type`: String = js.native
      def next: Node = js.native
      def destination: String = js.native
      def title: String = js.native
      def listType: String = js.native //, either Bullet or Ordered.
      def listTight: Boolean = js.native
      def literal: String = js.native
    }
  }

}
