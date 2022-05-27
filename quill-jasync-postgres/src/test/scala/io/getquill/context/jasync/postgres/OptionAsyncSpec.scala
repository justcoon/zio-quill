package io.getquill.context.jasync.postgres

import io.getquill.Spec
import scala.concurrent.ExecutionContext.Implicits.global

class OptionAsyncSpec extends Spec {
  val ctx = testContext
  import ctx._

  override protected def beforeAll(): Unit = {
    await(ctx.run(qr1.delete))
    ()
  }

  "select by option" in {
    val o1: Option[Int] = Some(1)
    val e1 = TestEntity("1", 1, 0, o1, true)

    await(ctx.run(qr1.insertValue(lift(e1))))
    await(ctx.run(qr1.filter(_.o.exists(_ == 1)))) mustEqual Seq(e1)
    await(ctx.run(qr1.filter(_.o == lift(o1)))) mustEqual Seq(e1)
  }

}
