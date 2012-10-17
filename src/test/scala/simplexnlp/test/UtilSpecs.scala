package simplexnlp.test

import org.scalatest.{GivenWhenThen, FunSpec}
import org.scalatest.matchers.ShouldMatchers
import com.codahale.logula.Logging
import org.apache.log4j.Level
import simplexnlp.core.Util.Timer

/**
 * User: Tim Rocktaeschel
 * Date: 10/17/12
 * Time: 4:34 PM
 */

class UtilSpecs extends FunSpec with ShouldMatchers with GivenWhenThen with Logging {
  Logging.configure { log =>
    log.registerWithJMX = true
    log.level = Level.ALL
    log.console.enabled = true
    log.console.threshold = Level.ALL
  }

  describe("Timer") {
    it("stopping") {
      val timer = new Timer
      info("stop")
      Thread.sleep(100)
      assert(math.abs(timer.stop - 100) < 10)
      info("reset")
      timer.reset()
      Thread.sleep(300)
      assert(math.abs(timer.stop - 300) < 10)
      info("total")
      Thread.sleep(200)
      assert(math.abs(timer.total - 600) < 10)
    }
    it("reporting") {
      val timer = new Timer
      assert(timer.report(3*60*60*1000, "%1.0fh") === "3h")
      assert(timer.report(40*1000, "%1.2fs") === "40.00s")
      assert(timer.report(30*1000, "%1.2fm") === "0.50m")
      assert(timer.report(14*24*60*60*1000, "%1.0fw") === "2w")
    }
  }

}