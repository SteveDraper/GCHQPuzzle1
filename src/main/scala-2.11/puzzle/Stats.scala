package puzzle

import java.util.concurrent.atomic.AtomicInteger

/**
  * Created by sdraper on 12/27/15.
  */
object Stats {
  private val logToStdout = false
  private val mostDetermined: AtomicInteger = new AtomicInteger(0)
  private val branchCount: AtomicInteger = new AtomicInteger(0)
  private var startTime: Long = _
  private var endTime: Long = _

  def noteStart = { startTime = System.currentTimeMillis() }

  def noteEnd = { endTime = System.currentTimeMillis() }

  def noteDetermnined(determined: Int) = {
    while ( determined > mostDetermined.get ) {
      val currMax = mostDetermined.get
      mostDetermined.compareAndSet(currMax, determined)
      if ( logToStdout )
        println(s"New max determined cells count: $determined")
    }
  }

  def getMostDetermined = mostDetermined.get

  def noteBranch = branchCount.incrementAndGet()

  def getBranchCount = branchCount.get

  def render(renderToHtml: Boolean) = {
    val statsBody = s"Determined $getMostDetermined cells in ${endTime-startTime}mS, with ${getBranchCount} unconstrained branches"

    if ( renderToHtml ) s"<br/>\n<br/>\n$statsBody"
    else statsBody
  }
}
