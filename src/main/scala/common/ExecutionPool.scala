package common

import scala.concurrent.ExecutionContext
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit
import java.util.concurrent.LinkedBlockingDeque

object ExecutionPool {
    given e: ExecutionContext = ExecutionContext.fromExecutor(
      new ThreadPoolExecutor(
        0,
        60,
        60L,
        TimeUnit.SECONDS,
        new LinkedBlockingDeque[Runnable]()
      )
    )
}