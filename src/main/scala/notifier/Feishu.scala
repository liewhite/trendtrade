package notifier

import sttp.client3.okhttp.quick._
import com.typesafe.scalalogging.Logger
import io.github.liewhite.json.{*, given}
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class FeishuMessageContent(
    text: String
)
case class FeishuMessage(
    msg_type: String = "text",
    content:  FeishuMessageContent
)

class FeishuNotify(webhook: String) extends Notify {
    val logger                           = Logger("feishu")
    def sendNotify(rawMsg: String): Unit = {
        Future {
            val msg = FeishuMessage(content = FeishuMessageContent(rawMsg)).toJson

            val response = quickRequest
                .post(
                  uri"${webhook}"
                )
                .body(msg)
                .send(backend)
            logger.info(
              s"send notify ${msg}, result: ${response.code} ${response.body}"
            )

        }
    }
}
