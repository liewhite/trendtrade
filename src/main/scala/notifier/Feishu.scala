package notifier

import sttp.client3.okhttp.quick._
import com.typesafe.scalalogging.Logger
import scala.concurrent.Future
import zio.json.*

case class FeishuMessageContent(
    text: String
) derives JsonEncoder
case class FeishuMessage(
    msg_type: String = "text",
    content:  FeishuMessageContent
) derives JsonEncoder

class FeishuNotify(webhook: String) extends Notify {
    val logger                           = Logger("feishu")
    def sendNotify(rawMsg: String): Unit = {
        try {
          val msg = FeishuMessage(content = FeishuMessageContent(rawMsg)).toJson
          val response = quickRequest
              .post(
                uri"${webhook}"
              )
              .body(msg)
              .send(backend)
          // logger.info(
          //   s"send notify ${msg}, result: ${response.code} ${response.body}"
          // )

        }catch {
          case e: Exception =>  {
            logger.info(
              s"failed send notify ${e}"
            )
            e.printStackTrace
          }
        }
    }
}
