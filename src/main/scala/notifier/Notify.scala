package notifier

trait Notify {
    def sendNotify(msg: String): Unit
}
