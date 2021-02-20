import scala.util.Random

object pattern_matching {

  def main(args: Array[String]): Unit = {

    val x: Int = Random.nextInt(10)

    x match {
      case 0 => println("Zero")
      case 1 => println("One")
      case 2 => println("Two")
      case _ => println("Other")
    }

    matchTest(3)
    matchTest(1)

    val someSms = SMS("12345", "Are you there")
    val someVoiceRecording = VoiceRecording("Tome", "voicerecording.org/id/123")

    println(showNotification(someSms))
    println(showNotification(someVoiceRecording))

    val importantPeople = Seq("867-5309", "jenny@gmail.com")

    val someOne = SMS("123-4567", "Are you there?")
    val someVoiceRecording1 = VoiceRecording("Tom", "voicerecording.org/id/123")
    val importantEmail = Email("jenny@gmail.com", "Drinks tonight?", "I'm free after 5!")
    val someSMS = SMS("867-5309", "I'm here! Where are you?")

    println(showImportanNotification(someOne, importantPeople))
    println(showImportanNotification(someVoiceRecording1, importantPeople))
    println(showImportanNotification(importantEmail, importantPeople))
    println(showImportanNotification(someSMS, importantPeople))

    var computer = Computer

    goIdle(computer)
  }

  def matchTest(x: Int) = x match {
    case 1 => println("one")
    case 2 => println("two")
    case _ => println("other")
  }

  def showNotification(notification: Notification) = {
    notification match {
      case Email(sender, title, _) => s"You got an email from $sender with title: $title"
      case SMS(number, message) => s"You got an SMS from $number! Message: $message"
      case VoiceRecording(contactName, link) => s"You received a voice recording from $contactName! Click the link to hear it: $link"
    }
  }

  def showImportanNotification(notification: Notification, important: Seq[String]) = {
    notification match {
      case Email(sender, _, _) if important.contains(sender) => "You got an email from special message"
      case SMS(caller, _) if important.contains(caller) => "you got an SMS from special someone"
      case other => showNotification(other)
    }
  }

  def goIdle(device: Device) = device match {
    case p: Phone => p.screenOff
    case c: Computer => c.screenOn
  }

}

abstract class Notification

case class Email(sender: String, title: String, body: String) extends Notification

case class SMS(caller: String, message: String) extends Notification

case class VoiceRecording(contactName: String, link: String) extends Notification


abstract class Device

case class Phone(model: String) extends Device {
  def screenOff = "Turning screen off"
}

case class Computer(model: String) extends Device {
  def screenOn = "Turning screen saver on..."
}

