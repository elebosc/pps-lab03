package it.unibo.pps.task2

object People:
  
  enum Person:
    case Student(name: String, year: Int)
    case Teacher(name: String, course: String)

  object Person:

    def name(p: Person): String = p match
      case Student (n, _) => n
      case Teacher (n, _) => n
    
    def course(p: Person): String = p match
      case Teacher(_, c) => c

    def isTeacher(p: Person): Boolean = p match
      case Teacher(_, _) => true
      case _ => false
