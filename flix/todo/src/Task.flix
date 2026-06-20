pun mod Task {

  pub enum Task({
    title = String,
    description = Option[String],
    status = StatusSelf,
    dependsOn = Option[Nel[Task]]
  })

  pub enum StatusSelf {
    case NotPlanned
    case Later
    case Scheduled(DateTime)
    case Done
  }

  pub enum StatusPropagated {
    case NotPlanned
    case PartiallyPlanned
    case Scheduled
    case BlockedBy(Nel[Task])
    case PartiallyDone
    case Dome
  }

}
