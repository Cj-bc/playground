pub mod ReadTasks {
  pub eff ReadTasks {
    pub def readTasks(): List[Task]
  }

  pub def readDependencies(task: Task): List[Task] \ ReadTasks
    = {
      
    }

  pub def runWithTaskRepository(f: Unit -> a \ ef): a \ (ef - ReadTasks) + TaskRepository =
    run { f() } with handler ReadTasks {
      def readTasks
    }
}
