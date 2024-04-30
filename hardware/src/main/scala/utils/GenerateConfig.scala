package utils


case class GenerateConfig(
                         debugMode: Boolean,
                         useIPMemory: Boolean
                         )

object GenerateConfig {
  def default = GenerateConfig(
    debugMode = false,
    useIPMemory = false
  )
}