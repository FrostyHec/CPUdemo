package configs

case class BoardConfig(
                        ledWidth: Int,
                        btnWidth: Int,
                        switchWidth: Int,
                      ) {
}

object BoardConfig {
  def selected = default

  private def default = BoardConfig(
    ledWidth = 24,
    btnWidth = 5,
    switchWidth = 24,
  )
}
