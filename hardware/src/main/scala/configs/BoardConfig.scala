package configs

case class BoardConfig(
                        ledWidth: Int,
                        btnWidth: Int,
                        switchWidth: Int,
                        seg7Width: Int,
                        anWidth: Int,
                        uart_baud_count: Int = 5  // must baud_count >=2
                      ) {
}

object BoardConfig {
  def selected = default

  private def default = BoardConfig(
    ledWidth = 24,
    btnWidth = 5,
    switchWidth = 24,
    seg7Width = 8,
    anWidth = 8
  )
}
