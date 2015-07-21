package com.dafttech.workspace

import com.dafttech.logic.Signal

/**
 * Created by LolHens on 21.07.2015.
 */
class Workspace {
  val on = Signal(true)
  println(on.value)
  println((!on).value)
}
