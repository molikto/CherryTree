package utils

import java.text.SimpleDateFormat

object InitPlatform {


  private val simpleDateFormat = new SimpleDateFormat("MMM dd, yyyy, HH:mm")
  platform.formatDate = (a: Long) => {
    simpleDateFormat.format(new java.util.Date(a))
  }
}
