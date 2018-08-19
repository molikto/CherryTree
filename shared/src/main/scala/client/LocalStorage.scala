package client

trait LocalStorage {
  def set(key: String, str: String)
  def remove(key: String)
  def get(key: String): Option[String]
}
