package shared.iv


trait ImView[PROPS, STATE] {

  def modify(s: STATE) = {

  }
}


class ClientInitializer {

  def onMount() = {
    // ..... async set state
  }

  def onUnmount() = {
    // cleanup state
  }

  def render(state: Option(Client)) {

  }
}