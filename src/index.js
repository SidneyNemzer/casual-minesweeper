import { Elm } from './Main.elm'

const div = document.createElement('div')
document.body.appendChild(div)

Elm.Main.init({
  node: div,
  flags: { seed: Math.ceil(Math.random() * 100000) }
})
