import {Elm} from 'Main';
import Tone from 'Tone';

var analyser = new Tone.Analyser();

var synth = new Tone.Synth().connect(analyzer).toMaster();

var app = Elm.Main.init({node: document.getElementById('elm')});


app.ports.triggerAttack.subscribe(note => {
  console.log('note triggered', note)
  synth.triggerAttack(note);

  app.ports.analyze.send(analyser.getValue())
})

app.ports.triggerRelease.subscribe(() => {
  console.log('note released')
  synth.triggerRelease()
})
