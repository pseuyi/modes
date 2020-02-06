import {Elm} from 'Main';
import Tone from 'Tone';

var synth = new Tone.Synth().toMaster();

var app = Elm.Main.init({node: document.getElementById('elm')});

/*
app.ports.synth.subscribe(note => {
  console.log('note triggered', note)
  synth.triggerAttackRelease(note, '4n');
});
*/

app.ports.triggerAttack.subscribe(note => {
  console.log('note triggered', note);
  synth.triggerAttack(note);
});

app.ports.triggerRelease.subscribe(() => {
  console.log('note released');
  synth.triggerRelease();
});

navigator.requestMIDIAccess().then(function(access) {
  // lists of available MIDI controllers converted from iterables to arrays
  const inputs = Array.from(access.inputs.values());
  const outputs = Array.from(access.outputs.values());

  // find the controller device in inputs
  const firstController = inputs[0];

  console.log('first: ', firstController);

  // listen to events from that controller with .onmidimessage
  firstController.onmidimessage = MIDIMessageEventHandler;

  access.onstatechange = function(e) {
    console.log(e.port.name, e.port.manufacturer, e.port.state);

    //if(firstController.connection === "open") {
    // tell elm that a controller has connected
    app.ports.connectMIDI.send(firstController.name);
    //}
  };
});

function MIDIMessageEventHandler(e) {
  console.log(e);
  //parse the midi event into something to send to elm

  const [command, note, velocity] = e.data;

  synth.triggerAttackRelease(note, '4n');

  if (command === 144 && velocity > 0) {
    // note on

    console.log('note on');
    // directly triggering tonejs
    // synth.triggerAttackRelease(note, '4n');

    // send note number to elm
  } else if (command === 128 || velocity === 0) {
    // note off

    console.log('note off');
    //send note number to elm
  }
}
