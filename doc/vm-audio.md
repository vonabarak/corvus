# VM audio

Each VM can have multiple virtual sound cards. Each card has a
speaker and a microphone. The audio backend determines where playback goes
and where microphone samples come from.

| Backend | Playback | Microphone |
|---|---|---|
| `pulse` | PulseAudio server on the node or reachable over the network | PulseAudio source |
| `pipewire` | PipeWire server on the node | PipeWire source |
| `spice` | SPICE client | Native SPICE client microphone |

Use `crv audio-device add VM BACKEND [--model MODEL] [--options KEY=VALUE,...]` to add a card.
The VM must be stopped. `crv audio-device list VM` shows its cards; `edit` and
`remove` take the numeric audio device ID. For example:

```sh
crv audio-device add workstation spice
crv audio-device add workstation pulse --options 'server=192.0.2.10,out.name=speakers,in.name=mic'
crv audio-device add workstation pipewire --options 'out.name=speakers,in.name=mic'
crv audio-device edit workstation 2 pulse --options 'server=192.0.2.10'
crv audio-device edit workstation 2 pulse --model ich9-intel-hda
crv audio-device remove workstation 2
```

Models are `virtio-sound` (default), `intel-hda`, `ich9-intel-hda`, and
`AC97`. Both HDA controllers use the `hda-micro` codec. The guest needs a
driver for the selected model. An edit without `--model` keeps the current
model.

The `options` string is a comma-separated list of QEMU `-audiodev`
`key=value` properties. It is passed through to QEMU after the backend and
generated ID. `id` and `driver` are reserved. Backend-specific properties
must be supported by the installed QEMU build. The nodeagent process must
have access to the local PulseAudio or PipeWire server and the selected
source and sink. A remote PulseAudio server must permit the node to connect.

SPICE audio needs a graphical VM. `crv vm view VM` launches the native SPICE
client for playback and microphone recording. The browser SPICE console
supports playback but does not currently forward microphone input.

Template YAML and apply VM definitions accept `audioDevices` as a list of
`{backend, model, options}` objects; `model` defaults to `virtio-sound`.
A template instance receives its own audio
device rows. The Python client offers `add_audio_device`,
`edit_audio_device`, `remove_audio_device`, and `list_audio_devices` on VM
objects, plus an `audio_devices` argument to VM creation.
