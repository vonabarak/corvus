import { Constants } from "@/lib/spice/enums.js";
import { SpicePlaybackConn } from "@/lib/spice/playback.js";

type PlaybackMessage = { type: number; data: ArrayBuffer };
type PlaybackConnection = InstanceType<typeof SpicePlaybackConn>;

// Firefox can mark a live WebM/Opus MediaSource as ended when every 10 ms
// SPICE packet is appended separately. Giving spice-html5 a short batch of
// packets lets its existing queue combine them into larger SourceBuffer writes.
const BATCH_MS = 200;
const pending = new WeakMap<PlaybackConnection, { messages: PlaybackMessage[]; timer: number }>();
let installed = false;

export function installBatchedSpiceAudio() {
  if (installed) return;
  installed = true;

  const original = SpicePlaybackConn.prototype.process_channel_message;

  function flush(conn: PlaybackConnection) {
    const batch = pending.get(conn);
    if (!batch) return;
    window.clearTimeout(batch.timer);
    pending.delete(conn);
    for (const message of batch.messages) original.call(conn, message);
  }

  SpicePlaybackConn.prototype.process_channel_message = function (message: PlaybackMessage) {
    if (message.type !== Constants.SPICE_MSG_PLAYBACK_DATA) {
      flush(this);
      return original.call(this, message);
    }

    const batch = pending.get(this);
    if (batch) {
      batch.messages.push(message);
    } else {
      pending.set(this, {
        messages: [message],
        timer: window.setTimeout(() => flush(this), BATCH_MS),
      });
    }
    return true;
  };
}
