"""Collect SPICE browser audio state and media events with Firefox WebDriver.

Requires ``selenium``, Firefox, geckodriver, and a running corvus-web gateway.
The guest must be producing audio during the probe.
"""

import argparse
import json
import time

from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.firefox.options import Options

INSTALL_EVENT_LOG = """
window.spiceAudioEvents = [];
const audio = document.querySelector('#spice-screen-' + arguments[0] + ' audio');
for (const name of [
  'play', 'playing', 'pause', 'seeking', 'seeked', 'timeupdate',
  'waiting', 'stalled', 'durationchange', 'canplay', 'ended', 'error'
]) {
  audio.addEventListener(name, () => {
    window.spiceAudioEvents.push({
      event: name,
      ms: Math.round(performance.now()),
      paused: audio.paused,
      time: String(audio.currentTime),
      duration: String(audio.duration),
      readyState: audio.readyState,
      bufferedEnd: audio.buffered.length
        ? audio.buffered.end(audio.buffered.length - 1) : null,
      error: audio.error?.message ?? null
    });
  });
}
"""

SAMPLE = """
const audio = document.querySelector('#spice-screen-' + arguments[0] + ' audio');
return {
  paused: audio.paused,
  ended: audio.ended,
  time: String(audio.currentTime),
  duration: String(audio.duration),
  readyState: audio.readyState,
  bufferedEnd: audio.buffered.length
    ? audio.buffered.end(audio.buffered.length - 1) : null,
  mediaSourceState: audio.spiceconn?.media_source?.readyState ?? null,
  sourceBufferUpdating: audio.spiceconn?.source_buffer?.updating ?? null,
  error: audio.error?.message ?? null,
  events: window.spiceAudioEvents?.splice(0) ?? []
};
"""


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("vm_id", type=int)
    parser.add_argument("--url", default="http://127.0.0.1:8080")
    parser.add_argument("--seconds", type=int, default=10)
    parser.add_argument("--firefox-binary")
    args = parser.parse_args()

    options = Options()
    options.add_argument("-headless")
    if args.firefox_binary:
        options.binary_location = args.firefox_binary
    # The button click grants playback; allow autoplay as well so the probe
    # isolates the media pipeline from Firefox's autoplay policy.
    options.set_preference("media.autoplay.default", 0)
    options.set_preference("media.autoplay.blocking_policy", 0)

    driver = webdriver.Firefox(options=options)
    try:
        driver.get(f"{args.url.rstrip('/')}/vms/{args.vm_id}/spice")
        selector = f"#spice-screen-{args.vm_id} audio"
        for _ in range(30):
            if driver.find_elements(By.CSS_SELECTOR, selector):
                break
            time.sleep(1)
        else:
            raise RuntimeError(
                "SPICE audio element did not appear; check the gateway and guest audio"
            )

        driver.execute_script(INSTALL_EVENT_LOG, args.vm_id)
        print(
            json.dumps({"stage": "before", **driver.execute_script(SAMPLE, args.vm_id)})
        )
        driver.find_element(By.XPATH, "//button[contains(., 'Enable sound')]").click()
        for second in range(1, args.seconds + 1):
            time.sleep(1)
            print(
                json.dumps(
                    {"second": second, **driver.execute_script(SAMPLE, args.vm_id)}
                )
            )
    finally:
        driver.quit()


if __name__ == "__main__":
    main()
