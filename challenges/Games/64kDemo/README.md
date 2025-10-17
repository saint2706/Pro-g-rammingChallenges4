# 64k Procedural Demo

This miniature WebGL production renders a procedurally generated visual synced with a lightweight procedural synthesizer. The build pipeline compresses the demo so the shipped payload fits under the 64&nbsp;KB milestone once gzipped.

## Runtime

* Modern desktop or mobile browser with WebGL 1.0 and Web Audio API support.
* Pointer or keyboard interaction to unlock audio playback (tap/click or press any key).

## Controls

* **Click / tap / key press**: start the audio stream. Once running, the visuals remain synchronized automatically.

## Building

```bash
cd challenges/Games/64kDemo
python build.py
```

The build script performs the following:

1. Inlines `src/main.js` into `src/index.html` to reduce HTTP overhead.
2. Applies a whitespace-only minifier to HTML/JS to avoid external dependencies while staying compatible with the tiny codebase.
3. Writes the packed document to `dist/index.html` and a gzipped version to `dist/index.html.gz`.
4. Prints the byte counts so you can confirm the gzipped payload remains within the 64&nbsp;KB requirement.

On the reference environment the gzipped bundle occupies well below the cap, leaving space for further experimentation.

## Size tactics

* **Minimal shaders / audio synth**: the GLSL fragment shader combines simple noise, sine bands, and radial modulation. The synth uses a compact ScriptProcessor node with analytic envelopes; no samples are stored.
* **Single file delivery**: building collapses the demo to one HTML file with inline script, letting gzip exploit redundancy between shader, audio, and JavaScript symbols.
* **Aggressive compiler settings**: when hosting natively, serve `dist/index.html.gz` with `Content-Encoding: gzip` to preserve the measured size. No additional libraries are used.

