# Tron Lightcycle Arena

> Challenge #130 — Multiplayer Tron clone with AI opponents and arena variants.

The Tron Lightcycle Arena recreates the classic lightcycle duel using vanilla JavaScript and the HTML5 canvas. It ships with
local multiplayer (keyboard + gamepad), adaptive AI riders, configurable arena sizes, boost pickups, scoring, and an optional
peer-to-peer mode powered by WebRTC data channels.

![Tron arena screenshot](screenshot.png)

## Features

- **Grid-locked duels** – deterministic movement on configurable grids (32×24 up to 60×44) with instant trail collisions.
- **Arena variants** – Classic, Fusion Ring, Sector Maze, and Quad Pillars layouts introduce different choke points.
- **Local multiplayer** – Up to four pilots with independent control schemes (WASD, Arrow keys, IJKL, Numpad, or gamepads).
- **AI opponents** – Heuristic bots evaluate accessible area, wall distance, and boosts to avoid traps and hunt rivals.
- **Boost system** – Neon pickups grant short speed bursts or shields, animated with glow effects and decay timers.
- **Score tracking** – First to the configured number of round wins claims the match; draws split points among survivors.
- **Experimental network mode** – Manual WebRTC handshake so one remote player can join a host-controlled arena.

## Running the game

1. Open `index.html` in any modern browser **or** start a static server from the `challenges/Games/Tron/` folder:
   ```bash
   cd challenges/Games/Tron
   python -m http.server 8000
   ```
   Visit <http://localhost:8000> to play.

2. Configure the match in the left panel:
   - Select arena variant, grid size, and pace (Tactical/Standard/Hyper).
   - Choose how many round wins are needed for victory.
   - For each pilot, pick a role:
     - **Human** – local keyboard/gamepad control.
     - **AI** – autonomous opponent.
     - **Remote** – reserved for the WebRTC pilot.
     - **Inactive** – remove the slot.

3. Assign controllers. Humans can mix keyboard layouts or detected gamepads; the panel updates when pads connect/disconnect.

4. Press **Launch Match**. Use **Next Round** to queue another round or **Reset** to restore defaults.

## Controls

| Player | Default Keys | Alternatives |
|--------|--------------|--------------|
| P1     | `WASD`       | Any other keyboard layout or Gamepad 1 |
| P2     | Arrow keys   | `IJKL`, `8456` (numpad) or Gamepad 2 |
| P3     | AI by default | Switch to human for a third local rider |
| P4     | Inactive by default | Enable for four-way battles or remote pilot |

Gamepads use either the d-pad or left stick. Direction changes are rejected if they reverse the current heading in the same
frame (classic lightcycle behaviour).

## AI behaviour

AI riders evaluate every legal move each tick with a weighted score:
- Breadth-first flood fill approximates open space to avoid self-trapping.
- Proximity bonuses encourage contesting nearby opponents.
- Distance-from-wall heuristics keep bikes away from outer boundaries.
- Boost squares add a large bonus so bots chase power-ups when safe.

Speed boosts trigger an extra hop per tick, while shields allow the bot to phase through one obstacle hit before dissipating.

## Network play (experimental)

The WebRTC data channel mode is a stretch-goal feature to let one remote rider join a host. It requires manual signalling:

1. **Host setup**
   - Set one player to **Remote** and switch the network role to **Host**.
   - Click **Create host offer**. Share the base64 blob with the remote player (chat, pastebin, etc.).
   - After receiving the answer blob from the remote pilot, paste it and press **Accept remote answer**.

2. **Remote pilot**
   - Switch the network role to **Join a remote arena**.
   - Paste the host's offer and click **Submit host offer**. Share the generated answer back to the host.
   - Pick a control scheme (keyboard or gamepad). The host will authorise input for the remote-designated lightcycle.

> Tips:
> - Keep both browsers focused during the handshake so ICE candidates gather successfully (default Google STUN server).
> - The host simulates the match and streams state updates; latency is acceptable on LAN/VPN but expect drift on slow links.

If the connection drops, the host can re-open the details panel, regenerate an offer, and continue playing locally.

## File overview

- `index.html` – UI shell, scoreboard, and static controls.
- `style.css` – Neon-inspired styling and responsive layout.
- `tron.js` – Game loop, AI heuristics, canvas rendering, gamepad support, and WebRTC helper.

## Assets

No external assets are required. The canvas renders all visuals programmatically.
