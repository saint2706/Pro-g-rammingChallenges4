const canvas = document.getElementById('arena');
const ctx = canvas.getContext('2d');
const scoreboardEl = document.getElementById('scoreboard');
const statusTextEl = document.getElementById('status-text');
const nextRoundBtn = document.getElementById('next-round');
const form = document.getElementById('match-form');
const playerConfigBody = document.getElementById('player-config');
const resetBtn = document.getElementById('reset-btn');
const networkRoleSelect = document.getElementById('network-role');
const signalIo = document.getElementById('signal-io');
const createOfferBtn = document.getElementById('create-offer');
const acceptAnswerBtn = document.getElementById('accept-answer');
const submitOfferBtn = document.getElementById('submit-offer');

const DIRECTIONS = {
  up: { x: 0, y: -1 },
  down: { x: 0, y: 1 },
  left: { x: -1, y: 0 },
  right: { x: 1, y: 0 }
};

const OPPOSITES = {
  up: 'down',
  down: 'up',
  left: 'right',
  right: 'left'
};

const CONTROL_SCHEMES = {
  WASD: {
    name: 'Keyboard – WASD',
    bindings: {
      w: 'up',
      a: 'left',
      s: 'down',
      d: 'right'
    }
  },
  ARROWS: {
    name: 'Keyboard – Arrow Keys',
    bindings: {
      ArrowUp: 'up',
      ArrowLeft: 'left',
      ArrowDown: 'down',
      ArrowRight: 'right'
    }
  },
  IJKL: {
    name: 'Keyboard – IJKL',
    bindings: {
      i: 'up',
      j: 'left',
      k: 'down',
      l: 'right'
    }
  },
  NUMPAD: {
    name: 'Keyboard – Numpad 8456',
    bindings: {
      8: 'up',
      4: 'left',
      5: 'down',
      6: 'right'
    }
  }
};

const PLAYER_PRESETS = [
  {
    id: 'P1',
    name: 'Neon Rider',
    role: 'human',
    controller: 'WASD',
    color: '#0ff0fc'
  },
  {
    id: 'P2',
    name: 'Solar Flare',
    role: 'human',
    controller: 'ARROWS',
    color: '#ff2f6d'
  },
  {
    id: 'P3',
    name: 'Circuit AI',
    role: 'ai',
    controller: 'AI',
    color: '#ffe066'
  },
  {
    id: 'P4',
    name: 'Violet Ghost',
    role: 'off',
    controller: 'AI',
    color: '#b56dff'
  }
];

const SPEED_PRESETS = {
  slow: 180,
  normal: 120,
  fast: 80
};

const SIZE_PRESETS = {
  sm: { width: 32, height: 24 },
  md: { width: 44, height: 32 },
  lg: { width: 60, height: 44 }
};

const VARIANT_NAMES = {
  classic: 'Classic Grid',
  ring: 'Fusion Ring',
  maze: 'Sector Maze',
  pillars: 'Quad Pillars'
};

const BOOST_TYPES = ['speed', 'shield'];

const MAX_GAMEPADS = 4;

class LightCycle {
  constructor(descriptor) {
    Object.assign(this, descriptor);
    this.spawn = { x: 0, y: 0 };
    this.direction = 'right';
    this.pending = null;
    this.alive = false;
    this.trail = [];
    this.score = 0;
    this.speedTicks = 0;
    this.shieldTicks = 0;
  }

  resetForRound(spawn, initialDirection) {
    this.spawn = spawn;
    this.position = { ...spawn };
    this.direction = initialDirection;
    this.pending = null;
    this.alive = true;
    this.trail = [{ ...spawn }];
    this.speedTicks = 0;
    this.shieldTicks = 0;
  }
}

class NetworkManager {
  constructor(updateCallback, inputCallback, statusCallback) {
    this.peer = null;
    this.channel = null;
    this.role = 'standalone';
    this.updateCallback = updateCallback;
    this.inputCallback = inputCallback;
    this.statusCallback = statusCallback;
    this.isReady = false;
    this.pendingPlayerId = null;
  }

  setRole(role) {
    this.role = role;
    if (role === 'standalone') {
      this.teardown();
    }
  }

  teardown() {
    if (this.channel) {
      this.channel.close();
    }
    if (this.peer) {
      this.peer.close();
    }
    this.peer = null;
    this.channel = null;
    this.isReady = false;
  }

  async createOffer(playerId) {
    this.pendingPlayerId = playerId;
    this.peer = new RTCPeerConnection({
      iceServers: [{ urls: ['stun:stun.l.google.com:19302'] }]
    });
    this.channel = this.peer.createDataChannel('tron');
    this.attachChannel();
    const offer = await this.peer.createOffer();
    await this.peer.setLocalDescription(offer);
    await this.waitForIceGathering();
    return btoa(JSON.stringify(this.peer.localDescription));
  }

  async acceptAnswer(answerText) {
    if (!this.peer) {
      throw new Error('Create an offer first.');
    }
    const answer = JSON.parse(atob(answerText));
    await this.peer.setRemoteDescription(answer);
    this.statusCallback('Remote pilot connected.');
  }

  async submitOffer(offerText, playerId) {
    this.pendingPlayerId = playerId;
    this.peer = new RTCPeerConnection({
      iceServers: [{ urls: ['stun:stun.l.google.com:19302'] }]
    });
    this.peer.ondatachannel = (event) => {
      this.channel = event.channel;
      this.attachChannel();
    };
    await this.peer.setRemoteDescription(JSON.parse(atob(offerText)));
    const answer = await this.peer.createAnswer();
    await this.peer.setLocalDescription(answer);
    await this.waitForIceGathering();
    return btoa(JSON.stringify(this.peer.localDescription));
  }

  attachChannel() {
    this.channel.binaryType = 'arraybuffer';
    this.channel.onopen = () => {
      this.isReady = true;
      this.statusCallback('Data channel ready.');
      if (this.role === 'client') {
        this.channel.send(
          JSON.stringify({
            type: 'identify',
            playerId: this.pendingPlayerId
          })
        );
      } else if (this.role === 'host' && this.pendingPlayerId) {
        this.requestInputControl(this.pendingPlayerId);
      }
    };
    this.channel.onmessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        if (data.type === 'state') {
          this.updateCallback(data.payload);
        } else if (data.type === 'requestInput') {
          this.pendingPlayerId = data.playerId;
        } else if (data.type === 'identify') {
          this.pendingPlayerId = data.playerId;
        } else if (data.type === 'input') {
          this.inputCallback(data);
        } else if (data.type === 'status') {
          this.statusCallback(data.message);
        }
      } catch (err) {
        console.error('Network message parse error', err);
      }
    };
    this.channel.onclose = () => {
      this.statusCallback('Network channel closed.');
      this.isReady = false;
    };
  }

  sendState(state) {
    if (this.isReady && this.channel && this.channel.readyState === 'open') {
      this.channel.send(
        JSON.stringify({
          type: 'state',
          payload: state
        })
      );
    }
  }

  sendStatus(message) {
    if (this.isReady && this.channel && this.channel.readyState === 'open') {
      this.channel.send(
        JSON.stringify({
          type: 'status',
          message
        })
      );
    }
  }

  requestInputControl(playerId) {
    if (this.isReady && this.channel && this.channel.readyState === 'open') {
      this.channel.send(
        JSON.stringify({
          type: 'requestInput',
          playerId
        })
      );
    }
  }

  forwardInput(playerId, direction) {
    if (this.isReady && this.channel && this.channel.readyState === 'open') {
      this.channel.send(
        JSON.stringify({
          type: 'input',
          playerId,
          direction
        })
      );
    }
  }

  waitForIceGathering() {
    if (!this.peer) return Promise.resolve();
    if (this.peer.iceGatheringState === 'complete') {
      return Promise.resolve();
    }
    return new Promise((resolve) => {
      const checkState = () => {
        if (!this.peer) {
          resolve();
          return;
        }
        if (this.peer.iceGatheringState === 'complete') {
          this.peer.removeEventListener('icegatheringstatechange', checkState);
          resolve();
        }
      };
      this.peer.addEventListener('icegatheringstatechange', checkState);
    });
  }
}

class TronGame {
  constructor() {
    this.players = PLAYER_PRESETS.map((preset) => new LightCycle(preset));
    this.gridWidth = SIZE_PRESETS.md.width;
    this.gridHeight = SIZE_PRESETS.md.height;
    this.cellSize = 20;
    this.grid = [];
    this.variant = 'classic';
    this.speed = SPEED_PRESETS.normal;
    this.roundTimer = null;
    this.running = false;
    this.tickCount = 0;
    this.targetScore = 3;
    this.boosts = [];
    this.obstacles = new Set();
    this.pendingNetworkPlayer = null;
    this.networkRole = 'standalone';
    this.remoteState = null;

    this.gamepadAssignments = {};
    this.gamepadLastDirections = {};
    this.keyToPlayer = new Map();

    this.network = new NetworkManager(
      (state) => this.applyRemoteState(state),
      (data) => this.applyRemoteInput(data),
      (message) => this.setStatus(message)
    );

    this.setupPlayerConfigUI();
    this.renderScoreboard();
    this.updateNetworkControls();

    window.addEventListener('keydown', (event) => this.handleKey(event));
    window.addEventListener('gamepadconnected', () => this.refreshGamepadOptions());
    window.addEventListener('gamepaddisconnected', () => this.refreshGamepadOptions());

    nextRoundBtn.addEventListener('click', () => {
      if (this.networkRole === 'client') {
        this.setStatus('Waiting for host to start the next round.');
        return;
      }
      this.startRound();
    });
  }

  setupPlayerConfigUI() {
    playerConfigBody.innerHTML = '';
    this.players.forEach((player, index) => {
      const row = document.createElement('tr');
      row.dataset.playerId = player.id;

      const numberCell = document.createElement('td');
      numberCell.textContent = index + 1;
      row.appendChild(numberCell);

      const nameCell = document.createElement('td');
      const nameInput = document.createElement('input');
      nameInput.type = 'text';
      nameInput.value = player.name;
      nameInput.maxLength = 24;
      nameInput.addEventListener('input', () => {
        player.name = nameInput.value || player.name;
        this.renderScoreboard();
      });
      nameCell.appendChild(nameInput);
      row.appendChild(nameCell);

      const roleCell = document.createElement('td');
      const roleSelect = document.createElement('select');
      ['off', 'human', 'ai', 'remote'].forEach((role) => {
        const option = document.createElement('option');
        option.value = role;
        option.textContent =
          role === 'off'
            ? 'Inactive'
            : role === 'human'
            ? 'Human'
            : role === 'ai'
            ? 'AI'
            : 'Remote';
        if (player.role === role) option.selected = true;
        roleSelect.appendChild(option);
      });
      roleSelect.addEventListener('change', () => {
        player.role = roleSelect.value;
        if (player.role === 'remote' && (!player.controller || player.controller === 'AI')) {
          player.controller = 'WASD';
        }
        if (player.role === 'ai') {
          player.controller = 'AI';
        }
        if (player.role === 'remote') {
          this.network.pendingPlayerId = player.id;
        }
        if (player.role !== 'remote' && this.network.pendingPlayerId === player.id) {
          this.network.pendingPlayerId = null;
        }
        controllerSelect.disabled = player.role === 'off' || player.role === 'ai';
        colorInput.disabled = player.role === 'off';
        this.refreshControllerOptions(controllerSelect, player);
        if (!controllerSelect.disabled) {
          controllerSelect.value = player.controller;
        }
        this.renderScoreboard();
        this.updateNetworkControls();
      });
      roleCell.appendChild(roleSelect);
      row.appendChild(roleCell);

      const controllerCell = document.createElement('td');
      const controllerSelect = document.createElement('select');
      controllerSelect.dataset.playerId = player.id;
      controllerCell.appendChild(controllerSelect);
      row.appendChild(controllerCell);

      const colorCell = document.createElement('td');
      colorCell.className = 'player-color';
      const chip = document.createElement('span');
      chip.className = 'color-chip';
      chip.style.background = player.color;
      const colorInput = document.createElement('input');
      colorInput.type = 'color';
      colorInput.value = player.color;
      colorInput.addEventListener('input', () => {
        player.color = colorInput.value;
        chip.style.background = player.color;
        this.renderScoreboard();
      });
      colorCell.appendChild(chip);
      colorCell.appendChild(colorInput);
      row.appendChild(colorCell);

      playerConfigBody.appendChild(row);
      this.refreshControllerOptions(controllerSelect, player);
      controllerSelect.disabled = player.role !== 'human';
      colorInput.disabled = player.role === 'off';
    });
    this.updateNetworkControls();
  }

  refreshControllerOptions(selectEl, player) {
    const selected = player.controller;
    selectEl.innerHTML = '';
    Object.entries(CONTROL_SCHEMES).forEach(([key, scheme]) => {
      const option = document.createElement('option');
      option.value = key;
      option.textContent = scheme.name;
      selectEl.appendChild(option);
    });
    for (let i = 0; i < MAX_GAMEPADS; i++) {
      const option = document.createElement('option');
      option.value = `GAMEPAD_${i}`;
      option.textContent = `Gamepad ${i + 1}`;
      selectEl.appendChild(option);
    }
    if (selected) {
      selectEl.value = selected;
    }
    selectEl.addEventListener('change', () => {
      player.controller = selectEl.value;
      if (player.controller.startsWith('GAMEPAD')) {
        this.gamepadAssignments[player.id] = Number(player.controller.split('_')[1]);
      }
    });
  }

  refreshGamepadOptions() {
    const gamepads = navigator.getGamepads ? navigator.getGamepads() : [];
    document.querySelectorAll('#player-config select').forEach((selectEl) => {
      if (!selectEl.dataset.playerId) return;
      const currentValue = selectEl.value;
      const options = Array.from(selectEl.options).filter((opt) => opt.value.startsWith('GAMEPAD_'));
      options.forEach((opt) => selectEl.removeChild(opt));
      for (let i = 0; i < gamepads.length; i++) {
        if (!gamepads[i]) continue;
        const option = document.createElement('option');
        option.value = `GAMEPAD_${i}`;
        option.textContent = `Gamepad ${i + 1}`;
        selectEl.appendChild(option);
      }
      selectEl.value = currentValue;
    });
  }

  configureFromForm() {
    const variant = document.getElementById('variant-select').value;
    const size = document.getElementById('size-select').value;
    const speed = document.getElementById('speed-select').value;
    const targetScore = Math.max(1, Math.min(15, Number(document.getElementById('target-score').value) || 3));

    const { width, height } = SIZE_PRESETS[size] || SIZE_PRESETS.md;
    this.gridWidth = width;
    this.gridHeight = height;
    const cellSize = Math.floor(Math.min(880 / width, 640 / height));
    this.cellSize = Math.max(6, cellSize);
    canvas.width = this.gridWidth * this.cellSize;
    canvas.height = this.gridHeight * this.cellSize;

    this.variant = variant;
    this.speed = SPEED_PRESETS[speed] || SPEED_PRESETS.normal;
    this.targetScore = targetScore;

    this.players.forEach((player) => {
      const row = playerConfigBody.querySelector(`tr[data-player-id="${player.id}"]`);
      if (!row) return;
      const nameInput = row.querySelector('input[type="text"]');
      player.name = nameInput.value.trim() || player.name;
      const role = row.querySelector('select').value;
      player.role = role;
      const controllerSelect = row.querySelector('td:nth-child(4) select');
      if (controllerSelect) {
        player.controller = controllerSelect.value;
      }
    });

    this.players.forEach((player) => {
      if (player.role === 'off') {
        player.score = 0;
      }
    });

    this.renderScoreboard();
  }

  buildGrid() {
    this.grid = Array.from({ length: this.gridHeight }, () => Array(this.gridWidth).fill(null));
    this.obstacles.clear?.();
    this.obstacles = new Set();

    if (this.variant === 'ring') {
      const innerMargin = 4;
      for (let y = innerMargin; y < this.gridHeight - innerMargin; y++) {
        this.obstacles.add(`${innerMargin},${y}`);
        this.obstacles.add(`${this.gridWidth - innerMargin - 1},${y}`);
      }
      for (let x = innerMargin; x < this.gridWidth - innerMargin; x++) {
        this.obstacles.add(`${x},${innerMargin}`);
        this.obstacles.add(`${x},${this.gridHeight - innerMargin - 1}`);
      }
      // carve spawn gaps
      this.obstacles.delete(`${innerMargin},${Math.floor(this.gridHeight / 2)}`);
      this.obstacles.delete(`${this.gridWidth - innerMargin - 1},${Math.floor(this.gridHeight / 2)}`);
    } else if (this.variant === 'maze') {
      for (let y = 4; y < this.gridHeight - 4; y += 6) {
        for (let x = 6; x < this.gridWidth - 6; x++) {
          if (x % 2 === 0) {
            this.obstacles.add(`${x},${y}`);
          }
        }
      }
      for (let x = 4; x < this.gridWidth - 4; x += 8) {
        for (let y = 6; y < this.gridHeight - 6; y++) {
          if (y % 2 === 0) {
            this.obstacles.add(`${x},${y}`);
          }
        }
      }
    } else if (this.variant === 'pillars') {
      const offsets = [
        [Math.floor(this.gridWidth * 0.25), Math.floor(this.gridHeight * 0.25)],
        [Math.floor(this.gridWidth * 0.75), Math.floor(this.gridHeight * 0.25)],
        [Math.floor(this.gridWidth * 0.25), Math.floor(this.gridHeight * 0.75)],
        [Math.floor(this.gridWidth * 0.75), Math.floor(this.gridHeight * 0.75)]
      ];
      offsets.forEach(([cx, cy]) => {
        for (let dx = -1; dx <= 1; dx++) {
          for (let dy = -1; dy <= 1; dy++) {
            this.obstacles.add(`${cx + dx},${cy + dy}`);
          }
        }
      });
    }

    this.obstacles.forEach((key) => {
      const [x, y] = key.split(',').map(Number);
      if (this.grid[y] && this.grid[y][x] !== undefined) {
        this.grid[y][x] = 'wall';
      }
    });
  }

  spawnPlayers() {
    const activePlayers = this.players.filter((p) => p.role !== 'off');
    const spawnPoints = this.calculateSpawnPoints(activePlayers.length);
    activePlayers.forEach((player, index) => {
      const spawn = spawnPoints[index] || spawnPoints[0];
      const direction = this.initialDirectionForSpawn(spawn);
      player.resetForRound(spawn, direction);
      this.grid[spawn.y][spawn.x] = player.id;
    });
  }

  calculateSpawnPoints(count) {
    const points = [];
    const padding = 3;
    if (count === 0) return points;
    const positions = [
      { x: padding, y: Math.floor(this.gridHeight / 2) },
      { x: this.gridWidth - padding - 1, y: Math.floor(this.gridHeight / 2) },
      { x: Math.floor(this.gridWidth / 2), y: padding },
      { x: Math.floor(this.gridWidth / 2), y: this.gridHeight - padding - 1 }
    ];
    for (let i = 0; i < count; i++) {
      points.push(positions[i % positions.length]);
    }
    return points;
  }

  initialDirectionForSpawn(spawn) {
    if (spawn.x <= 3) return 'right';
    if (spawn.x >= this.gridWidth - 4) return 'left';
    if (spawn.y <= 3) return 'down';
    return 'up';
  }

  startRound() {
    if (this.networkRole === 'client') return;
    this.configureFromForm();
    const active = this.players.filter((p) => p.role !== 'off');
    if (active.length < 2) {
      this.setStatus('Need at least two active players (human, AI, or remote) to start.');
      return;
    }
    this.buildGrid();
    this.spawnPlayers();
    this.boosts = [];
    this.tickCount = 0;
    this.running = true;
    this.remoteState = null;
    statusTextEl.textContent = 'Round in progress – avoid trails and claim the arena!';
    nextRoundBtn.disabled = true;
    if (this.roundTimer) {
      clearInterval(this.roundTimer);
    }
    if (this.networkRole === 'host' && this.network.isReady) {
      this.network.sendStatus('Host starting a new round.');
      const remotePilot = this.players.find((p) => p.role === 'remote');
      if (remotePilot) {
        this.network.requestInputControl(remotePilot.id);
      }
    }
    this.roundTimer = setInterval(() => this.tick(), this.speed);
    this.render();
  }

  endRound(winners, reason) {
    this.running = false;
    if (this.roundTimer) {
      clearInterval(this.roundTimer);
      this.roundTimer = null;
    }
    nextRoundBtn.disabled = false;

    if (winners.length === 1) {
      winners[0].score += 1;
      statusTextEl.textContent = `${winners[0].name} wins the round (${reason}).`;
    } else if (winners.length > 1) {
      winners.forEach((player) => (player.score += 1));
      statusTextEl.textContent = `Draw! Survivors split the round (${reason}).`;
    } else {
      statusTextEl.textContent = `All lightcycles derezzed (${reason}).`;
    }

    this.renderScoreboard();
    const champion = this.players.find((p) => p.score >= this.targetScore && p.role !== 'off');
    if (champion) {
      nextRoundBtn.disabled = true;
      statusTextEl.textContent = `${champion.name} claims the match (${champion.score} points)!`;
      if (this.networkRole === 'host' && this.network.isReady) {
        this.network.sendStatus(statusTextEl.textContent);
      }
    }
  }

  setStatus(message) {
    statusTextEl.textContent = message;
  }

  handleKey(event) {
    if (!this.running && this.networkRole !== 'client') return;
    if (this.networkRole === 'client' && !this.remoteState) return;
    const key = event.key;
    this.players.forEach((player) => {
      const humanLocal = player.role === 'human';
      const remoteClient = player.role === 'remote' && this.networkRole === 'client';
      if (!humanLocal && !remoteClient) return;
      const scheme = CONTROL_SCHEMES[player.controller];
      if (!scheme) return;
      const direction = scheme.bindings[key];
      if (direction) {
        this.queueDirection(player, direction);
      }
    });
  }

  queueDirection(player, direction) {
    if (!player.alive) return;
    if (direction === player.direction || direction === player.pending) return;
    if (OPPOSITES[direction] === player.direction) return;
    player.pending = direction;
    if (this.networkRole === 'client' && player.role === 'remote') {
      this.network.forwardInput(player.id, direction);
    }
  }

  handleGamepads() {
    if (!navigator.getGamepads) return;
    const gamepads = navigator.getGamepads();
    this.players.forEach((player) => {
      const humanLocal = player.role === 'human';
      const remoteClient = player.role === 'remote' && this.networkRole === 'client';
      if (!humanLocal && !remoteClient) return;
      if (!player.controller.startsWith('GAMEPAD')) return;
      const index = Number(player.controller.split('_')[1]);
      const pad = gamepads[index];
      if (!pad) return;
      const threshold = 0.5;
      let direction = null;
      if (pad.axes[0] <= -threshold) direction = 'left';
      else if (pad.axes[0] >= threshold) direction = 'right';
      else if (pad.axes[1] <= -threshold) direction = 'up';
      else if (pad.axes[1] >= threshold) direction = 'down';

      pad.buttons.forEach((button, idx) => {
        if (button.pressed) {
          if (idx === 12) direction = 'up';
          if (idx === 13) direction = 'down';
          if (idx === 14) direction = 'left';
          if (idx === 15) direction = 'right';
        }
      });

      if (!direction) {
        this.gamepadLastDirections[player.id] = null;
        return;
      }
      if (this.gamepadLastDirections[player.id] === direction) {
        return;
      }
      this.gamepadLastDirections[player.id] = direction;
      this.queueDirection(player, direction);
    });
  }

  updateAI() {
    this.players.forEach((player) => {
      if (!player.alive || player.role !== 'ai') return;
      const bestDirection = this.pickAIDirection(player);
      if (bestDirection) {
        this.queueDirection(player, bestDirection);
      }
    });
  }

  pickAIDirection(player) {
    const options = ['up', 'down', 'left', 'right'];
    let bestScore = -Infinity;
    let bestDirection = null;
    options.forEach((direction) => {
      if (direction === OPPOSITES[player.direction]) return;
      const next = {
        x: player.position.x + DIRECTIONS[direction].x,
        y: player.position.y + DIRECTIONS[direction].y
      };
      const state = this.evaluateMove(player, next);
      if (!state.valid) return;
      if (state.score > bestScore) {
        bestScore = state.score;
        bestDirection = direction;
      }
    });
    return bestDirection;
  }

  evaluateMove(player, next) {
    if (!this.inBounds(next.x, next.y)) return { valid: false, score: -Infinity };
    const cell = this.grid[next.y][next.x];
    if (cell && cell !== player.id && player.shieldTicks <= 0) {
      return { valid: false, score: -Infinity };
    }
    if (cell === 'wall') {
      return { valid: false, score: -Infinity };
    }

    const flood = this.floodFillArea(next.x, next.y, 220);
    const distanceWall = Math.min(
      next.x,
      next.y,
      this.gridWidth - 1 - next.x,
      this.gridHeight - 1 - next.y
    );
    let score = flood * 1.8 + distanceWall * 0.3;
    const opponentDistance = this.distanceToClosestOpponent(player, next);
    if (opponentDistance !== null) {
      score += Math.max(0, 12 - opponentDistance);
    }
    const boostHere = this.boosts.find((boost) => boost.x === next.x && boost.y === next.y);
    if (boostHere) score += 15;
    return { valid: true, score };
  }

  floodFillArea(startX, startY, limit = 200) {
    const visited = new Set();
    const queue = [{ x: startX, y: startY }];
    let size = 0;
    while (queue.length && size < limit) {
      const { x, y } = queue.shift();
      const key = `${x},${y}`;
      if (visited.has(key)) continue;
      if (!this.inBounds(x, y)) continue;
      const cell = this.grid[y][x];
      if (cell) continue;
      visited.add(key);
      size += 1;
      Object.values(DIRECTIONS).forEach(({ x: dx, y: dy }) => {
        const nx = x + dx;
        const ny = y + dy;
        const nKey = `${nx},${ny}`;
        if (!visited.has(nKey) && this.inBounds(nx, ny)) {
          const value = this.grid[ny][nx];
          if (!value) queue.push({ x: nx, y: ny });
        }
      });
    }
    return size;
  }

  distanceToClosestOpponent(player, next) {
    let best = null;
    this.players.forEach((other) => {
      if (other === player || !other.alive) return;
      const distance = Math.abs(other.position.x - next.x) + Math.abs(other.position.y - next.y);
      if (best === null || distance < best) best = distance;
    });
    return best;
  }

  inBounds(x, y) {
    return x >= 0 && y >= 0 && x < this.gridWidth && y < this.gridHeight;
  }

  tick() {
    if (!this.running) return;
    this.tickCount += 1;
    this.handleGamepads();
    this.updateAI();

    if (this.tickCount % 18 === 0) {
      this.spawnBoost();
    }

    const plannedMoves = [];
    this.players.forEach((player) => {
      if (!player.alive) return;
      if (player.pending) {
        player.direction = player.pending;
        player.pending = null;
      }
      const dir = DIRECTIONS[player.direction];
      if (!dir) return;
      const target = {
        x: player.position.x + dir.x,
        y: player.position.y + dir.y
      };
      plannedMoves.push({ player, target });
    });

    const crashes = new Set();
    const occupancy = new Map();

    plannedMoves.forEach(({ player, target }) => {
      if (!this.inBounds(target.x, target.y)) {
        crashes.add(player);
        return;
      }
      const cell = this.grid[target.y][target.x];
      const boost = this.boosts.find((b) => b.x === target.x && b.y === target.y);
      if (cell && cell !== player.id) {
        if (player.shieldTicks > 0) {
          // ghost through but consume shield
          player.shieldTicks -= 1;
        } else {
          crashes.add(player);
          return;
        }
      }
      if (cell === 'wall') {
        if (player.shieldTicks > 0) {
          player.shieldTicks = 0;
        } else {
          crashes.add(player);
          return;
        }
      }
      const key = `${target.x},${target.y}`;
      if (!occupancy.has(key)) {
        occupancy.set(key, []);
      }
      occupancy.get(key).push({ player, target, boost });
    });

    occupancy.forEach((entries) => {
      if (entries.length > 1) {
        entries.forEach(({ player }) => crashes.add(player));
      }
    });

    plannedMoves.forEach(({ player, target }) => {
      if (crashes.has(player)) {
        player.alive = false;
        return;
      }
      this.grid[player.position.y][player.position.x] = player.id;
      player.position = target;
      player.trail.push({ ...target });
      if (player.trail.length > 512) {
        player.trail.shift();
      }
      const boost = this.boosts.find((b) => b.x === target.x && b.y === target.y);
      if (boost) {
        if (boost.type === 'speed') {
          player.speedTicks = 5;
        }
        if (boost.type === 'shield') {
          player.shieldTicks = 5;
        }
        this.boosts = this.boosts.filter((b) => b !== boost);
      }
      this.grid[target.y][target.x] = player.id;
    });

    this.players.forEach((player) => {
      if (player.speedTicks > 0) {
        player.speedTicks -= 1;
        if (player.speedTicks >= 0) {
          // Extra hop for speed boost
          const dir = DIRECTIONS[player.direction];
          const extraTarget = {
            x: player.position.x + dir.x,
            y: player.position.y + dir.y
          };
          const evaluation = this.evaluateMove(player, extraTarget);
          if (evaluation.valid) {
            player.position = extraTarget;
            player.trail.push({ ...extraTarget });
            this.grid[extraTarget.y][extraTarget.x] = player.id;
          } else {
            player.speedTicks = 0;
            crashes.add(player);
          }
        }
      }
      if (player.shieldTicks > 0) {
        player.shieldTicks -= 1;
      }
    });

    crashes.forEach((player) => {
      player.alive = false;
    });

    const alivePlayers = this.players.filter((player) => player.alive);
    let roundOver = false;
    if (alivePlayers.length === 0) {
      roundOver = true;
      this.endRound([], 'complete wipeout');
    } else if (alivePlayers.length === 1 && this.players.filter((p) => p.role !== 'off').length > 1) {
      roundOver = true;
      this.endRound(alivePlayers, 'last bike standing');
    }

    if (!roundOver) {
      this.render();
    }

    if (this.networkRole === 'host' && this.network.isReady) {
      this.network.sendState(this.serializeState());
    }
  }

  spawnBoost() {
    const freeCells = [];
    for (let y = 0; y < this.gridHeight; y++) {
      for (let x = 0; x < this.gridWidth; x++) {
        if (!this.grid[y][x]) {
          freeCells.push({ x, y });
        }
      }
    }
    if (!freeCells.length) return;
    const { x, y } = freeCells[Math.floor(Math.random() * freeCells.length)];
    const type = BOOST_TYPES[Math.floor(Math.random() * BOOST_TYPES.length)];
    this.boosts.push({ x, y, type, ttl: 80 });
  }

  decayBoosts() {
    this.boosts = this.boosts
      .map((boost) => ({ ...boost, ttl: boost.ttl - 1 }))
      .filter((boost) => boost.ttl > 0);
  }

  serializeState() {
    return {
      gridWidth: this.gridWidth,
      gridHeight: this.gridHeight,
      cellSize: this.cellSize,
      players: this.players.map((player) => ({
        id: player.id,
        name: player.name,
        color: player.color,
        position: player.position,
        trail: player.trail,
        alive: player.alive,
        score: player.score,
        role: player.role,
        direction: player.direction
      })),
      boosts: this.boosts,
      status: statusTextEl.textContent,
      tick: this.tickCount,
      variant: this.variant,
      targetScore: this.targetScore,
      obstacles: Array.from(this.obstacles)
    };
  }

  applyRemoteState(state) {
    if (this.networkRole !== 'client') return;
    this.remoteState = state;
    this.gridWidth = state.gridWidth;
    this.gridHeight = state.gridHeight;
    this.cellSize = state.cellSize;
    canvas.width = this.gridWidth * this.cellSize;
    canvas.height = this.gridHeight * this.cellSize;
    statusTextEl.textContent = state.status;
    this.obstacles = new Set(state.obstacles || []);
    state.players.forEach((remote) => {
      const local = this.players.find((p) => p.id === remote.id);
      if (local) {
        local.name = remote.name;
        local.color = remote.color;
        local.role = remote.role;
        local.score = remote.score;
        local.controller = local.controller || 'WASD';
      }
    });
    this.renderRemoteState();
  }

  applyRemoteInput(data) {
    if (this.networkRole !== 'host') return;
    const player = this.players.find((p) => p.id === data.playerId);
    if (player) {
      this.queueDirection(player, data.direction);
    }
  }

  renderRemoteState() {
    if (!this.remoteState) return;
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    this.drawBackground();
    this.remoteState.players.forEach((player) => {
      this.drawTrail(player, player.trail);
    });
    this.remoteState.boosts.forEach((boost) => this.drawBoost(boost));
    this.remoteState.players.forEach((player) => this.drawCycle(player));
    this.renderScoreboard(this.remoteState.players);
  }

  renderScoreboard(overridePlayers) {
    const players = overridePlayers || this.players;
    scoreboardEl.innerHTML = '';
    players
      .filter((player) => player.role !== 'off')
      .forEach((player) => {
        const card = document.createElement('div');
        card.className = 'score-card';
        card.style.borderColor = `${player.color}66`;
        const name = document.createElement('div');
        name.className = 'score-name';
        name.textContent = player.name;
        const points = document.createElement('div');
        points.className = 'score-points';
        points.textContent = `${player.score} pts`;
        card.appendChild(name);
        card.appendChild(points);
        scoreboardEl.appendChild(card);
      });
  }

  drawBackground() {
    const gradient = ctx.createLinearGradient(0, 0, canvas.width, canvas.height);
    gradient.addColorStop(0, '#061026');
    gradient.addColorStop(1, '#030614');
    ctx.fillStyle = gradient;
    ctx.fillRect(0, 0, canvas.width, canvas.height);

    ctx.strokeStyle = 'rgba(0, 200, 255, 0.12)';
    ctx.lineWidth = 1;
    for (let x = 0; x <= this.gridWidth; x++) {
      ctx.beginPath();
      ctx.moveTo(x * this.cellSize, 0);
      ctx.lineTo(x * this.cellSize, canvas.height);
      ctx.stroke();
    }
    for (let y = 0; y <= this.gridHeight; y++) {
      ctx.beginPath();
      ctx.moveTo(0, y * this.cellSize);
      ctx.lineTo(canvas.width, y * this.cellSize);
      ctx.stroke();
    }

    this.obstacles.forEach((key) => {
      const [x, y] = key.split(',').map(Number);
      ctx.fillStyle = 'rgba(255, 87, 34, 0.45)';
      ctx.fillRect(
        x * this.cellSize,
        y * this.cellSize,
        this.cellSize,
        this.cellSize
      );
    });
  }

  drawTrail(player, trail) {
    ctx.save();
    ctx.shadowColor = `${player.color}55`;
    ctx.shadowBlur = 16;
    ctx.fillStyle = `${player.color}55`;
    trail.forEach((segment, index) => {
      const alpha = Math.min(1, 0.35 + (index / trail.length) * 0.65);
      ctx.fillStyle = this.fadeColor(player.color, alpha);
      ctx.fillRect(
        segment.x * this.cellSize,
        segment.y * this.cellSize,
        this.cellSize,
        this.cellSize
      );
    });
    ctx.restore();
  }

  drawBoost(boost) {
    ctx.save();
    const colors = {
      speed: '#2bff88',
      shield: '#8bfffd'
    };
    ctx.fillStyle = `${colors[boost.type] || '#fff'}AA`;
    ctx.beginPath();
    const cx = boost.x * this.cellSize + this.cellSize / 2;
    const cy = boost.y * this.cellSize + this.cellSize / 2;
    const radius = this.cellSize * 0.35;
    ctx.arc(cx, cy, radius, 0, Math.PI * 2);
    ctx.fill();
    ctx.restore();
  }

  drawCycle(player) {
    if (!player.alive) return;
    ctx.save();
    ctx.fillStyle = player.color;
    ctx.shadowColor = `${player.color}aa`;
    ctx.shadowBlur = 18;
    const size = this.cellSize * 0.8;
    const offset = (this.cellSize - size) / 2;
    ctx.fillRect(
      player.position.x * this.cellSize + offset,
      player.position.y * this.cellSize + offset,
      size,
      size
    );
    ctx.restore();
  }

  fadeColor(hex, alpha = 1) {
    const bigint = parseInt(hex.replace('#', ''), 16);
    const r = (bigint >> 16) & 255;
    const g = (bigint >> 8) & 255;
    const b = bigint & 255;
    return `rgba(${r}, ${g}, ${b}, ${alpha.toFixed(2)})`;
  }

  render() {
    ctx.clearRect(0, 0, canvas.width, canvas.height);
    this.drawBackground();
    this.players
      .filter((player) => player.role !== 'off')
      .forEach((player) => {
        this.drawTrail(player, player.trail);
      });
    this.decayBoosts();
    this.boosts.forEach((boost) => this.drawBoost(boost));
    this.players.forEach((player) => this.drawCycle(player));
    this.renderScoreboard();
  }

  updateNetworkControls() {
    const remoteAvailable = this.players.some((player) => player.role === 'remote');
    if (this.networkRole === 'host') {
      createOfferBtn.disabled = !remoteAvailable;
      acceptAnswerBtn.disabled = !remoteAvailable;
      submitOfferBtn.disabled = true;
    } else if (this.networkRole === 'client') {
      createOfferBtn.disabled = true;
      acceptAnswerBtn.disabled = true;
      submitOfferBtn.disabled = false;
    } else {
      createOfferBtn.disabled = true;
      acceptAnswerBtn.disabled = true;
      submitOfferBtn.disabled = true;
    }
  }
}

const game = new TronGame();

form.addEventListener('submit', (event) => {
  event.preventDefault();
  if (game.networkRole === 'client') {
    game.setStatus('Client mode mirrors the host. Ask the host to launch the round.');
    return;
  }
  game.startRound();
});

resetBtn.addEventListener('click', () => {
  PLAYER_PRESETS.forEach((preset, index) => {
    const player = game.players[index];
    Object.assign(player, preset);
    player.score = 0;
  });
  document.getElementById('variant-select').value = 'classic';
  document.getElementById('size-select').value = 'md';
  document.getElementById('speed-select').value = 'normal';
  document.getElementById('target-score').value = 3;
  game.setupPlayerConfigUI();
  game.renderScoreboard();
  game.updateNetworkControls();
  statusTextEl.textContent = 'Configuration reset. Press Launch Match to play.';
});

networkRoleSelect.addEventListener('change', async () => {
  const role = networkRoleSelect.value;
  game.networkRole = role;
  game.network.setRole(role);
  signalIo.value = '';
  if (role === 'host') {
    game.setStatus('Host mode: set a player to Remote and share the offer.');
  } else if (role === 'client') {
    game.setStatus('Client mode: paste host offer, click Submit host offer, then share the answer back.');
  } else {
    game.setStatus('Standalone mode.');
  }
  game.updateNetworkControls();
});

createOfferBtn.addEventListener('click', async () => {
  try {
    const remotePlayer = game.players.find((player) => player.role === 'remote');
    if (!remotePlayer) {
      game.setStatus('Assign a player as Remote to generate an offer.');
      return;
    }
    const offer = await game.network.createOffer(remotePlayer.id);
    signalIo.value = offer;
    acceptAnswerBtn.disabled = false;
    game.setStatus('Offer created. Share it with the remote pilot and paste their answer below.');
  } catch (err) {
    game.setStatus(`Offer error: ${err.message}`);
  }
});

acceptAnswerBtn.addEventListener('click', async () => {
  try {
    const text = signalIo.value.trim();
    if (!text) {
      game.setStatus('Paste the remote answer first.');
      return;
    }
    await game.network.acceptAnswer(text);
    game.setStatus('Connection established. Remote pilot ready.');
  } catch (err) {
    game.setStatus(`Answer error: ${err.message}`);
  }
});

submitOfferBtn.addEventListener('click', async () => {
  try {
    const remotePlayer = game.players.find((player) => player.role === 'remote');
    if (!remotePlayer) {
      game.setStatus('The host must assign a remote player.');
      return;
    }
    const text = signalIo.value.trim();
    if (!text) {
      game.setStatus('Paste the host offer first.');
      return;
    }
    const answer = await game.network.submitOffer(text, remotePlayer.id);
    signalIo.value = answer;
    submitOfferBtn.disabled = true;
    game.setStatus('Answer generated. Send it to the host to finalise the connection.');
  } catch (err) {
    game.setStatus(`Connection error: ${err.message}`);
  }
});

function animationLoop() {
  requestAnimationFrame(animationLoop);
  if (game.networkRole === 'client' && game.remoteState) {
    game.handleGamepads();
    game.renderRemoteState();
  }
}

animationLoop();
