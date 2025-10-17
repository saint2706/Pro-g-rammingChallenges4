export class NetworkManager {
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
          this.statusCallback(`Remote pilot ready as ${data.playerId}.`);
        } else if (data.type === 'input') {
          this.inputCallback(data.payload);
        } else if (data.type === 'status') {
          this.statusCallback(data.payload);
        }
      } catch (error) {
        console.error('Failed to parse network message', error);
      }
    };
    this.channel.onclose = () => {
      this.statusCallback('Connection closed.');
      this.isReady = false;
    };
  }

  requestInputControl(playerId) {
    if (!this.channel || this.channel.readyState !== 'open') return;
    this.channel.send(
      JSON.stringify({
        type: 'requestInput',
        playerId
      })
    );
  }

  sendState(payload) {
    if (!this.channel || this.channel.readyState !== 'open') return;
    this.channel.send(
      JSON.stringify({
        type: 'state',
        payload
      })
    );
  }

  sendInput(direction) {
    if (!this.channel || this.channel.readyState !== 'open') return;
    this.channel.send(
      JSON.stringify({
        type: 'input',
        payload: direction
      })
    );
  }

  sendStatus(message) {
    if (!this.channel || this.channel.readyState !== 'open') return;
    this.channel.send(
      JSON.stringify({
        type: 'status',
        payload: message
      })
    );
  }

  async waitForIceGathering() {
    if (!this.peer) return;
    if (this.peer.iceGatheringState === 'complete') return;
    await new Promise((resolve) => {
      const checkState = () => {
        if (!this.peer) return;
        if (this.peer.iceGatheringState === 'complete') {
          this.peer.removeEventListener('icegatheringstatechange', checkState);
          resolve();
        }
      };
      this.peer.addEventListener('icegatheringstatechange', checkState);
    });
  }
}
