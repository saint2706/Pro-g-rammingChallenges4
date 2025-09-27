const d=document,c=d.getElementById("c"),gl=c.getContext("webgl");
if(!gl){d.body.textContent="WebGL unsupported";throw new Error("no gl");}
const vs=gl.createShader(gl.VERTEX_SHADER),fs=gl.createShader(gl.FRAGMENT_SHADER),pr=gl.createProgram();
const vsrc="attribute vec2 p;void main(){gl_Position=vec4(p,0.,1.);}",fsrc=`precision highp float;uniform float t;uniform vec2 r;float h(vec2 p){return fract(sin(dot(p,vec2(41.3,289.1)))*43758.5453.);}float n(vec2 p){vec2 i=floor(p),f=fract(p);float a=h(i),b=h(i+vec2(1.,0.)),c=h(i+vec2(0.,1.)),d=h(i+vec2(1.));vec2 u=f*f*(3.-2.*f);return mix(a,b,u.x)+(c-a)*u.y*(1.-u.x)+(d-b)*u.x*u.y;}void main(){vec2 uv=(gl_FragCoord.xy-.5*r)/r.y;float tt=t*.25;float s=1.,o=0.;for(int i=0;i<4;i++){o+=n(uv*s+tt)/s;s*=2.;}float radial=sin(length(uv*2.)-t*.2);float bands=sin((uv.x+uv.y+sin(tt))*8.);vec3 col=.5+.5*sin(vec3(.5,1.,1.5)*(o*3.+radial*2.+bands));gl_FragColor=vec4(col,1.);}`;
gl.shaderSource(vs,vsrc);gl.compileShader(vs);
if(!gl.getShaderParameter(vs,gl.COMPILE_STATUS))throw gl.getShaderInfoLog(vs);
gl.shaderSource(fs,fsrc);gl.compileShader(fs);
if(!gl.getShaderParameter(fs,gl.COMPILE_STATUS))throw gl.getShaderInfoLog(fs);
gl.attachShader(pr,vs);gl.attachShader(pr,fs);gl.linkProgram(pr);
if(!gl.getProgramParameter(pr,gl.LINK_STATUS))throw gl.getProgramInfoLog(pr);
const buf=gl.createBuffer();gl.bindBuffer(gl.ARRAY_BUFFER,buf);
gl.bufferData(gl.ARRAY_BUFFER,new Float32Array([-1,-1,1,-1,-1,1,1,1]),gl.STATIC_DRAW);
const loc=gl.getAttribLocation(pr,"p");gl.enableVertexAttribArray(loc);
gl.vertexAttribPointer(loc,2,gl.FLOAT,false,0,0);
const ut=gl.getUniformLocation(pr,"t"),ur=gl.getUniformLocation(pr,"r");
function resize(){const w=window.innerWidth,h=window.innerHeight;c.width=w;c.height=h;gl.viewport(0,0,w,h);}resize();
window.addEventListener("resize",resize);
let start=performance.now();
function render(now){gl.useProgram(pr);gl.uniform1f(ut,(now-start)/1000);gl.uniform2f(ur,c.width,c.height);gl.drawArrays(gl.TRIANGLE_STRIP,0,4);requestAnimationFrame(render);}requestAnimationFrame(render);
const AudioCtx=window.AudioContext||window.webkitAudioContext;let audioStarted=false,ctx,phase=0;
function synth(){if(audioStarted)return;audioStarted=true;ctx=new AudioCtx();const rate=ctx.sampleRate,node=ctx.createScriptProcessor(4096,0,1);const bpm=120;node.onaudioprocess=e=>{const out=e.outputBuffer.getChannelData(0);for(let i=0;i<out.length;i++){const t=phase/rate;const beat=t*bpm/60;const pulse=beat-Math.floor(beat);const kickEnv=Math.exp(-pulse*6);const kick=Math.sin(2*Math.PI*(50+200*(1-pulse))*t)*kickEnv;const arpFreq=220+110*Math.sin(beat*3.+Math.floor(beat));const lead=Math.sin(2*Math.PI*arpFreq*t+Math.sin(t*3.));const pad=Math.sin(2*Math.PI*0.5*t);out[i]=0.4*kick+0.2*lead+0.1*pad;phase++;}};node.connect(ctx.destination);ctx.resume();d.getElementById("hint").style.display="none";}
window.addEventListener("pointerdown",synth,{once:false});
window.addEventListener("keydown",synth,{once:false});
