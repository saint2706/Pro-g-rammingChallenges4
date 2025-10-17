local stage_json = [[
{
  "id": "stage2",
  "title": "Azure Storm",
  "difficulty": "hard",
  "background": "tempest",
  "boss": "skyEmpress",
  "patterns": [
    {
      "id": "denseWall",
      "parameters": [
        { "name": "speed", "default": 180 }
      ],
      "steps": [
        { "time": 0.0, "action": { "type": "spawnRadial", "bullet": "needle", "count": 24, "speed": 180 } },
        { "time": 0.4, "action": { "type": "spawnRadial", "bullet": "needle", "count": 28, "speed": 200 } }
      ]
    },
    {
      "id": "crossSpiral",
      "parameters": [
        { "name": "speed", "default": 160 },
        { "name": "spiralDirection", "default": -1 }
      ],
      "steps": [
        { "time": 0.0, "action": { "type": "spawnSpiral", "bullet": "spread", "count": 96, "revolutions": 5 } },
        { "time": 1.8, "action": { "type": "spawnSpiral", "bullet": "spread", "count": 96, "revolutions": 5 } }
      ]
    },
    {
      "id": "lightningStrike",
      "parameters": [
        { "name": "speed", "default": 220 }
      ],
      "steps": [
        { "time": 0.0, "action": { "type": "spawnAimed", "bullet": "orb", "speed": 200 } },
        { "time": 0.25, "action": { "type": "spawnAimed", "bullet": "orb", "speed": 220 } },
        { "time": 0.5, "action": { "type": "spawnRadial", "bullet": "needle", "count": 32, "speed": 210 } }
      ]
    }
  ],
  "waves": [
    { "time": 0.8, "enemies": ["denseWall"] },
    { "time": 4.0, "enemies": ["crossSpiral"] },
    { "time": 9.0, "enemies": ["lightningStrike"], "patternOverrides": { "lightningStrike": { "speed": 240 } } }
  ],
  "configOverrides": {
    "difficultyScaling": {
      "hard": 1.4,
      "lunatic": 1.7
    }
  }
}
]]

return stage_json
