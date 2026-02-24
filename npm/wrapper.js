#!/usr/bin/env node
const { spawnSync } = require('child_process');
const path = require('path');
const os = require('os');

const platform = os.platform(); // 'darwin', 'linux'
const arch = os.arch();         // 'arm64', 'x64'

const binaryName = `deslop-${platform}-${arch}`;
const binaryPath = path.join(__dirname, 'bin', binaryName);

const result = spawnSync(binaryPath, process.argv.slice(2), { 
  stdio: 'inherit',
  shell: false 
});

if (result.error) {
  if (result.error.code === 'ENOENT') {
    console.error(`Error: Deslop binary not found for ${platform}-${arch}.`);
    console.error(`Deslop currently supports Linux (x64) and macOS (arm64).`);
  } else {
    console.error(`Failed to start Deslop: ${result.error.message}`);
  }
  process.exit(1);
}

process.exit(result.status ?? 1);
