#!/usr/bin/env node
const { spawnSync } = require('child_process');
const path = require('path');
const os = require('os');

const platform = os.platform(); // 'darwin', 'linux', 'win32'
const arch = os.arch();         // 'arm64', 'x64'
const ext = platform === 'win32' ? '.exe' : '';

const binaryName = `deslop-${platform}-${arch}${ext}`;
const binaryPath = path.join(__dirname, 'bin', binaryName);

const result = spawnSync(binaryPath, process.argv.slice(2), { stdio: 'inherit' });

if (result.error) {
  console.error(`Failed to start Deslop: ${result.error.message}`);
  process.exit(1);
}

process.exit(result.status ?? 1);
