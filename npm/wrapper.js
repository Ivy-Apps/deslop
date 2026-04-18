#!/usr/bin/env node
const { spawnSync } = require('child_process');
const os = require('os');

const platform = os.platform();
const arch = os.arch();

const pkgName = `@ivy-apps/deslop-${platform}-${arch}`;
let binaryPath;
try {
  binaryPath = require.resolve(`${pkgName}/bin/deslop`);
} catch {
  console.error(`deslop: unsupported platform ${platform}-${arch}.`);
  console.error(`Supported: linux/darwin on x64 and arm64.`);
  process.exit(1);
}

const result = spawnSync(binaryPath, process.argv.slice(2), { stdio: 'inherit', shell: false });
if (result.error) {
  console.error(`deslop: failed to start: ${result.error.message}`);
  process.exit(1);
}
process.exit(result.status ?? 1);
