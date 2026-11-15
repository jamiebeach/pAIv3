# Start the host-side bridges the three agents use, after a reboot.
#   powershell -File tools\bridges.ps1          start any that are not running
#   powershell -File tools\bridges.ps1 status   say which are up
#   powershell -File tools\bridges.ps1 stop     stop them
# Each runs hidden, with its output in private\<name>.out.log and .err.log.
# A bridge whose port already answers is left alone. The relay to cloud guests has
# no port; it is found by its command line.
param([string]$Action = 'start')
$ErrorActionPreference = 'Stop'
$repo = Split-Path -Parent $PSScriptRoot
Set-Location $repo

$bridges = @(
  @{ name = 'beads-dev';    script = 'tools/beads-bridge.py'; config = 'private/beads-bridge.json';           port = 8765 },
  @{ name = 'beads-prod';   script = 'tools/beads-bridge.py'; config = 'private/beads-bridge-prod.json';     port = 8766 },
  @{ name = 'beads-test';    script = 'tools/beads-bridge.py'; config = 'private/beads-bridge-test.json';      port = 8767 },
  # The dev and test agents read files through one bridge; prod has its own.
  @{ name = 'files-dev-test'; script = 'tools/files-bridge.py'; config = 'private/files-bridge-dev-test.json'; port = 8773 },
  @{ name = 'files-prod';    script = 'tools/files-bridge.py'; config = 'private/files-bridge-prod.json';     port = 8774 },
  @{ name = 'guest-relay';   script = 'tools/guest-relay.py';  config = 'private/relay.json';                  port = $null }
)

function Get-BridgeProcess($b) {
  $pattern = [regex]::Escape($b.script.Replace('/', '\')) + '|' + [regex]::Escape($b.script)
  Get-CimInstance Win32_Process -Filter "Name like 'python%'" |
    Where-Object { $_.CommandLine -match $pattern -and $_.CommandLine -match [regex]::Escape($b.config) }
}

function Test-Bridge($b) {
  if ($b.port) { return [bool](Get-NetTCPConnection -LocalPort $b.port -State Listen -ErrorAction SilentlyContinue) }
  return [bool](Get-BridgeProcess $b)
}

$python = (Get-Command python -ErrorAction Stop).Source
foreach ($b in $bridges) {
  switch ($Action) {
    'status' { '{0,-20} {1}' -f $b.name, $(if (Test-Bridge $b) { 'up' } else { 'down' }) }
    'stop' {
      $p = Get-BridgeProcess $b
      if ($p) { $p | ForEach-Object { Stop-Process -Id $_.ProcessId -Force }; "$($b.name): stopped" } else { "$($b.name): not running" }
    }
    default {
      if (Test-Bridge $b) { "$($b.name): already up"; continue }
      if (-not (Test-Path $b.config)) { "$($b.name): no config at $($b.config), skipped"; continue }
      Start-Process -FilePath $python -ArgumentList $b.script, $b.config -WindowStyle Hidden `
        -RedirectStandardOutput "private/$($b.name).out.log" -RedirectStandardError "private/$($b.name).err.log" | Out-Null
      "$($b.name): started"
    }
  }
}
if ($Action -eq 'start') {
  Start-Sleep -Seconds 3
  foreach ($b in $bridges) { if (-not (Test-Bridge $b)) { "$($b.name): NOT up; see private/$($b.name).err.log" } }
}
