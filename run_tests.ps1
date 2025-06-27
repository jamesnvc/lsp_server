
$args = Get-ChildItem -Path test -Filter *.plt |
         ForEach-Object { @("-l", $_.FullName) }

$args += @("-g", "run_tests", "-t", "halt", "--quiet", "-g", "nl")

$proc = Start-Process -FilePath swipl_win/bin/swipl.exe -ArgumentList $args -NoNewWindow -Wait -PassThru

if ($proc.ExitCode -ne 0) {
   exit $proc.ExitCode
}
