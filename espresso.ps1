$EspressoHome = "C:\graalvm\espresso-25.0.2"
$Java = "$EspressoHome\bin\java.exe"
$Aliases = "espresso:nrepl"

# Resolve classpath via clj (uses both espresso + nrepl aliases)
$cp = & clj -A:$Aliases -Spath 2>$null
if ($LASTEXITCODE -ne 0) {
    Write-Error "Failed to resolve classpath"
    exit 1
}

# Read JVM opts from the alias cache
$jvmFile = Get-ChildItem ".cpcache\*.jvm" -ErrorAction SilentlyContinue |
    Sort-Object LastWriteTime -Descending |
    Select-Object -First 1

$jvmOpts = @("-XX:+IgnoreUnrecognizedVMOptions")
if ($jvmFile) {
    $jvmOpts += (Get-Content $jvmFile.FullName | Where-Object { $_ -ne "" })
}

$javaArgs = $jvmOpts + @("-cp", $cp, "clojure.main", "-m", "nrepl.cmdline", "--port", "7890")

Write-Host "Starting Espresso nREPL on port 7890..."
& $Java @javaArgs
exit $LASTEXITCODE
