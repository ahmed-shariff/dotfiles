:: youtube-dl --no-mtime --audio-quality 0 --audio-format mp3 -x %1

@echo off

if "%~1"=="" (
   echo "NOPE"
   goto end
)

:: making sure the list is removed
for /f %%i in ('python -c "print(""%~1"".split(""&list"")[0])"') do set file_name=%%i


if "%~2"=="" (
   youtube-dl --no-mtime --audio-quality 0 --audio-format mp3 -x %file_name%
) else if "%~3" == "" (
   youtube-dl --no-mtime --audio-quality 0 --audio-format mp3 -x %file_name%  --exec "move {} temp & ffmpeg -i temp -ss %~2 {} & del temp"
) else (
   youtube-dl --no-mtime --audio-quality 0 --audio-format mp3 -x %file_name%  --exec "move {} temp & ffmpeg -i temp -ss %~2 -t %~3 {} & del temp"
)

:end
