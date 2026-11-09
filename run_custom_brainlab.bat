@echo off
echo Loading BrainLab pipeline

:: Check if a container name is provided as an argument
if "%~1"=="" (
    set CONTAINER_NAME=BLAB2
) else (
    set CONTAINER_NAME=%~1
)

echo Using container name: %CONTAINER_NAME%

docker container stop %CONTAINER_NAME%
docker container rm %CONTAINER_NAME%
docker container run --name %CONTAINER_NAME% -it -v .\:/shared shayofirgeva/brainlab:latest