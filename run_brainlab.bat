@echo off
echo Loading BrainLab pipeline 
docker container stop BLAB2
docker container rm BLAB2
docker container run --name BLAB2 -it -v  .\:/shared shayofirgeva/brainlab:latest