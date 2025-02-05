function invoke_container
% checks if there is an exisiting brainlab docker-container, stop it, delete it and revoke
% it:
    system('docker container stop BLAB')
    system('docker container rm BLAB')
    system ('docker container run --name BLAB -dt -v  .\:/shared shayofirgeva/brainlab:latest');
        
end