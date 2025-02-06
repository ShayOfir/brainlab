function Rscript(x)

system (['docker exec -w /shared BLAB Rscript "',x,'"']);
end