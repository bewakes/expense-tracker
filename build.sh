# Modified from: https://medium.com/permutive/optimized-docker-builds-for-haskell-76a9808eb10b

docker pull bewakes/expense-dependencies:latest || true

docker build --target dependencies --cache-from bewakes/expense-dependencies:latest -t bewakes/expense-dependencies .

docker build --target app --cache-from bewakes/expense-dependencies:latest -t bewakes/expense-tracker .

docker push bewakes/expense-dependencies:latest
docker push bewakes/expense-tracker:latest
echo "Pruning docker unused images.."
yes | docker system prune
echo "DONE!!"
