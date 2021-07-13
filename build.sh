docker pull bewakes/expense-dependencies:latest || true

docker build --target dependencies --cache-from bewakes/expense-dependencies:latest -t bewakes/expense-dependencies .

docker build --target app --cache-from bewakes/expense-dependencies:latest -t bewakes/expense-tracker .

docker push bewakes/expense-dependencies:latest
docker push bewakes/expense-tracker:latest
