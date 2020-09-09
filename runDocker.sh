
docker rmi institutions
docker build -t "institutions" .
docker run -v "$(pwd)/Out":/institutions/Out --rm institutions
