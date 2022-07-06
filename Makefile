
site/arlington-rec-tennis.js: src/Main.elm
	elm make src/Main.elm --optimize --output site/arlington-rec-tennis.js

deploy: site/arlington-rec-tennis.js
	./deploy.sh

serve: site/arlington-rec-tennis.js
	http-server site -p 8080 -c-1
