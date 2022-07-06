
site/arlington-rec-tennis.js: src/Main.elm
	elm make src/Main.elm --optimize --output site/arlington-rec-tennis.js

deploy: site/arlington-rec-tennis.js
	./bin/deploy.sh

serve: site/arlington-rec-tennis.js
	./bin/serve.sh
