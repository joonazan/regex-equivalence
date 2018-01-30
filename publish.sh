stack install --local-bin-path heroku/regex-equality/bin/
cd heroku/regex-equality
echo "web: bin/regex-equality-exe" > Procfile
git add .
git commit -m "asd"
git push heroku master