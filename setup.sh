echo `date` : Creating bookbrainz database and user
sudo -u postgres createuser -S -D -R bookbrainz
sudo -u postgres createdb -O bookbrainz bookbrainz
cd sql
psql -U postgres < install.sql
cd ..
