echo `date` : Creating bookbrainz database and user
sudo -u postgres createuser -S -D -R bookbrainz
sudo -u postgres createdb -O bookbrainz bookbrainz
sudo -u postgres createlang -U bookbrainz plpgsql bookbrainz

echo `date` : Installing Versioning
git submodule init
git submodule update
psql -U bookbrainz bookbrainz < ./Versioning/install.versioning.sql
