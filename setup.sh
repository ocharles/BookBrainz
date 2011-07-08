echo `date` : Creating bookbrainz database and user
sudo -u postgres createuser -S -D -R bookbrainz
sudo -u postgres createdb -O bookbrainz bookbrainz
sudo -u postgres createlang -U bookbrainz plpgsql bookbrainz

echo `date` : Installing Versioning
git submodule init
git submodule update
psql -U bookbrainz bookbrainz < ./Versioning/install.versioning.sql

echo `date` : Upgrading database schema
find sql -type f -name '*.sql' | \
    sed 's/^[^(]*(//' | while read LINE
    do
        export PATCH_NAME="$( echo "$LINE" | cut -d\' -f2 )"
        echo "$LINE" | sed "s/^[^']*'[^']\\+'[[:space:]]*,[[:space:]]*//" | \
            perl -ne '
                my @w;
                if ( s/^ARRAY\s*\[// ) {
                    s/\].*//;
                    @w = /\047([^\047]+)\047/g;
                }
                push @w, $ENV{"PATCH_NAME"} if ( 0 == @w ) || ( 0 == ( @w % 2 ) );
                printf "%s %s\n", $ENV{"PATCH_NAME"}, $_ for @w;
            '
    done | tsort | tac | while read LINE
    do
        echo `date` : Applyling $LINE
        psql -U bookbrainz bookbrainz < $LINE
    done
