CURRENT=`git rev-parse HEAD`

echo `date` : Upgrading code
git pull -r --ff-only origin master

echo `date` : Upgrading database schema
git diff --name-only $CURRENT...HEAD -- sql | \
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

echo `date` : Done
