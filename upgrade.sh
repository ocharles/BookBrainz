CURRENT=`git rev-parse HEAD`

echo `date` : Upgrading code
git pull -r --ff-only origin master

echo `date` : Upgrading database schema
git diff --name-only $CURRENT...HEAD -- sql | \
    sed 's/^[^(]*(//' | while read LINE
    do
        export PATCH_NAME="$( cat "$LINE" |grep register_patch | cut -d\' -f2 )"
        cat "$LINE" | grep register_patch | sed "s/^[^']*'[^']\\+'[[:space:]]*,[[:space:]]*//" | \
            perl -ne '
                my @w;
                if ( s/^ARRAY\s*\[// ) {
                    s/\].*//;
                    @w = /\047([^\047]+)\047/g;
                }
                push @w, $ENV{"PATCH_NAME"} if ( 0 == @w ) || ( 0 == ( @w % 2 ) );
                printf "%s %s\n", $_, $ENV{"PATCH_NAME"} for @w;
            '
    done | tsort | while read LINE
    do
        echo `date` : Applyling $LINE
        psql -U bookbrainz bookbrainz < sql/$LINE.sql
    done

echo `date` : Done
